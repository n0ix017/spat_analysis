#script/11_01_build_station.R

#dir.create(dirname(OUT_GJ), showWarnings = FALSE, recursive = TRUE)
library(sf)
library(dplyr)
library(stringi)
library(here)

source(here("code", "01_config_paths.R"))
source(here("code", "02_config_params.R"))
source(here("code", "03_util_strings.R"))
source(here("data_ref", "stops_shinkansen.R"))

#行政界ユニオンを読み込み
adm_u <- readRDS(ADM_UNION_RDS)

#駅geojsonデータを読み込み
station_raw <- sf::st_read(STATION_GJ, quiet=TRUE) |>
  sf::st_transform(EPSG_WGS84)

#N02_003が路線名, N02_005が駅名
#必要な列のみに絞ってラベル張り替え
stn <- station_raw |>
  transmute(
    line     = N02_003,
    station  = N02_005,
    geometry = geometry
  ) |>
  mutate(
    station_key = norm_ja(station),
    line_key    = norm_ja(line)
  )
#路線名が東海道新幹線のもののみ抽出
stn_tokaido <- stn |> filter(line_key == norm_ja("東海道新幹線"))
#同名重複を取り除く
stn_tokaido <- stn_tokaido |> distinct(station_key, .keep_all = TRUE)

#念の為手入力した停車駅リストも洗っておく
NOZOMI_KEY <- norm_ja(NOZOMI)
HIKARI_KEY <- norm_ja(HIKARI)
KODAMA_KEY <- norm_ja(KODAMA)

#駅毎にひかり・のぞみの停車有無でフラグを立てる
stn_flag <- stn_tokaido |>
  mutate(
    nozomi = as.integer(station_key %in% NOZOMI_KEY),
    hikari = as.integer(station_key %in% HIKARI_KEY),
    #一応序列付与
    service_tier = case_when(
      nozomi == 1L ~ 2L,
      hikari == 1L ~ 1L,
      TRUE         ~ 0L
    )
  ) |>
  arrange(desc(nozomi), desc(hikari), station)

#出力ファイル名を定義
STATIONS_OUT_GJ <- file.path(OUT_DIR_GJ, "tokaido_stations_flags.geojson")
STATIONS_OUT_RDS <- file.path(OUT_DIR_RDS, "tokaido_stations_flags.rds")

# 出力
dir.create(dirname(STATIONS_OUT_GJ), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(STATIONS_OUT_RDS), showWarnings = FALSE, recursive = TRUE)

sf::st_write(stn_flag, STATIONS_OUT_GJ, delete_dsn = TRUE, quiet = TRUE)
saveRDS(stn_flag, STATIONS_OUT_RDS)

