#code/32_build_station_bands.R
library(sf)
library(dplyr)
library(here)

source(here("code","01_config_paths.R"))
source(here("code","02_config_params.R"))

#駅ごとに距離帯ポリゴンを作る
#BAND_BREAKS_KMは02_config_params.Rで定義済み

build_station_bands <- # return: 各駅×各バンドのポリゴン sf
  build_station_bands <- function(stn_flag = NULL, breaks_km = BAND_BREAKS_KM) {
    if (is.null(stn_flag)) stn_flag <- readRDS(STATIONS_OUT_RDS)
    
    # メートル系でバッファ → WGS84へ戻す
    stn_bufs <- stn_flag |>
      st_transform(EPSG_METRIC)
    
    # 連続バッファ（外側だけをまず作る）
    kms <- unique(sort(breaks_km))
    outer_list <- lapply(seq_len(length(kms)-1L), function(i){
      r2 <- kms[i+1] * 1000
      st_buffer(stn_bufs, r2)
    })
    
    # リング化（外側 - 内側）
    ring_list <- lapply(seq_along(outer_list), function(i){
      r_from <- kms[i]
      r_to   <- kms[i+1]
      outer  <- outer_list[[i]]
      inner  <- if (i==1) st_buffer(stn_bufs, 0) else outer_list[[i-1]]
      ring   <- st_difference(outer, inner)
      ring   <- st_transform(ring, EPSG_WGS84)
      ring$band_from_km <- r_from
      ring$band_to_km   <- r_to
      ring
    })
    
    bands <- do.call(rbind, ring_list) |>
      mutate(band_id = paste0("[", band_from_km, ",", band_to_km, ")")) |>
      # 駅属性をそのまま保持
      select(station_jp = station, station_key, nozomi, hikari, kodama,
             service_tier, band_from_km, band_to_km, band_id, geometry)
    bands
  }