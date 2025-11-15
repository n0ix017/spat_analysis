#code/34_build_station_band_panel.R

library(sf) 
library(dplyr) 
library(purrr)
library(readr)
library(here)


source(here("code","01_config_paths.R"))
source(here("code","02_config_params.R"))
source(here("code","32_build_station_bands.R"))  # build_station_bands() を使う場合

#------------------------- ユーティリティ -------------------------

list_l02_files <- function(year, root = DATA_L02_GJ_ROOT) {
  year_dir <- file.path(root, as.character(year))
  if (!dir.exists(year_dir)) stop("L02年別ディレクトリが見つかりません: ", year_dir)
  files <- list.files(year_dir, pattern = "\\.geojson$", full.names = TRUE)
  if (length(files) == 0) stop("年", year, "のL02 GeoJSONが見つかりません。")
  files
}

read_l02_year <- function(year, target_pref = TARGET_PREF_CODES) {
  files <- list_l02_files(year)
  # 必要列だけ読む（存在チェックしつつ安全に）
  pick_cols <- function(nms) {
    # 年, 価格, 用途/利用区分, 行政コード, geometry の候補
    year_col <- intersect(nms, c("L02_005","年度","year"))
    price_col <- intersect(nms, c("L02_006","価格","price"))
    use_col <- intersect(nms, c("L02_003","L02_027","用途区分","利用区分"))
    adm_col <- intersect(nms, c("L02_020","行政区域コード","adm_code"))
    need <- c(year_col[1], price_col[1], use_col[1], adm_col[1], "geometry")
    if (any(is.na(need)) || length(need) < 5)
      stop("L02列名の想定が外れています。列を確認してください: ", paste(nms, collapse=", "))
    list(year=year_col[1], price=price_col[1], use=use_col[1], adm=adm_col[1])
  }

  parts <- lapply(files, function(f){
    x <- sf::st_read(f, quiet = TRUE)
    cols <- pick_cols(names(x))
    x |>
      dplyr::transmute(
        year  = .data[[cols$year]],
        price = as.numeric(.data[[cols$price]]),
        use   = as.character(.data[[cols$use]]),
        adm   = as.character(.data[[cols$adm]]),
        geometry = geometry
      )
  })

  l02 <- do.call(sf::st_rbind, parts)

  # 年の型を正す（GeoJSONによっては文字になっている）
  l02$year <- as.integer(l02$year)

  # 基本的なフィルタ
  l02 <- l02 |>
    filter(is.finite(price), price > 0)

  # 森林(020)除外（L02_003=用途区分 or L02_027=利用区分、いずれかに"020"が入ることが多い）
  l02 <- l02 |> filter(!(use %in% c("020", 20)))

  # 対象都道府県に限定（5桁の先頭2桁が都道府県コード）
  l02 <- l02 |> filter(substr(adm, 1, 2) %in% target_pref)

  # CRS 統一（WGS84 前提）
  if (sf::st_is_longlat(l02)) {
    l02 <- sf::st_set_crs(l02, EPSG_WGS84)
  } else {
    l02 <- sf::st_transform(l02, EPSG_WGS84)
  }
  l02
}

agg_l02_to_bands <- function(l02_points, bands_sf) {
  # 点 ∈ リング
  j <- sf::st_join(l02_points, bands_sf, join = sf::st_within, left = FALSE)

  # 落ちる可能性のある列（NULL安全）
  need_cols <- c("year","station_key","station_jp","band_id",
                 "band_from_km","band_to_km","nozomi","hikari","kodama","service_tier","price")
  miss <- setdiff(need_cols, names(j))
  if (length(miss)>0) stop("結合後に不足列: ", paste(miss, collapse=", "))

  # 要約（好みに応じて調整）
  j |>
    st_drop_geometry() |>
    group_by(year, station_key, station_jp, band_id,
             band_from_km, band_to_km, nozomi, hikari, kodama, service_tier) |>
    summarise(
      n_pts        = dplyr::n(),
      price_mean   = mean(price),
      price_median = median(price),
      price_p25    = quantile(price, 0.25, names = FALSE),
      price_p75    = quantile(price, 0.75, names = FALSE),
      price_trim10 = mean(price, trim = 0.10),
      log_mean     = mean(log(price)),             # ログ平均（DID回帰などで使いやすい）
      .groups = "drop"
    ) |>
    arrange(year, station_key, band_from_km)
}

#------------------------- メイン -------------------------

build_station_band_panel <- function(years = 2009:2025,
                                     breaks_km = BAND_BREAKS_KM,
                                     save_geojson = FALSE) {

  # 駅×距離帯リング（既存を読む or 生成）
  bands <- if (file.exists(STATIONS_BANDS_RDS)) {
    readRDS(STATIONS_BANDS_RDS)
  } else {
    b <- build_station_bands(stn_flag = readRDS(STATIONS_OUT_RDS),
                             breaks_km = breaks_km)
    dir.create(dirname(STATIONS_BANDS_RDS), recursive = TRUE, showWarnings = FALSE)
    saveRDS(b, STATIONS_BANDS_RDS)
    b
  }

  results <- vector("list", length(years))

  for (i in seq_along(years)) {
    yy <- years[i]
    message(">> Year ", yy, " …  L02読み込み & 集計中")
    l02 <- read_l02_year(yy)
    ag  <- agg_l02_to_bands(l02, bands)
    results[[i]] <- ag
    rm(l02, ag); gc()
  }

  panel <- bind_rows(results)

  # 保存先
  PANEL_RDS <- here("data_fmt", "fmt_rds", "panel_station_band_2009_2025.rds")
  PANEL_CSV <- here("data_fmt", "fmt_csv", "panel_station_band_2009_2025.csv")
  dir.create(dirname(PANEL_RDS), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(PANEL_CSV), recursive = TRUE, showWarnings = FALSE)

  saveRDS(panel, PANEL_RDS)
  readr::write_csv(panel, PANEL_CSV)

  if (save_geojson) {
    # 幾何を付与（リング形状は年で不変なので単純結合）
    panel_geo <- panel |>
      left_join(
        st_drop_geometry(bands),
        by = c("station_key","band_id","band_from_km","band_to_km",
               "station_jp","nozomi","hikari","kodama","service_tier")
      ) |>
      left_join(
        bands |>
          st_as_sf() |>
          dplyr::select(station_key, band_id, geometry),
        by = c("station_key","band_id")
      ) |>
      st_as_sf()

    PANEL_GJ <- here("data_fmt","fmt_gj","panel_station_band_2009_2025.geojson")
    dir.create(dirname(PANEL_GJ), recursive = TRUE, showWarnings = FALSE)
    st_write(panel_geo, PANEL_GJ, delete_dsn = TRUE, quiet = TRUE)
  }

  message("✅ パネル完成: ", nrow(panel), " 行")
  panel
}

# 実行
# panel <- build_station_band_panel()