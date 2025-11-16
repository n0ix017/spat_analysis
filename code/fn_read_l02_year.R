#code/fn_read_l02_year.R

library(sf)
library(dplyr)
library(here)

source(here("code","01_config_paths.R"))      # L02_DIR_FMT などのPATH
source(here("code","02_config_params.R"))     # EPSG_WGS84, TARGET_PREFS など
source(here::here("code","08_standardize_columns.R"))

# どちらか存在する用途区分列名を返すヘルパー
.pick_use_col <- function(nm) {
  cand <- c("L02_001", "L02_003", "用途区分")
  intersect(cand, nm)[1]  # 最初に見つかったもの
}

# 年ごとの県別GeoJSONを束ねて標準化して返す
read_l02_year <- function(year,
                          drop_forest = TRUE,        # 林地(020)を除外
                          keep_cols   = c("price_yen_m2","log_price","year",
                                          "pref_code","use_code","road_dist_m",
                                          "geometry")) {
  gj_dir <- here("data_fmt","l02_std", as.character(year))  # 既に整理済みの場所
  files  <- list.files(gj_dir, pattern="\\.geojson$", full.names = TRUE)
  stopifnot(length(files) > 0)
  
  l <- map(files, ~ st_read(.x, quiet = TRUE)) |> list_rbind()
  
  # 列の存在に応じて安全に拾う
  nm  <- names(l)
  use_col  <- .pick_use_col(nm)
  year_col <- dplyr::case_when("L02_005" %in% nm ~ "L02_005",
                               TRUE ~ NA_character_)
  price_col <- dplyr::case_when("L02_006" %in% nm ~ "L02_006",
                                TRUE ~ NA_character_)
  dist_col <- dplyr::case_when("L02_045" %in% nm ~ "L02_045",
                               TRUE ~ NA_character_)
  
  if (is.na(price_col)) stop("価格列 L02_006 が見つかりません")
  if (is.na(year_col))  warning("L02_005(年度)が無いため、引数yearを採用します")
  
  out <- l |>
    mutate(
      year         = if (!is.na(year_col)) as.integer(.data[[year_col]]) else as.integer(year),
      price_yen_m2 = suppressWarnings(as.numeric(.data[[price_col]])),
      use_code     = if (!is.na(use_col)) as.character(.data[[use_col]]) else NA_character_,
      pref_code    = if ("L02_020" %in% nm) as.character(.data[["L02_020"]]) else NA_character_,
      road_dist_m  = if (!is.na(dist_col)) suppressWarnings(as.integer(.data[[dist_col]])) else NA_integer_
    ) |>
    st_as_sf() |>
    st_transform(EPSG_WGS84)
  
  # 林地除外（単位違い対策）
  if (drop_forest && "use_code" %in% names(out)) {
    out <- out |> filter(use_code != "020")
  }
  
  # 基本的なクリーニング
  out <- out |>
    filter(is.finite(price_yen_m2), price_yen_m2 > 0) |>
    mutate(log_price = log(price_yen_m2)) |>
    select(any_of(keep_cols))
  
  #標準化
  out <- standardize_l02_columns(out)
  return(out)
}