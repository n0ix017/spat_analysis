suppressPackageStartupMessages({library(here); library(sf); library(dplyr); library(purrr)})

diag_l02_year <- function(y){
  dir <- here::here("data_raw","ksj_l02_landprice", as.character(y))
  fs  <- list.files(dir, pattern="\\.geojson$", full.names=TRUE)
  purrr::map_dfr(fs, function(f){
    x <- suppressWarnings(st_read(f, quiet=TRUE))
    tibble::tibble(
      year = y,
      file = basename(f),
      nrow = nrow(x),
      crs  = paste0("EPSG:", st_crs(x)$epsg %||% NA_integer_),
      valid_rate = mean(st_is_valid(x)),
      xmin = st_bbox(x)[1], ymin = st_bbox(x)[2],
      xmax = st_bbox(x)[3], ymax = st_bbox(x)[4]
    )
  })
}

# 2014と2024をチェック
bind_rows(diag_l02_year(2014), diag_l02_year(2024))



suppressPackageStartupMessages({library(here); library(sf); library(dplyr); library(ggplot2)})

BANDS_PATH <- here::here("data_fmt","fmt_rds","station_bands.rds")

check_station_year <- function(y, key, plot = TRUE){
  l02 <- read_l02_year(y)                         # 既存の読み込み（内部で4326へ統一済み想定）
  b   <- readRDS(BANDS_PATH) %>% filter(station_key == key)

  # 6697(メートル)で厳密に当てる
  l02m <- st_transform(l02, 6697)
  bm   <- st_transform(b,   6697)

  # 帯ごとのヒット件数
  hit_tbl <- st_join(l02m[,c("price_yen_m2")], bm, left = FALSE) %>%
    st_drop_geometry() %>%
    count(band_id, band_from_km, band_to_km, name = "n_hit") %>%
    arrange(band_from_km)

  message(sprintf("[%d][%s] hit by band:\n%s",
                  y, key, paste(capture.output(print(hit_tbl)), collapse="\n")))

  if (plot){
    # 可視化：帯ポリゴン + ヒット点（薄い）+ 全点（最薄）
    # 点が多いのでサンプルを少し落とすと軽い
    set.seed(1)
    l02m_small <- l02m %>% slice_sample(prop = min(1, 5000/nrow(l02m)))

    gg <- ggplot() +
      geom_sf(data = bm, aes(fill = band_id), color="grey30", alpha = 0.2, show.legend = FALSE) +
      geom_sf(data = l02m_small, color="grey80", size=0.2, alpha=0.4) +
      geom_sf(data = st_join(l02m_small, bm, left = FALSE), color="black", size=0.4, alpha=0.7) +
      ggtitle(sprintf("L02 vs Bands — %d / %s (sampled points)", y, key)) +
      coord_sf(datum = NA)
    print(gg)
  }
  invisible(hit_tbl)
}

# 4ケースを一括チェック
problem_cases <- list(c(2014,"新横浜"), c(2014,"小田原"),
                      c(2024,"東京"),   c(2024,"品川"))
for (pc in problem_cases) check_station_year(as.integer(pc[1]), pc[2], plot = TRUE)

pref_files <- function(y, pref2){
  dir <- here::here("data_raw","ksj_l02_landprice", as.character(y))
  list.files(dir, pattern = paste0("_", pref2, "\\.geojson$"), full.names = TRUE)
}

check_station_pref_only <- function(y, key, pref2){
  fs <- pref_files(y, pref2)
  stopifnot(length(fs) > 0)
  x <- purrr::map_dfr(fs, ~st_read(.x, quiet=TRUE))
  b <- readRDS(BANDS_PATH) %>% filter(station_key == key)

  xm <- st_transform(x, 6697)
  bm <- st_transform(b, 6697)
  n  <- nrow(st_join(xm, bm, left = FALSE))
  message(sprintf("[%d][%s] hits using ONLY pref %s: %d", y, key, pref2, n))
  invisible(n)
}

# 県ファイル限定で当ててみる
check_station_pref_only(2014, "新横浜", "14")  # 神奈川
check_station_pref_only(2014, "小田原", "14")  # 神奈川
check_station_pref_only(2024, "東京",   "13")  # 東京
check_station_pref_only(2024, "品川",   "13")  # 東京





suppressPackageStartupMessages({library(here); library(sf); library(dplyr)})

BANDS_PATH <- here::here("data_fmt","fmt_rds","station_bands.rds")

min_dist_km <- function(y, key, pref2){
  # 該当県だけ読み込み
  dir <- here::here("data_raw","ksj_l02_landprice", as.character(y))
  fs  <- list.files(dir, pattern = paste0("_", pref2, "\\.geojson$"), full.names = TRUE)
  stopifnot(length(fs) > 0)
  x   <- do.call(rbind, lapply(fs, function(f) st_read(f, quiet=TRUE)))

  # 駅の中心点（バンドの中心＝駅点）を取り出す
  b0  <- readRDS(BANDS_PATH) %>% filter(station_key == key) %>% slice(1)
  # メートル系へ
  xm  <- st_transform(x, 6697)
  p   <- st_transform(st_centroid(st_geometry(b0)), 6697) # 駅の点

  # L02各点から駅点への距離
  dkm <- as.numeric(st_distance(xm, p, by_element = FALSE)) / 1000
  dmin <- min(dkm)
  cat(sprintf("[y=%d, key=%s, pref=%s] min_dist_km=%.2f\n", y, key, pref2, dmin))
  invisible(dmin)
}

min_dist_km(2014, "新横浜", "14")
min_dist_km(2014, "小田原", "14")
min_dist_km(2024, "東京",   "13")
min_dist_km(2024, "品川",   "13")