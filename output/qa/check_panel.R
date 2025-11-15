suppressPackageStartupMessages({library(here); library(dplyr); library(sf)})

PANEL_PATH <- here::here("data_fmt","fmt_rds","panel_band_l02.rds")
BANDS_PATH <- here::here("data_fmt","fmt_rds","station_bands.rds")

# read_l02_year() がワークスペースに無ければ読み込み
if (!exists("read_l02_year", mode="function")) {
  source(here::here("code","fn_read_l02_year.R"))
}

panel <- readRDS(PANEL_PATH)
bands <- readRDS(BANDS_PATH)

cat("== 基本チェック ==\n")
cat("行数:", nrow(panel), " 列数:", ncol(panel), "\n")
cat("年度レンジ:", paste(range(panel$year), collapse=" - "), "\n")
print(summary(panel[,c("n_obs","mean_price","median_price","mean_log")]))

cat("\n== 年度ごとの集計（グループ数/観測数/駅数） ==\n")
yr_summary <- panel %>%
  group_by(year) %>%
  summarise(groups = n(), obs_total = sum(n_obs), stations = n_distinct(station_key), .groups="drop")
print(yr_summary, n=Inf)

cat("\n== 重複キーの検出（year, station_key, band_id で重複が無いか） ==\n")
dup <- panel %>%
  count(year, station_key, band_id, band_from_km, band_to_km, nozomi, hikari, kodama, service_tier) %>%
  filter(n > 1)
if (nrow(dup) == 0) {
  cat("重複なし ✅\n")
} else {
  cat("重複あり ⚠️\n"); print(dup, n=20)
}

cat("\n== 駅のカバレッジ（bandsにある駅がpanelにも登場しているか） ==\n")
miss_st <- setdiff(unique(bands$station_key), unique(panel$station_key))
if (length(miss_st) == 0) {
  cat("全駅カバー ✅\n")
} else {
  cat("未出現の駅 ⚠️:", paste(head(miss_st, 10), collapse=", "),
      ifelse(length(miss_st) > 10, "...", ""), "\n")
}

cat("\n== 幾何平均の妥当性（mean_log と mean_price の関係ざっくり） ==\n")
chk_geo <- panel %>%
  mutate(geo_from_log = exp(mean_log),
         ratio_geo_to_arith = geo_from_log / mean_price) %>%
  summarise(p05 = quantile(ratio_geo_to_arith, 0.05, na.rm=TRUE),
            p50 = quantile(ratio_geo_to_arith, 0.50, na.rm=TRUE),
            p95 = quantile(ratio_geo_to_arith, 0.95, na.rm=TRUE))
print(chk_geo)

cat("\n== 代表駅のバンド順ソート確認（距離と価格の並び、目視用） ==\n")
top_st <- panel %>% group_by(station_key) %>% summarise(tot_obs = sum(n_obs), .groups="drop") %>%
  slice_max(tot_obs, n=3) %>% pull(station_key)
for (st in top_st) {
  cat("\n-- 駅:", st, " 年:", min(panel$year), "と", max(panel$year), " --\n")
  print(panel %>%
          filter(station_key == st, year %in% c(min(panel$year), max(panel$year))) %>%
          arrange(year, band_from_km) %>%
          select(year, band_id, n_obs, mean_price), n=Inf)
}

cat("\n== マッチ率のスポットチェック（3年だけ） ==\n")
set.seed(2025)
yrs_check <- sort(sample(unique(panel$year), size=min(3, length(unique(panel$year)))))
for (y in yrs_check) {
  l02 <- read_l02_year(y)              # 入力L02件数（年内全都道府県ぶん）
  matched <- panel %>% filter(year==y) %>% summarise(matched = sum(n_obs), .groups="drop") %>% pull(matched)
  cat(y, ": L02件数=", nrow(l02), " / 距離帯に入った件数=", matched,
      " / シェア=", round(matched / nrow(l02), 3), "\n")
}

cat("\n== 価格の外れ値ざっくり（分位点） ==\n")
print(quantile(panel$mean_price, probs = c(0, .5, .9, .99, 1), na.rm=TRUE))

yrs <- 2009:2025
get_year_prefs <- function(y){
  files <- list.files(here::here("data_raw","ksj_l02_landprice", as.character(y)),
                      pattern="\\.geojson$", full.names=FALSE)
  if (!length(files)) return(NULL)
  pref <- sub(".*_(\\d{2})\\.geojson$", "\\1", files)
  data.frame(year=y, pref=pref, stringsAsFactors = FALSE)
}

cov <- lapply(yrs, get_year_prefs) |> bind_rows()
cat("年×都道府県の在庫（件数）\n")
print(with(cov, table(year, pref)))

# 年ごとのユニーク都道府県数（= その年の“駅”が少ない理由の当たり）
cov %>% group_by(year) %>% summarise(n_pref = n_distinct(pref), .groups="drop")


BANDS_PATH <- here::here("data_fmt","fmt_rds","station_bands.rds")
bands <- readRDS(BANDS_PATH)

y <- 2020  # 任意の年
l02 <- read_l02_year(y)

# CRS合わせ
if (st_crs(bands) != st_crs(l02)) bands <- st_transform(bands, st_crs(l02))

# 各L02レコードが何本の駅帯にヒットするか
hits <- lengths(st_intersects(l02, bands))
summary(hits)
quantile(hits, c(.1,.5,.9,.99))
mean(hits)

suppressPackageStartupMessages({library(here); library(dplyr); library(sf); library(purrr)})

panel <- readRDS(here::here("data_fmt","fmt_rds","panel_band_l02.rds"))
bands <- readRDS(here::here("data_fmt","fmt_rds","station_bands.rds"))

# 駅の基準リスト（bands由来）
st_base <- bands %>% st_drop_geometry() %>%
  distinct(station_key, station_jp)

missing_by_year <- function(y){
  have <- panel %>% filter(year == y) %>% distinct(station_key)
  st_base %>% anti_join(have, by = "station_key") %>% mutate(year = y)
}

missing <- map_dfr(c(2014, 2024), missing_by_year) %>%
  select(year, station_key, station_jp) %>%
  arrange(year, station_key)

missing

suppressPackageStartupMessages({library(here); library(sf); library(dplyr)})

BANDS_PATH <- here::here("data_fmt","fmt_rds","station_bands.rds")

check_station_year <- function(y, key){
  l02 <- read_l02_year(y)
  b   <- readRDS(BANDS_PATH) %>% dplyr::filter(station_key == key)

  # いつも通り（L02のCRSに合わせる）
  b1 <- if (st_crs(b) != st_crs(l02)) st_transform(b, st_crs(l02)) else b
  j1 <- st_join(l02[,c("year","price_yen_m2")], b1, left = FALSE)

  # 念のためメートル系での交差（より頑健）
  l02m <- st_transform(l02, 6697)
  bm   <- st_transform(b,   6697)
  j2   <- st_join(l02m[,c("year","price_yen_m2")], bm, left = FALSE)

  tibble::tibble(
    year = y, station_key = key,
    hits_default = nrow(j1),
    hits_metric  = nrow(j2),
    crs_l02 = paste0("EPSG:", st_crs(l02)$epsg)
  )
}

to_check <- list(
  c(2014,"新横浜"), c(2014,"小田原"),
  c(2024,"東京"),   c(2024,"品川")
)

purrr::map_dfr(to_check, ~check_station_year(.x[1], .x[2]))