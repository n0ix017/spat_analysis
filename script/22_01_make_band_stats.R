# script/22_01_make_band_stats.R
suppressPackageStartupMessages({
  library(here); library(dplyr); library(purrr)
})

winsorize <- function(x, p = 0.01){
  q <- stats::quantile(x, c(p, 1-p), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

# 1) 入力
panel <- readRDS(here::here("data_fmt","fmt_rds","panel_band_l02.rds"))

# 2) ざっくりQC
qc <- list(
  n_rows      = nrow(panel),
  cols        = names(panel),
  n_na_mean   = sum(is.na(panel$mean_price)),
  n_na_median = sum(is.na(panel$median_price)),
  by_year     = panel %>% count(year),
  zero_obs    = panel %>% filter(n_obs == 0) %>% nrow()
)
print(qc$by_year, n=Inf)
message(sprintf("rows=%d, NA(mean_price)=%d, NA(median_price)=%d, zero_obs_rows=%d",
                qc$n_rows, qc$n_na_mean, qc$n_na_median, qc$zero_obs))

# 3) バンド粒度での派生列
panel_enriched <- panel %>%
  mutate(
    mean_price_w   = winsorize(mean_price, 0.01),
    mean_log_w     = log(mean_price_w),
    median_log     = log(median_price),
    median_price_w = winsorize(median_price, 0.01)
  )

saveRDS(panel_enriched, here::here("data_fmt","fmt_rds","panel_band_l02_enriched.rds"))
message("Saved: data_fmt/fmt_rds/panel_band_l02_enriched.rds")

# 4) 駅×年（3バンド統合）の代表値（n_obsで加重）
station_year_weighted <- panel_enriched %>%
  group_by(year, station_key, station_jp) %>%
  summarise(
    n_obs_total          = sum(n_obs),
    mean_price_w_wavg    = weighted.mean(mean_price_w, n_obs, na.rm=TRUE),
    median_price_w_wavg  = weighted.mean(median_price_w, n_obs, na.rm=TRUE),
    mean_log_w_wavg      = weighted.mean(mean_log_w, n_obs, na.rm=TRUE),
    median_log_wavg      = weighted.mean(median_log, n_obs, na.rm=TRUE),
    .groups = "drop"
  )

# 5) 駅×年（3バンドの単純中央値）も併せて作成（ロバスト用）
station_year_robust <- panel_enriched %>%
  group_by(year, station_key, station_jp) %>%
  summarise(
    mean_price_w_med    = median(mean_price_w,   na.rm=TRUE),
    median_price_w_med  = median(median_price_w, na.rm=TRUE),
    mean_log_w_med      = median(mean_log_w,     na.rm=TRUE),
    median_log_med      = median(median_log,     na.rm=TRUE),
    .groups = "drop"
  )

station_year <- station_year_weighted %>%
  left_join(station_year_robust, by = c("year","station_key","station_jp"))

saveRDS(station_year, here::here("data_fmt","fmt_rds","station_year_price_stats.rds"))
message("Saved: data_fmt/fmt_rds/station_year_price_stats.rds")