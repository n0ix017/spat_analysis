# script/22_03_build_model_data.R
suppressPackageStartupMessages({
  library(here); library(dplyr)
})

# ---- 設定 ----
NEAR_BAND <- "[0,1)"   # 駅至近
FAR_BAND  <- "[3,5)"   # 基準用の遠方帯
OUTFILE   <- here::here("data_fmt","fmt_rds","model_data_bands_long.rds")

# ---- 入力 ----
# columns (panel_band_l02_enriched.rds):
# year, station_key, station_jp, band_id, band_from_km, band_to_km,
# nozomi, hikari, kodama, service_tier, n_obs, mean_price, median_price, mean_log
panel <- readRDS(here::here("data_fmt","fmt_rds","panel_band_l02_enriched.rds"))

# ---- 整形：長形式（[0,1)と[3,5)の2バンド）----
dat_long <- panel %>%
  filter(band_id %in% c(NEAR_BAND, FAR_BAND)) %>%
  group_by(station_key) %>%
  mutate(treated = as.integer(any(nozomi == 1))) %>%  # 駅レベルで不変
  ungroup() %>%
  mutate(
    near  = as.integer(band_id == NEAR_BAND),
    log_p = log(median_price)
  ) %>%
  select(year, station_key, station_jp, band_id, near, log_p, treated)

# ---- QC（必須チェック）----
# 1) 各 station-year に2バンドとも存在すること
chk_pairs <- dat_long %>% count(station_key, year) %>% filter(n != 2)
if (nrow(chk_pairs) > 0) {
  stop(sprintf("各 station-year に2バンド揃っていない行があります（例: %s）",
               paste0(chk_pairs$station_key[1], ":", chk_pairs$year[1])))
}
# 2) 価格のNAがないこと
stopifnot(!any(is.na(dat_long$log_p)))

# ---- 保存 ----
dir.create(dirname(OUTFILE), showWarnings = FALSE, recursive = TRUE)
saveRDS(dat_long, OUTFILE)
message(sprintf("Saved: %s (rows=%d, stations=%d, years=%d)",
                OUTFILE, nrow(dat_long),
                dplyr::n_distinct(dat_long$station_key),
                dplyr::n_distinct(dat_long$year)))

# ---- （任意）モデル推定：空間DID（B案）----
#  * fixest が無い環境ではスキップします
if (requireNamespace("fixest", quietly = TRUE)) {
  suppressPackageStartupMessages(library(fixest))
  dir.create(here::here("output","model"), showWarnings = FALSE, recursive = TRUE)

  # 駅FE + 年FE、駅クラスタSE
  m2 <- feols(
    log_p ~ near + treated:near | station_key + year,
    data = dat_long,
    cluster = ~ station_key
  )

  saveRDS(m2, here::here("output","model","did_spatial_m2.rds"))
  cat("\n=== DID推定結果 ===\n")
  print(etable(m2, se = "cluster"))
} else {
  warning("Package 'fixest' が見つかりませんでした。モデル推定はスキップしました。install.packages('fixest') を実行してください。")
}