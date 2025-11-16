# script/22_02_qc_and_features.R
suppressPackageStartupMessages({
  library(here); library(dplyr); library(tidyr)
  library(ggplot2); library(stringr); library(readr); library(purrr)
})

IN_RDS  <- here("data_fmt","fmt_rds","panel_band_l02_enriched.rds")
OUT_DIR <- here("output","fig","qc")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)


panel <- readRDS(IN_RDS)

# ---- QC対象駅のスコープ設定 -----------------------------------------------
# 出力する駅の集合を切り替えられるようにする
# "all" | "nozomi" | "hikari_kodama" | "custom"
QC_SCOPE <- "all"
CUSTOM_STNS <- c()  # 例: c("三島","三河安城")

stn_keys_tbl <- panel %>% dplyr::distinct(station_key, nozomi, hikari, kodama)

if (QC_SCOPE == "nozomi") {
  stn_keys <- stn_keys_tbl %>% dplyr::filter(nozomi == 1) %>% dplyr::pull(station_key)
} else if (QC_SCOPE == "hikari_kodama") {
  stn_keys <- stn_keys_tbl %>% dplyr::filter(nozomi == 0) %>% dplyr::pull(station_key)
} else if (QC_SCOPE == "custom") {
  stn_keys <- CUSTOM_STNS
} else {
  stn_keys <- stn_keys_tbl$station_key
}

# 出力先をスコープ別のサブフォルダに
OUT_DIR <- file.path(OUT_DIR, QC_SCOPE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 0) 基本QC --------------------------------------------------------------
message(sprintf("rows=%d, years=%d, stations=%d",
                nrow(panel),
                n_distinct(panel$year),
                n_distinct(panel$station_key)))

stopifnot(!any(is.na(panel$mean_price)))
stopifnot(!any(is.na(panel$median_price)))

exp_years    <- sort(unique(panel$year))
exp_bands    <- c("[0,1)","[1,3)","[3,5)")
exp_stations <- sort(unique(panel$station_key))

expect_n <- length(exp_years) * length(exp_bands) * length(exp_stations)
if(nrow(panel) != expect_n){
  warning(sprintf("バランスパネルで想定(%d) ≠ 実数(%d)", expect_n, nrow(panel)))
}

# 年×駅で3バンド揃っているか
band_check <- panel %>%
  count(year, station_key, band_id) %>%
  count(year, station_key, name = "n_bands") %>%
  filter(n_bands != 3)

if(nrow(band_check) > 0){
  warning("一部の(year, station)でバンド欠損があります"); print(band_check, n=Inf)
} else {
  message("全ての(year, station)に3バンド揃っています")
}

# ---- 1) 可視化：駅ごとの距離プロファイル（年別） -------------------------
# バンドの中心距離（凡例整列用）
band_mid_km <- function(bid){
  ifelse(bid=="[0,1)", 0.5,
  ifelse(bid=="[1,3)", 2.0,
  ifelse(bid=="[3,5)", 4.0, NA_real_)))
}

plot_band_profile <- function(st, years = c(min(panel$year), median(panel$year), max(panel$year)),
                              stat = c("mean_price","median_price")){
  stat <- match.arg(stat)
  df <- panel %>%
    filter(station_key == st, year %in% years) %>%
    mutate(band_mid_km = band_mid_km(band_id))

  gg <- ggplot(df, aes(band_mid_km, .data[[stat]], color = factor(year), group = year)) +
    geom_line() + geom_point() +
    scale_x_continuous("距離バンド中心[km]", breaks = c(0.5,2,4)) +
    scale_y_continuous(paste0(stat, " (円/m^2)"), labels = scales::comma) +
    labs(title = sprintf("距離プロファイル：%s（%s）", st, stat), color = "年") +
    theme_bw(base_size = 12)
  gg
}

# 例：全/指定駅の図を保存
for (st in stn_keys) {
  g1 <- plot_band_profile(st, years = c(2009, 2017, max(panel$year)), stat="mean_price")
  ggsave(filename = file.path(OUT_DIR, sprintf("band_profile_mean_%s.png", st)),
         plot = g1, width = 6, height = 4, dpi = 150)
}

# ---- 2) 可視化：年×バンドのヒートマップ（駅固定） -------------------------
plot_station_heatmap <- function(st, stat = c("mean_price","median_price")){
  stat <- match.arg(stat)
  df <- panel %>%
    filter(station_key == st) %>%
    mutate(band_id = factor(band_id, levels = c("[0,1)","[1,3)","[3,5)")))
  gg <- ggplot(df, aes(x = year, y = band_id, fill = .data[[stat]])) +
    geom_tile() + scale_fill_viridis_c(option = "C") +
    labs(title = sprintf("年×バンドのヒートマップ：%s（%s）", st, stat),
         x = "年", y = "距離バンド") +
    theme_bw(base_size = 12)
  gg
}

for (st in stn_keys) {
  g2 <- plot_station_heatmap(st, stat="mean_price")
  ggsave(filename = file.path(OUT_DIR, sprintf("heatmap_mean_%s.png", st)),
         plot = g2, width = 6.5, height = 3.8, dpi = 150)
}

# ---- 3) 派生指標：距離減衰の強さ（内外ログ差） -----------------------------
# Δlog = log(内側[0,1)) - log(外側[3,5))
grad <- panel %>%
  mutate(band = recode(band_id, "[0,1)"="inner", "[3,5)"="outer", .default = NA_character_)) %>%
  filter(!is.na(band)) %>%
  select(year, station_key, station_jp, band, mean_log) %>%
  pivot_wider(names_from = band, values_from = mean_log) %>%
  mutate(grad_log = inner - outer) %>%
  arrange(station_key, year)

# 保存＆ざっくり確認
saveRDS(grad, here("data_fmt","fmt_rds","station_year_grad_log.rds"))
write_csv(grad, here("output","station_year_grad_log.csv"))

# 推移図（駅ごと）
plot_grad <- function(st){
  df <- grad %>% filter(station_key == st)
  ggplot(df, aes(year, grad_log)) +
    geom_line() + geom_point() +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(title = sprintf("距離減衰(Δlog)推移：%s", st),
         x = "年", y = "Δlog{[0,1) − [3,5)}") +
    theme_bw(base_size = 12)
}

for (st in stn_keys) {
  g3 <- plot_grad(st)
  ggsave(filename = file.path(OUT_DIR, sprintf("grad_log_%s.png", st)),
         plot = g3, width = 6.2, height = 3.8, dpi = 150)
}

message("QC図・派生CSVを出力：", OUT_DIR)