# script/21_01_build_full_panel.R
# 目的: 年度ごとのL02地価データを「駅×距離帯(band)」に結合し、
#       駅×距離帯×年のパネル（集計テーブル）を一括生成・保存する。

suppressPackageStartupMessages({
  library(here)     # パス解決
  library(dplyr)    # 後半で軽い確認に使用
})

# ---- 設定・関数の読み込み ----------------------------------------------------
# ・PATHや定数（EPSG, 対象都道府県など）
source(here("code","01_config_paths.R"))
source(here("code","02_config_params.R"))

# ・関数群（読み込み・距離帯生成・パネル生成）
source(here("code","fn_read_l02_year.R"))
source(here("code","fn_build_station_bands.R"))
source(here("code","fn_build_station_band_panel.R"))

# ---- 対象年の設定 ------------------------------------------------------------
# ・PANEL_YEARS が 02_config_params.R で定義されていればそれを使う
# ・無ければ 2009:2025 を既定値にする
years <- if (exists("PANEL_YEARS")) PANEL_YEARS else 2009:2025

message("=== フルパネル構築を開始: 年度 ", min(years), "–", max(years), " ===")

# ---- 駅×距離帯(bands) の作成（1回だけ） -------------------------------------
# ・駅sf（停車フラグ付き）から、BAND_BREAKS_KM に基づきリングポリゴンを作成
bands <- build_station_bands(preview = FALSE)

# ---- 出力パスの準備 ----------------------------------------------------------
out_dir  <- here("output","tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

out_rds  <- file.path(out_dir, paste0("station_band_panel_", min(years), "_", max(years), ".rds"))
out_csv  <- file.path(out_dir, paste0("station_band_panel_", min(years), "_", max(years), ".csv"))

# ---- パネルの生成 ------------------------------------------------------------
# ・各年のL02を読み込み → bandsと空間結合 → 駅×距離帯×年に集計 → 全年を結合
# ・price_var/agg/min_pts は必要に応じて調整
panel <- build_station_band_panel(
  years     = years,
  bands     = bands,
  write_rds = out_rds,
  write_csv = out_csv,
  price_var = c("log_price","price_yen_m2"),
  agg       = c("mean","median"),
  min_pts   = 3L
)

# ---- 生成結果の簡易確認（任意） ---------------------------------------------
message("=== 生成完了 ===")
message("保存先（RDS）: ", out_rds)
message("保存先（CSV）: ", out_csv)

# 行数・列数の確認
message("panel: ", nrow(panel), " 行 × ", ncol(panel), " 列")

# 先頭数行だけ表示（多すぎる場合は適宜コメントアウト）
print(utils::head(panel, n = 10))

# 年×距離帯IDごとの件数を軽くチェック（CSVにも保存したい場合は追記）
chk <- panel |>
  count(year, band_id, name = "n_groups") |>
  arrange(year, band_id)

print(utils::head(chk, n = 12))
