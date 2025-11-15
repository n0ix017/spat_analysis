# script/21_01_build_full_panel.R
# 目的:
#   年度ごとの L02 地価データを「駅 × 距離帯(band)」に空間結合し、
#   駅×距離帯×年の集計パネルを作る。

build_station_band_panel <- function(
  years,
  bands,
  write_rds = NULL,
  write_csv = NULL,
  price_var = c("log_price", "price_yen_m2"),
  agg = c("mean", "median"),
  min_pts = 3L
) {
  # ---- 必要パッケージ -------------------------------------------------------
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
    library(sf)
  })

# ---- 使う集計器を“関数”で定義（文字列NG） ----
agg_funs <- list(
  median = function(x) stats::median(x, na.rm = TRUE),
  mean   = function(x) base::mean(x, na.rm = TRUE),
  p25    = function(x) stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE),
  p75    = function(x) stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE),
  n      = function(x) sum(!is.na(x))
)
# 呼び出し側が指定する agg（例: c("median","mean")）がキーに含まれているか検証
if (!all(agg %in% names(agg_funs))) {
  stop("未知の集計器が指定されています: ", paste(setdiff(agg, names(agg_funs)), collapse = ", "))
}

  # ---- 年ごと処理（pref_code のフィルタはしない：あなたの bands 側で空間的に絞れる想定） ---
  panel_list <- purrr::map(years, function(y) {
    # L02 読み込み：あなたの環境には read_l02_year(y) があり、sf を返す前提
    l02 <- read_l02_year(y) %>% ensure_year(y)

    # ---- CRS を bands に合わせて統一（st_join 前に必ず実施） ----
    bands_crs <- sf::st_crs(bands)
    if (!is.na(bands_crs)) {
      l02_crs <- sf::st_crs(l02)
      if (is.na(l02_crs)) {
        message(sprintf("⚠️ [%d] L02 の CRS が未設定のため、bands に合わせて設定します: %s", y, format(bands_crs)))
        sf::st_crs(l02) <- bands_crs
      } else if (l02_crs != bands_crs) {
        l02 <- sf::st_transform(l02, bands_crs)
      }
    }
    # 形状の不整合がある場合に備えて有効化
    l02 <- sf::st_make_valid(l02)

    # year 列が無ければ付与（簡潔な保険）
    if (!("year" %in% names(l02))) {
      l02 <- dplyr::mutate(l02, year = y)
    }

    # 空間結合（bands も sf 前提）
    joined <- sf::st_join(l02, bands, left = FALSE)

    # 駅×距離帯×年で集計
    joined %>%
      dplyr::group_by(station_id, band_id, year) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(price_var), agg_funs[agg], .names = "{.col}_{.fn}"),
        n_obs = dplyr::n(),
        .groups = "drop_last"
      ) %>%
      dplyr::filter(n_obs >= min_pts) %>%   # 観測点が少ないバンドを除外
      dplyr::ungroup()
  })

  panel <- dplyr::bind_rows(panel_list)

  # ---- 出力（必要なときだけ） ----------------------------------------------
  if (!is.null(write_rds)) saveRDS(panel, write_rds)
  if (!is.null(write_csv)) readr::write_csv(panel, write_csv)

  panel
}

# ======================================================================
# 実行ブロック
# - 最低限の依存だけ読み込み、2009–2025 を一括生成して保存。
# - pref_code や設定ファイルの自動検出は行わない。
# ======================================================================
{
  suppressPackageStartupMessages({
    library(here)
    library(readr)
  })

  # bands を既存 RDS から読む（無ければ関数で作る）
  bands_rds <- here::here("data_fmt", "fmt_rds", "station_bands.rds")
  if (file.exists(bands_rds)) {
    bands <- readRDS(bands_rds)
  } else {
    source(here::here("code", "fn_build_station_bands.R"))
    bands <- build_station_bands()  # あなたの関数前提のデフォルトで作成
    dir.create(dirname(bands_rds), recursive = TRUE, showWarnings = FALSE)
    saveRDS(bands, bands_rds)
  }
  # ---- bands 読み込み直後に標準化 ----
suppressPackageStartupMessages({ library(dplyr); library(rlang) })

# 1) 駅ID列：station_key を station_id に揃える
if (!"station_id" %in% names(bands)) {
  if ("station_key" %in% names(bands)) {
    bands <- bands %>% rename(station_id = station_key)
  } else {
    stop("bands に station_id / station_key が見つかりません。")
  }
}

# 2) band_id が無い場合は距離帯から合成（※今回は既にあるので保険）
if (!"band_id" %in% names(bands)) {
  if (all(c("band_from_km","band_to_km") %in% names(bands))) {
    bands <- bands %>%
      mutate(band_id = sprintf("[%g,%g)", band_from_km, band_to_km))
  } else {
    stop("bands に band_id も band_from_km/band_to_km もありません。")
  }
}

# 3) L02読み込み後に year が無い場合付与する小関数
ensure_year <- function(x, y) {
  if (!"year" %in% names(x)) dplyr::mutate(x, year = y) else x
}

  # L02 を読む関数の読み込み（固定パスのみ）
  if (!exists("read_l02_year", mode = "function")) {
    source(here::here("code", "fn_read_l02_year.R"))
  }

  years  <- 2009:2025
  outdir <- here::here("output", "tables")
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  out_rds <- file.path(outdir, sprintf("station_band_panel_%d_%d.rds", min(years), max(years)))
  out_csv <- file.path(outdir, sprintf("station_band_panel_%d_%d.csv", min(years), max(years)))

  message(sprintf("=== フルパネル構築を開始: 年度 %d–%d ===", min(years), max(years)))
  panel <- build_station_band_panel(
    years     = years,
    bands     = bands,
    write_rds = out_rds,
    write_csv = out_csv,
    price_var = c("log_price", "price_yen_m2"),
    agg       = c("mean", "median"),
    min_pts   = 3L
  )
  message(sprintf("✅ 完了: %s / %s", out_rds, out_csv))
}
