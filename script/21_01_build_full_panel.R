# code/fn_read_l02_year.R
# 役割: 指定年の L01/L02 地価データを `sf` で読み込んで返す。
# 仕様（このプロジェクト前提のシンプル版）:
#   1) data_raw/ksj_l02_landprice/<year>/ 直下の GeoJSON（*.geojson）だけを読む
#   2) 列名の最小正規化（price_yen_m2 / year）のみ行い、それ以外は変更しない
#   3) CRS 未設定なら JGD2000(EPSG:4612) を仮設定（後段で bands に合わせて変換）
#   4) SHP/GML 等へのフォールバックは行わない（GeoJSON が無ければ明示的にエラー）

read_l02_year <- function(year,
                           base_dir = here::here("data_raw", "ksj_l02_landprice")) {
  # --- 目的 ---------------------------------------------------------------
  # ・このプロジェクトでは 2009–2017 も含め、すでに GeoJSON を事前生成済み
  # ・そのため、信頼性を担保するため「年フォルダ直下の .geojson のみ」を読む
  # ・SHP/GML 等へのフォールバックは行わない（想定外入力は明確にエラー）

  # --- 前提チェック -------------------------------------------------------
  stopifnot(length(year) == 1L, is.numeric(year))
  year_dir <- file.path(base_dir, as.character(year))

  # --- GeoJSON 列挙（年フォルダ直下のみ / 大文字・小文字は無視） ----------
  files_geojson <- if (dir.exists(year_dir)) {
    list.files(
      year_dir,
      pattern    = "\\.geojson$",
      recursive  = FALSE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    character(0)
  }

  # --- 見つからない場合は丁寧に落とす -----------------------------------
  if (!length(files_geojson)) {
    msg <- paste0(
      "read_l02_year(): GeoJSON が見つかりませんでした。\n",
      "  - 探索したディレクトリ: ", normalizePath(year_dir, mustWork = FALSE), "\n",
      "  - 対応策: 先に convert 関数で ", year, " 年の GeoJSON を作成してください。\n",
      "            例: L02-", sprintf("%02d", as.integer(year) %% 100L), "_13.geojson など"
    )
    stop(msg, call. = FALSE)
  }

  message(sprintf("  - GeoJSON %d件: %s",
                  length(files_geojson),
                  paste(basename(files_geojson), collapse = ", ")))

  # --- 読み込み -----------------------------------------------------------
  suppressPackageStartupMessages({ library(sf) })

  pieces <- lapply(files_geojson, function(fp) {
    # 静かに読む（quiet=TRUE）
    x <- tryCatch(sf::st_read(fp, quiet = TRUE), error = function(e) NULL)
    if (is.null(x)) return(NULL)

    # CRS 無し → JGD2000(EPSG:4612) を仮設定（旧データ対策）
    if (is.na(sf::st_crs(x))) {
      sf::st_crs(x) <- 4612
    }
    # 読み込み直後に 4326 へ正規化（ファイル間の CRS 差異を吸収）
    x <- sf::st_transform(x, 4326)

    # ---- 列名の最小正規化 -----------------------------------------------
    nm <- names(x)

    # 価格（円/㎡）
    if (!"price_yen_m2" %in% nm) {
      cand <- intersect(c("price_yen_m2", "L02_006", "L01_006"), nm)
      if (length(cand)) {
        x[["price_yen_m2"]] <- suppressWarnings(as.numeric(x[[cand[1]]]))
      }
    }

    # 年度（無ければ引数 year を付与）
    if (!"year" %in% nm) {
      x[["year"]] <- as.integer(year)
    }

    # ジオメトリ妥当化（壊れたポリゴン等の保険）
    if (any(sf::st_is_valid(x) == FALSE)) {
      x <- sf::st_make_valid(x)
    }

    x
  })

  pieces <- Filter(Negate(is.null), pieces)
  if (!length(pieces)) {
    stop(sprintf("read_l02_year(): 読み込みに失敗しました（year=%s）", year), call. = FALSE)
  }

  # 行方向に結合して返す
  suppressWarnings(suppressMessages(do.call(rbind, pieces)))
}

# === フルパネル（駅×距離帯×年）を一括構築する簡潔版 ===
suppressPackageStartupMessages({ library(here); library(sf); library(dplyr); library(purrr) })

# 依存関数（年別 L01/L02 を読み込む）をロード
# ※ 既に code/fn_read_l02_year.R に定義済みである前提
if (!exists("read_l02_year", mode = "function")) {
  source(here::here("code","fn_read_l02_year.R"))
}

YEARS <- 2009:2025
BANDS_PATH <- here::here("data_fmt","fmt_rds","station_bands.rds")
OUT_PATH   <- here::here("data_fmt","fmt_rds","panel_band_l02.rds")

message("=== フルパネル構築を開始: 年度 ", min(YEARS), "–", max(YEARS), " ===")

# 駅×距離帯ポリゴンの読み込み（CRS 未設定なら 4326 に仮置き）
bands <- readRDS(BANDS_PATH)
if (is.na(sf::st_crs(bands))) sf::st_crs(bands) <- 4326

# 年単位の集計関数（最小限・可読性重視）
build_one <- function(year){
  message(">> 年 ", year, " のパネルを作成中 …")

  # L01/L02 を読み込み（年フォルダ直下の GeoJSON のみ）
  l02 <- read_l02_year(year)

  # 価格の前処理（log 取り）
  l02 <- l02 |>
    dplyr::mutate(
      price_yen_m2 = suppressWarnings(as.numeric(.data[["price_yen_m2"]])),
      log_price    = log(price_yen_m2)
    ) |>
    dplyr::filter(is.finite(log_price))

  # --- CRS を合わせる（結合前に 4326 に統一） --------------------------
  # L02: NA のときは 4612 を仮置きしてから 4326 へ
  if (is.na(sf::st_crs(l02))) sf::st_crs(l02) <- 4612
  l02 <- sf::st_transform(l02, 4326)

  # bands: NA のときは 4326 を明示／異なるなら 4326 へ
  b <- bands
  if (is.na(sf::st_crs(b))) {
    sf::st_crs(b) <- 4326
  } else if (sf::st_crs(b) != sf::st_crs(l02)) {
    b <- sf::st_transform(b, 4326)
  }

  # 空間結合：地価点/面 → 距離帯（左外部でなく inner 的に）
  joined <- sf::st_join(
    l02[, c("year","price_yen_m2","log_price")],
    b,
    left = FALSE
  )

  # 幾何を落として距離帯×駅×年で集計
  out <- joined |>
    sf::st_drop_geometry() |>
    dplyr::group_by(
      year, station_key, station_jp,
      band_id, band_from_km, band_to_km,
      nozomi, hikari, kodama, service_tier
    ) |>
    dplyr::summarise(
      n_obs        = dplyr::n(),
      mean_price   = mean(price_yen_m2, na.rm = TRUE),
      median_price = stats::median(price_yen_m2, na.rm = TRUE),
      mean_log     = mean(log_price, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(station_key, band_from_km)

  invisible(out)
}

# すべての年をまとめて DataFrame に
panel <- purrr::map_dfr(YEARS, build_one)

# 保存
saveRDS(panel, OUT_PATH)
message("✅ 完了: ", OUT_PATH)