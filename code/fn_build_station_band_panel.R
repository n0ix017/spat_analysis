# code/fn_build_station_band_panel.R
# 目的：
#   ・L02（都道府県地価調査）の GeoJSON を年ごとに読み込む
#   ・駅×距離帯リング（band）と空間結合
#   ・駅×距離帯×年 単位に集計（平均・中央値など）
#   ・年ループでパネル化して返す（必要なら保存）
#
# 依存：
#   - code/01_config_paths.R       : DATA_L02_GJ_ROOT, STATIONS_OUT_RDS などの入出力パス定義
#   - code/02_config_params.R      : TARGET_PREF_CODES, EPSG_WGS84 等の分析定数
#   - code/fn_build_station_bands.R: build_station_bands()（駅×距離帯のリング生成）
#
# 期待する L02 の主な列（GeoJSON想定）：
#   - L02_005 : 年度（西暦の年）
#   - L02_006 : 価格 [円/㎡]（森林（020）は [円/10a] だが通常は都市用途を対象にするため除外が無難）
#   - L02_020 : 行政区域コード（5桁、先頭2桁が都道府県コード）
#   - geometry: ポイント
#
# 備考：
#   ・本ファイルは「関数群」を提供します。実行は scripts/ 側から行い、
#     そこでは当該関数を source() して使う方針を想定しています。

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(purrr)
  library(here)
})

# 設定の読み込み（パス／定数）
source(here("code","01_config_paths.R"))
source(here("code","02_config_params.R"))

# =============================================================================
# 1) ユーティリティ：指定年の L02 GeoJSON ファイル一覧を取得
# -----------------------------------------------------------------------------
# 引数：
#   year : 数値または文字列（例：2020）
#   root : 年別フォルダの親（01_config_paths.R の DATA_L02_GJ_ROOT を想定）
# 返り値：
#   該当年フォルダ内の *.geojson のフルパス文字列ベクトル
# =============================================================================
list_l02_files <- function(year, root = DATA_L02_GJ_ROOT) {
  year_dir <- file.path(root, as.character(year))
  if (!dir.exists(year_dir)) stop("L02年別ディレクトリが見つかりません: ", year_dir)
  files <- list.files(year_dir, pattern = "\\.geojson$", full.names = TRUE)
  if (length(files) == 0) stop("年", year, "の L02 GeoJSON が見つかりません。")
  files
}

# =============================================================================
# 2) 指定年の L02 を安全に読み込み＆前処理
# -----------------------------------------------------------------------------
# すること：
#   ・年フォルダ内の都道府県別 GeoJSON をすべて読み取り
#   ・（必要なら）対象都道府県にフィルタ
#   ・座標系を WGS84 に統一
#   ・key 列を標準化（year, pref_code, price_yen_m2, log_price）
# 引数：
#   year        : 数値 or 文字列
#   target_pref : 2桁の都道府県コード文字列ベクトル（例：c("22","23",...)）
# 返り値：
#   sf（ポイント）
# =============================================================================
read_l02_year <- function(year, target_pref = TARGET_PREF_CODES) {
  files <- list_l02_files(year)

  # 県ファイルを読み込み → 行結合（sf のまま）
  l02_list <- map(files, ~ sf::st_read(.x, quiet = TRUE))
  l02_sf   <- do.call(rbind, l02_list)

  # 座標系を WGS84 へ（可視化・共有で扱いやすく）
  l02_sf <- sf::st_transform(l02_sf, EPSG_WGS84)

  # 列名の存在チェックと最小限の正規化
  cols <- names(l02_sf)

  # 年（L02_005）がなければ year 列などにフォールバック
  year_col <- if ("L02_005" %in% cols) "L02_005" else if ("year" %in% cols) "year" else NA_character_
  if (is.na(year_col)) stop("L02 年度列（L02_005 / year）が見つかりません。")

  # 価格 [円/㎡] 列。標準名に合わせ price_yen_m2 を作る
  if ("L02_006" %in% cols) {
    price_col <- "L02_006"
  } else if ("price_yen_m2" %in% cols) {
    price_col <- "price_yen_m2"
  } else {
    stop("価格列（L02_006 / price_yen_m2）が見つかりません。")
  }

  # 行政区域コード（都道府県2桁抽出に使用）
  pref_col <- if ("L02_020" %in% cols) "L02_020" else if ("pref_code" %in% cols) "pref_code" else NA_character_
  if (is.na(pref_col)) stop("行政コード列（L02_020 / pref_code）が見つかりません。")

  # 最小限の列へスリム化
  l02_sf <- l02_sf |>
    mutate(
      year         = as.integer(.data[[year_col]]),
      pref_code_5d = as.character(.data[[pref_col]]),
      price_yen_m2 = as.numeric(.data[[price_col]])
    ) |>
    # 先頭2桁を都道府県コードとして抽出（例："22xxx" -> "22"）
    mutate(pref2 = substr(pref_code_5d, 1, 2)) |>
    select(year, pref2, price_yen_m2, geometry)

  # 対象都道府県だけにフィルタ（必要ない場合は TARGET_PREF_CODES を NULL にしておく）
  if (!is.null(target_pref) && length(target_pref) > 0) {
    l02_sf <- filter(l02_sf, pref2 %in% target_pref)
  }

  # 価格の log も用意（ゼロやNAは除外的に扱う）
  l02_sf <- l02_sf |>
    mutate(log_price = ifelse(is.finite(price_yen_m2) & price_yen_m2 > 0,
                              log(price_yen_m2), NA_real_))

  l02_sf
}

# =============================================================================
# 3) L02 と 駅×距離帯（bands）を空間結合
# -----------------------------------------------------------------------------
# すること：
#   ・ポイント（L02） ∈ ポリゴン（bands） で結合（st_within / st_intersects）
#   ・駅属性（nozomi/hikari/kodama, band_id 等）を付与
# 引数：
#   l02_sf : read_l02_year() の戻り（ポイント sf）
#   bands  : build_station_bands() の戻り（ポリゴン sf）
#   join_fun : 空間結合関数（デフォルト：st_within）
# 返り値：
#   l02_joined : L02 に駅×距離帯属性が付いた sf（必要なら geometry を後で落とす）
# =============================================================================
join_l02_to_bands <- function(l02_sf, bands, join_fun = sf::st_within) {
  # st_join は左側の geometry を維持する（ポイント密度が高いので後で geometry を落とす想定）
  l02_joined <- sf::st_join(l02_sf, bands, join = join_fun)

  # 駅に紐づかない点（距離帯外）は除外（必要に応じて方針変更可）
  l02_joined <- filter(l02_joined, !is.na(station_key))

  l02_joined
}

# =============================================================================
# 4) 駅×距離帯×年 で集計（平均・中央値など）
# -----------------------------------------------------------------------------
# 引数：
#   l02_joined : join_l02_to_bands() の戻り
#   price_var  : 集計対象の数値列名ベクトル（例：c("log_price","price_yen_m2")）
#   agg        : 集計関数名ベクトル（"mean","median","sd","p25" 等、下で対応）
#   min_pts    : 集計に必要な最小サンプル数（未満は NA）
# 返り値：
#   tibble/data.frame（geometry なし）
# 備考：
#   ・列名は {集計関数}_{変数} という形で作ります（例：mean_log_price）
# =============================================================================
summarise_bands <- function(l02_joined,
                            price_var = c("log_price","price_yen_m2"),
                            agg       = c("mean","median"),
                            min_pts   = 3L) {

  # 集計関数のディスパッチ（必要に応じて追加）
  agg_funs <- list(
    mean   = ~ mean(.x, na.rm = TRUE),
    median = ~ median(.x, na.rm = TRUE),
    sd     = ~ stats::sd(.x, na.rm = TRUE),
    p25    = ~ stats::quantile(.x, 0.25, na.rm = TRUE),
    p75    = ~ stats::quantile(.x, 0.75, na.rm = TRUE)
  )
  stopifnot(all(agg %in% names(agg_funs)))

  # 集計キー（駅・距離帯・年）
  by_keys <- c("year","station_key","station_jp","band_id",
               "band_from_km","band_to_km",
               "nozomi","hikari","kodama","service_tier")

  # まず geometry を落として軽量化
  df <- l02_joined |>
    sf::st_drop_geometry() |>
    select(all_of(by_keys), all_of(price_var))

  # グルーピング
  df_grp <- group_by(df, across(all_of(by_keys)))

  # サンプル数
  out <- summarise(df_grp, n = dplyr::n(), .groups = "drop_last")

  # 変数×集計関数の組み合わせで列を追加
  for (v in price_var) {
    for (a in agg) {
      col_nm <- paste0(a, "_", v)
      out[[col_nm]] <- df_grp |>
        summarise(val = agg_funs[[a]](.data[[v]]), .groups = "drop_last") |>
        pull(val)
    }
  }

  out <- ungroup(out)

  # サンプル数が閾値未満のグループは集計値を NA に（n は残す）
  if (is.finite(min_pts) && min_pts > 0) {
    mask <- out$n < min_pts
    value_cols <- setdiff(names(out), c(by_keys, "n"))
    out[mask, value_cols] <- NA_real_
  }

  as.data.frame(out)
}

# =============================================================================
# 5) 1年分のパネル（駅×距離帯×年の集計）を作る
# -----------------------------------------------------------------------------
# 引数：
#   year       : 数値/文字列
#   bands      : build_station_bands() の戻り（NULL の場合は STATIONS_OUT_RDS から作成）
#   price_var  : summarise_bands() に同じ
#   agg        : summarise_bands() に同じ
#   min_pts    : summarise_bands() に同じ
# 返り値：
#   data.frame（geometry なし）
# 備考：
#   ・bands が NULL の場合、この場で駅RDS→ build_station_bands() で生成します
# =============================================================================
build_panel_for_year <- function(year,
                                 bands     = NULL,
                                 price_var = c("log_price","price_yen_m2"),
                                 agg       = c("mean","median"),
                                 min_pts   = 3L) {
  # 駅×距離帯（ポリゴン）を用意
  if (is.null(bands)) {
    # 駅フラグ付き sf を読み、関数でリング生成
    if (!exists("build_station_bands")) {
      stop("build_station_bands() が見つかりません。fn_build_station_bands.R を source してください。")
    }
    bands <- build_station_bands(stn_flag = NULL, preview = FALSE)
  }

  # L02 を読み込み
  l02_sf <- read_l02_year(year)

  # 空間結合
  joined <- join_l02_to_bands(l02_sf, bands, join_fun = sf::st_within)

  # 集計して返す
  summarise_bands(joined, price_var = price_var, agg = agg, min_pts = min_pts)
}

# =============================================================================
# 6) 複数年をループしてフル・パネルを作る
# -----------------------------------------------------------------------------
# 引数：
#   years     : 例 2009:2025
#   bands     : 事前に build_station_bands() 済みの sf（NULL なら中で生成）
#   write_rds : RDS の保存パス（NULL なら保存しない）
#   write_csv : CSV の保存パス（NULL なら保存しない）
# 返り値：
#   data.frame（全年度結合）
# =============================================================================
build_station_band_panel <- function(years,
                                     bands     = NULL,
                                     write_rds = NULL,
                                     write_csv = NULL,
                                     price_var = c("log_price","price_yen_m2"),
                                     agg       = c("mean","median"),
                                     min_pts   = 3L) {
  # bands 準備（1回だけ作る）
  if (is.null(bands)) {
    if (!exists("build_station_bands")) {
      stop("build_station_bands() が見つかりません。fn_build_station_bands.R を source してください。")
    }
    bands <- build_station_bands(stn_flag = NULL, preview = FALSE)
  }

  # 年ループで結合
  panel_list <- lapply(as.list(years), function(y) {
    message(">> 年 ", y, " のパネルを作成中 …")
    build_panel_for_year(y, bands = bands,
                         price_var = price_var, agg = agg, min_pts = min_pts)
  })
  panel <- dplyr::bind_rows(panel_list)

  # 保存（必要な場合のみ）
  if (!is.null(write_rds)) {
    dir.create(dirname(write_rds), recursive = TRUE, showWarnings = FALSE)
    saveRDS(panel, write_rds)
  }
  if (!is.null(write_csv)) {
    dir.create(dirname(write_csv), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(panel, write_csv)
  }

  panel
}