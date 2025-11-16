

# 22_01b_build_convariates.R
# 共変量（≤ L02_031 の情報に限定）をポイント→駅×距離帯へ集計し、
# panel_band_l02_enriched.rds に左結合して保存します。

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(fs)
  library(yaml)
  library(stringr)
  library(purrr)
  library(tidyr)
})

logf <- function(...) cat(sprintf(...), "\n")

# --- 入力ファイルの場所 ------------------------------------------------------
PANEL_FILE <- here::here("data_fmt","fmt_rds","panel_band_l02_enriched.rds")
# バンド割当済みのポイントデータ（候補名を順に探索）
POINTS_CANDIDATES <- c(
  here::here("data_fmt","fmt_rds","l02_points_with_bands.rds"),
  here::here("data_fmt","fmt_rds","points_l02_with_bands.rds"),
  here::here("data_fmt","fmt_rds","l02_points_banded.rds")
)
# 列名エイリアス定義のYML（複数の候補から探索）
YML_CANDIDATES <- c(
  here::here("code","config","07_config_column.yml"),
  here::here("config","07_config_column.yml"),
  here::here("code","07_config_column.yml"),
  here::here("07_config_column.yml")
)

OUT_RDS   <- here::here("data_fmt","fmt_rds","panel_band_l02_with_covars.rds")
OUT_MISS  <- here::here("output","model","covariate_missing_report.csv")
fs::dir_create(dirname(OUT_RDS))
fs::dir_create(dirname(OUT_MISS))

# --- 存在チェック ------------------------------------------------------------
stopifnot(file.exists(PANEL_FILE))
# まず、環境変数 or options() の上書きを優先してポイントRDSを探す
override <- getOption("l02.points_with_bands", Sys.getenv("L02_POINTS_WITH_BANDS", NA))
cand <- POINTS_CANDIDATES[fs::file_exists(POINTS_CANDIDATES)]
points_file <- if (!is.na(override) && fs::file_exists(override)) override else cand[1]

# それでも見つからない場合、fmt_rds配下を再帰探索して要件を満たすRDSを自動検出
if (is.na(points_file)) {
  all_rds <- fs::dir_ls(here::here("data_fmt","fmt_rds"), recurse = TRUE, type = "file", glob = "*.rds")
  hit <- purrr::keep(all_rds, function(p) {
    ok <- FALSE
    try({
      x <- readRDS(p)
      nm <- names(x)
      ok <- all(c("year","station_key","band_id") %in% nm)
    }, silent = TRUE)
    ok
  })
  points_file <- hit[1]
}

if (is.na(points_file)) {
  stop(
    "バンド割当済みポイントRDSが見つかりませんでした。探索場所:\n - ",
    paste(POINTS_CANDIDATES, collapse = "\n - "),
    "\n上書き方法: options(l02.points_with_bands = \"/path/to/points.rds\") または 環境変数 L02_POINTS_WITH_BANDS を指定してください。",
    "\nまた、fmt_rds配下の自動探索でも見つかりませんでした。"
  )
}

# --- YML（あれば）を読み、エイリアス→canonical の写像を作成 --------------
yml_path <- YML_CANDIDATES[fs::file_exists(YML_CANDIDATES)][1]
alias_map <- NULL
if (!is.na(yml_path)) {
  cfg <- yaml::read_yaml(yml_path)
  # cfg は canonical/aliases/type/… を持つリスト配列の想定
  alias_map <- purrr::map(cfg, ~{
    tibble(canonical = .x$canonical, alias = unlist(.x$aliases))
  }) |> bind_rows()
}

standardize_cols <- function(df, alias_map_tbl) {
  if (is.null(alias_map_tbl)) return(df)      # YMLが無い場合は素通し
  rn <- names(df)
  # 列名に合致する alias を canonical に置換
  for (i in seq_len(nrow(alias_map_tbl))) {
    al <- alias_map_tbl$alias[i]
    if (!is.na(al) && al %in% rn) {
      colnames(df)[match(al, rn)] <- alias_map_tbl$canonical[i]
      rn <- names(df)
    }
  }
  df
}

# --- データ読み込み ----------------------------------------------------------
panel  <- readRDS(PANEL_FILE)
points <- readRDS(points_file)
points <- standardize_cols(points, alias_map)

# 必要列（canonical 名）を確認
required_cols <- c(
  "year","station_key","band_id",
  "price_yen_m2","yoy_rate_pct","site_area_m2",
  "water_flag","gas_flag","landuse_code"
)
missing <- setdiff(required_cols, names(points))
if (length(missing) > 0) {
  stop("ポイントデータに必要列が不足しています: ", paste(missing, collapse=", "))
}

logf("panel rows=%d, cols=%d", nrow(panel), ncol(panel))
logf("points rows=%d, cols=%d [%s]", nrow(points), ncol(points), basename(points_file))

# --- 林地(用途コード"020")は除外（単位が異なるため） -------------------------
points <- points |> filter(!(landuse_code %in% c("020","20")))

# --- 共変量作成（駅×年×距離帯） --------------------------------------------
covars <- points |> 
  group_by(year, station_key, band_id) |> 
  summarise(
    cov_water_share = mean(water_flag == 1, na.rm = TRUE),
    cov_gas_share   = mean(gas_flag   == 1, na.rm = TRUE),
    cov_site_m2_med = suppressWarnings(stats::median(site_area_m2, na.rm = TRUE)),
    cov_yoy_mean    = mean(yoy_rate_pct, na.rm = TRUE),
    n_points        = dplyr::n(),
    .groups = "drop"
  )

logf("covars groups=%d", nrow(covars))

# --- 結合（左結合で panel の行数を保持） -----------------------------------
out <- panel |> left_join(covars, by = c("year","station_key","band_id"))

# 結合確認
if (nrow(out) != nrow(panel)) {
  warning("結合後の行数が変化しています: before=", nrow(panel), " after=", nrow(out))
}

# --- 保存 --------------------------------------------------------------------
saveRDS(out, OUT_RDS)
logf("Saved: %s (rows=%d, cols=%d)", OUT_RDS, nrow(out), ncol(out))

# --- 欠損レポート ------------------------------------------------------------
miss_tbl <- out |>
  summarise(across(c(cov_water_share, cov_gas_share, cov_site_m2_med, cov_yoy_mean),
                   ~ sum(is.na(.)), .names = "na_{.col}")) |>
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") |>
  mutate(total_rows = nrow(out), na_rate = na_count / total_rows)

readr::write_csv(miss_tbl, OUT_MISS)
logf("Saved: %s", OUT_MISS)

# --- 画面にも簡易表示 --------------------------------------------------------
print(miss_tbl)