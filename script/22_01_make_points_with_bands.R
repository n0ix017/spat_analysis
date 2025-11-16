# 22_01_make_points_with_bands.R  (short & readable)
# 目的:
#  - L02ポイント(2009-2017)を読み込み
#  - 07_config_column.yml で列名を正規化（year, price_yen_m2 等）
#  - 林地("020"/"20")を除外
#  - 最寄り新幹線駅との距離から [0,1), [1,3), [3,5) km の band_id を付与
#  - 出力: data_fmt/fmt_rds/l02_points_with_bands.rds
# 方針:
#  - 依存関数は最小限・簡潔に（過度な堅牢化はしない）
#  - 列名重複は「後勝ち」で片方を落とす
#  - EPSG:4326 のまま s2 によるメートル距離計算

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(purrr); library(stringr)
  library(here); library(fs); library(yaml)
})

logf <- function(...) cat(sprintf(...), "\n")

# ==============================
# 入力の場所（必要に応じて追加）
# ==============================
YML_CAND <- here("code","07_config_column.yml")
STATION_CAND <-  here("data_fmt","fmt_rds","tokaido_stations_flags.rds")
L02_DIRS <- c(here("data_fmt"), here("data_raw"))
OUT_RDS <- here("data_fmt","fmt_rds","l02_points_with_bands.rds")

dir_create(dirname(OUT_RDS))

# ==============================
# YAML 読み込みと簡易マッピング
# ==============================
yml_path <- YML_CAND[file_exists(YML_CAND)][1]
if (is.na(yml_path)) stop("07_config_column.yml が見つかりません。")
logf("Using column YML: %s", yml_path)

cfg <- yaml::read_yaml(yml_path)

make_alias_map <- function(cfg){
  do.call(dplyr::bind_rows, lapply(cfg, function(x){
    tibble::tibble(canonical = x$canonical, alias = unlist(x$aliases))
  }))
}
make_meta_tbl <- function(cfg){
  do.call(dplyr::bind_rows, lapply(cfg, function(x){
    na_if <- x$na_if; if (is.null(na_if)) na_if <- list()
    type  <- x$type;  if (is.null(type))  type  <- NA_character_
    tibble::tibble(canonical = x$canonical, type = type, na_if = list(na_if))
  }))
}

ALIAS_MAP <- make_alias_map(cfg)
META_TBL  <- make_meta_tbl(cfg)

# 列名を alias->canonical に貼り替え（単純）
standardize_cols <- function(df, alias_tbl){
  rn <- names(df)
  for (i in seq_len(nrow(alias_tbl))){
    al <- alias_tbl$alias[i]
    if (!is.na(al) && al %in% rn){
      colnames(df)[match(al, rn)] <- alias_tbl$canonical[i]
      rn <- names(df)
    }
  }
  # 重複があれば「後勝ち」で片側を落とす
  if (any(duplicated(names(df)))){
    df <- df[, !duplicated(names(df), fromLast = FALSE), drop = FALSE]
  }
  df
}

# NA置換と簡易型変換
apply_na_type <- function(df, meta_tbl){
  for (i in seq_len(nrow(meta_tbl))){
    cn <- meta_tbl$canonical[i]
    if (!cn %in% names(df)) next
    vals <- meta_tbl$na_if[[i]]
    if (length(vals)){
      for (v in vals) df[[cn]][df[[cn]] %in% v] <- NA
    }
    tp <- meta_tbl$type[i]
    if (!is.na(tp)){
      df[[cn]] <- switch(tp,
        integer   = suppressWarnings(as.integer(df[[cn]])),
        numeric   = suppressWarnings(as.numeric(df[[cn]])),
        character = suppressWarnings(as.character(df[[cn]])),
        df[[cn]]
      )
    }
  }
  df
}

# どの拡張子でも読み込む（sf 優先）
read_any <- function(path){
  ext <- tolower(fs::path_ext(path))
  if (ext == "rds") readRDS(path)
  else if (ext %in% c("gpkg","geojson")) suppressWarnings(sf::read_sf(path))
  else stop("未知拡張子: ", path)
}


unify_geom <- function(x){
  gnm <- tryCatch(sf::st_geometry_name(x), error = function(e) NA_character_)
  if (!is.na(gnm) && gnm != "geometry"){
    names(x)[names(x) == gnm] <- "geometry"
    sf::st_geometry(x) <- "geometry"
  }
  x
}

# 文字列カラムと列名をUTF-8に正規化（不正バイトは除去）
make_utf8_df <- function(df){
  # 列名の正規化
  names(df) <- suppressWarnings(iconv(names(df), from = "", to = "UTF-8", sub = ""))
  # 文字列カラムの正規化
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  for (cn in chr_cols) {
    df[[cn]] <- suppressWarnings(iconv(df[[cn]], from = "", to = "UTF-8", sub = ""))
  }
  df
}

# L02_XXX のうち 32 以上の列は使用しないので落とす
drop_l02_over31 <- function(df){
  rn <- names(df)
  m  <- stringr::str_match(rn, "^L02_(\\d{2,3})$")
  num <- suppressWarnings(as.integer(m[,2]))
  drop <- !is.na(num) & num > 31L
  if (any(drop)) df <- df[, !drop, drop = FALSE]
  df
}

# 年度を安全に数値化する（UTF-8化 → 4桁の西暦だけ抽出）
clean_year <- function(v, f){
  # 文字ベクトル化＆UTF-8 正規化（不正バイトは除去）
  v_chr <- as.character(v)
  v_utf <- suppressWarnings(iconv(v_chr, from = "", to = "UTF-8", sub = ""))
  # 本文から 2000 年代の4桁を抽出
  y <- suppressWarnings(as.integer(stringr::str_extract(v_utf, "(?<!\\d)(20\\d{2})(?!\\d)")))
  # まだ全部 NA の場合はファイル名から推定し、行数分リサイクル
  if (all(is.na(y))) {
    y_file <- suppressWarnings(as.integer(stringr::str_extract(basename(f), "(?<!\\d)(20\\d{2})(?!\\d)")))
    if (!is.na(y_file)) y <- rep.int(y_file, length(v_utf))
  }
  y
}

# ==============================
# L02 ポイント読込（2009–2017）
# ==============================
L02_FILES <- L02_DIRS[file_exists(L02_DIRS)] |>
  map(~dir_ls(.x, recurse = TRUE, type = "file",
              regexp = "(?i)(l02).*(rds|gpkg|geojson)$")) |>
  flatten_chr() |>
  unique() |>
  # 明示的に shapefile 配下は除外
  keep(~ !str_detect(.x, "(?i)/shapefile/")) |>
  keep(~ !str_detect(basename(.x), "(?i)panel|enriched|panel_band|band_l02|model_data"))

if (!length(L02_FILES)) stop("L02 のソースファイルが見つかりません。")

l02_list <- list()
for (f in L02_FILES){
  x <- tryCatch(read_any(f), error = function(e) NULL)
  if (is.null(x) || !inherits(x, "sf")){
    logf("Skip non-sf: %s", f); next
  }
  # 幾何列名の統一＋文字コード(UTF-8)に正規化
  x <- unify_geom(x)
  x <- make_utf8_df(x)
  x <- drop_l02_over31(x)
  # year の確保（列 or ファイル名から推定）＋頑強に数値化
  if (!"year" %in% names(x)) x <- standardize_cols(x, ALIAS_MAP)
  if (!"year" %in% names(x) && "L02_005" %in% names(x)) x$year <- x$L02_005
  if (!"year" %in% names(x)) x$year <- NA_character_

  # 文字化けや全角・単位付きでも 4桁の西暦だけ抜き出して整数化
  x$year <- clean_year(x$year, f)

  # 2009–2017 のみ残す（as.integer をここで再度呼ばない）
  x <- dplyr::filter(x, !is.na(year) & dplyr::between(year, 2009L, 2017L))
  if (!nrow(x)) next

  x <- standardize_cols(x, ALIAS_MAP) |> apply_na_type(META_TBL) |> unify_geom()

  l02_list[[length(l02_list)+1]] <- x
  logf("Keep: %s (rows=%d, cols=%d)", f, nrow(x), ncol(x))
}
if (!length(l02_list)) stop("2009-2017 の L02 ポイントが見つかりませんでした。")

l02 <- dplyr::bind_rows(l02_list)

# 必須列が無ければ NA で作って先に進む（シンプル運用）
need_vars <- c("year","price_yen_m2","yoy_rate_pct","site_area_m2",
               "water_flag","gas_flag","landuse_code")
miss <- setdiff(need_vars, names(l02))
for (v in miss) l02[[v]] <- NA
if (length(miss)) logf("WARNING: 欠けている項目: %s", paste(miss, collapse=", "))

# 林地除外
if ("landuse_code" %in% names(l02)){
  n0 <- nrow(l02)
  l02 <- dplyr::filter(l02, !(landuse_code %in% c("020","20")))
  logf("林地除外: %d -> %d rows", n0, nrow(l02))
}

# ==============================
# 駅データ読込 & 距離帯付与
# ==============================
station_file <- STATION_CAND[file_exists(STATION_CAND)][1]
if (is.na(station_file)) stop("駅データが見つかりません (stations.rds/gpkg)。")
logf("Using stations   : %s", station_file)

stn <- read_any(station_file)
if (!inherits(stn, "sf")) stop("駅データが sf ではありません。")
if (!"station_key" %in% names(stn)) stop("駅データに station_key 列が必要です。")

l02 <- sf::st_transform(l02, 4326)
stn <- sf::st_transform(stn, 4326)

stn <- unify_geom(stn)

idx <- sf::st_nearest_feature(l02, stn)
near_stn <- stn[idx, , drop = FALSE]

dist_m  <- sf::st_distance(l02, near_stn, by_element = TRUE)
dist_km <- as.numeric(dist_m) / 1000

band <- cut(dist_km,
            breaks = c(0, 1, 3, 5, Inf), right = FALSE,
            labels = c("[0,1)", "[1,3)", "[3,5)", "[5,Inf)"))

keep <- !is.na(band) & band != "[5,Inf)"
l02  <- l02[keep, ]

l02$band_id     <- droplevels(band[keep])
l02$station_key <- near_stn$station_key[keep]
l02$dist_km     <- dist_km[keep]
l02$band_id     <- factor(l02$band_id, levels = c("[0,1)","[1,3)","[3,5)"))

# ==============================
# 保存
# ==============================
saveRDS(l02, OUT_RDS)
logf("Saved: %s (rows=%d, cols=%d)", OUT_RDS, nrow(l02), ncol(l02))