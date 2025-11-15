#code/fn_convert_randplace_geojson.R
#2017年以前のものがgeojson形式で配布されていなかったためshapefileを変換して形式を揃える
#2013,2014,2015年の一部は他とディレクトリ構成が違うぽい
#2009-2011のものはL02ではなくL01だったため、変換処理
library(sf)
library(stringr)
library(here)

# ---- 年フォルダ内から最も適切な入力データを探索 -------------------------------
# 優先順位は input_priority で指定（既定：c("geojson","shp","gml")）
# ・pref_code に合致するファイル名を厳密に優先
# 戻り値：list(path = <ファイルパス>, type = "geojson"|"shp"|"gml")
# 年(yyyy)と都道府県コード(pp: "13" など)で L02-yy_pp.shp を厳密探索
find_l02_shp <- function(year, pref_code, base_dir){
  yy <- sprintf("%02d", as.integer(year) %% 100)
  pp <- sprintf("%02d", as.integer(pref_code))

  root <- file.path(base_dir, "shapefile", paste0("l02_", yy))
  # 例: .../shapefile/l02_14/**/SHAPE/L02-14_14.shp を厳密一致で拾う
  cand <- list.files(
    root, recursive = TRUE, full.names = TRUE,
    pattern = paste0("^L02-", yy, "_", pp, "\\.shp$")
  )
  # よくある配下の正式パス …/SHAPE/… を優先
  cand <- cand[grepl("/SHAPE/", cand)]

  if (length(cand) == 0) stop("SHP not found: year=", year, " pref=", pp)
  if (length(cand) > 1) {
    # まれに重複ヒットした場合はいちばん階層が深いものを採用
    cand <- cand[which.max(nchar(cand))]
  }
  return(cand[[1]])
}
.l02_detect_input <- function(year_dir, pref_code = NULL,
                              input_priority = c("geojson","shp","gml"),
                              verbose = TRUE,
                              alt_dirs = NULL) {
  .l02_require("fs"); .l02_require("stringr")

  # 日本語: 優先順の文字列を正規化（未知の値が混ざっても無視）
  input_priority <- tolower(input_priority)
  input_priority <- intersect(input_priority, c("geojson","shp","gml"))
  if (length(input_priority) == 0L) input_priority <- c("geojson","shp","gml")

  # 日本語: 検索対象ディレクトリ（年フォルダ + 追加ディレクトリ）を統合
  search_dirs <- unique(c(year_dir, alt_dirs))
  search_dirs <- search_dirs[fs::dir_exists(search_dirs)]

  # 日本語: 都道府県コード（例：13）にマッチするベース名を優先して1件選ぶヘルパ
  pick_by_pref <- function(paths, pref_code) {
    if (length(paths) == 0L) return(NULL)
    if (is.null(pref_code)) return(paths[[1]])
    pc2  <- sprintf("%02d", as.integer(pref_code))          # "01".."47" の2桁
    pc1  <- as.character(as.integer(pref_code))             # "1".."47"
    base <- fs::path_file(paths)

    # 厳密: 2桁コードの直前がアンダースコア（例: "_13"）
    pat_strict <- stringr::regex(paste0("_", pc2, "([^0-9]|$)"))
    hit <- paths[stringr::str_detect(base, pat_strict)]
    if (length(hit) > 0L) return(hit[[1]])

    # フォールバック: 非ゼロ埋め（直前がアンダースコア, 例: "_1"）
    pat_loose <- stringr::regex(paste0("_", pc1, "([^0-9]|$)"))
    hit2 <- paths[stringr::str_detect(base, pat_loose)]
    if (length(hit2) > 0L) return(hit2[[1]])

    # 見つからなければ先頭を返す（後方互換）
    paths[[1]]
  }

  # 日本語: 優先順に従って各拡張子を探索（年フォルダ + 追加ディレクトリ）
  for (typ in input_priority) {
    if (typ == "geojson") {
      gj <- unlist(lapply(search_dirs, function(d)
        fs::dir_ls(d, recurse = TRUE, type = "file", glob = "*.geojson")
      ))
      gj <- gj[!is.na(gj)]
      gj_pick <- pick_by_pref(gj, pref_code)
      if (!is.null(gj_pick)) {
        .l02_msg(paste0("  - 入力検出：GeoJSON（", fs::path_file(gj_pick), "）"), verbose)
        return(list(path = gj_pick, type = "geojson"))
      }
    } else if (typ == "shp") {
      # Prefer exact L02-yy_pp.shp match to avoid picking the YEAR (e.g., '-14_') by mistake
      year_guess <- suppressWarnings(as.integer(fs::path_file(year_dir)))
      if (!is.na(year_guess)) {
        in_shp <- tryCatch(
          find_l02_shp(year = year_guess, pref_code = pref_code, base_dir = fs::path_dir(year_dir)),
          error = function(e) NULL
        )
        if (!is.null(in_shp) && fs::file_exists(in_shp)) {
          .l02_msg(paste0("  - 入力検出：SHP（", fs::path_file(in_shp), "）"), verbose)
          return(list(path = in_shp, type = "shp"))
        }
      }
      # Fallback: scan recursively and pick by refined pattern
      shp <- unlist(lapply(search_dirs, function(d)
        fs::dir_ls(d, recurse = TRUE, type = "file", glob = "*.shp")
      ))
      shp <- shp[!is.na(shp)]
      shp_pick <- pick_by_pref(shp, pref_code)
      if (!is.null(shp_pick)) {
        .l02_msg(paste0("  - 入力検出：SHP（", fs::path_file(shp_pick), "）"), verbose)
        return(list(path = shp_pick, type = "shp"))
      }
    } else if (typ == "gml") {
      gml <- unlist(lapply(search_dirs, function(d)
        fs::dir_ls(d, recurse = TRUE, type = "file", glob = "*.gml")
      ))
      gml <- gml[!is.na(gml)]
      gml_pick <- pick_by_pref(gml, pref_code)
      if (!is.null(gml_pick)) {
        .l02_msg(paste0("  - 入力検出：GML（", fs::path_file(gml_pick), "）"), verbose)
        return(list(path = gml_pick, type = "gml"))
      }
    }
  }

  stop("探索パス内に入力ファイル（GeoJSON/SHP/GML）が見つかりません。", call. = FALSE)
}

# ---- 依存の存在チェック（なければわかりやすく案内） ---------------------------
.l02_require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("パッケージ '%s' が必要です。install.packages('%s') で導入してください。", pkg, pkg), call. = FALSE)
  }
}

# ---- ログ出力ヘルパ（verbose フラグで制御） -----------------------------------
.l02_msg <- function(..., verbose = TRUE) {
  if (isTRUE(verbose)) message(paste0(...))
}

# ---- here::here のフォールバックを用いたパス解決 ------------------------------
# ---- here::here のフォールバックを用いたパス解決 ------------------------------
.l02_path <- function(...) {
  # here があれば here::here(...)、なければ file.path(...)
  if (requireNamespace("here", quietly = TRUE)) {
    return(do.call(here::here, list(...)))
  } else {
    return(do.call(file.path, as.list(c(...))))
  }
}

# ---- base_dir/shapefile/l02_yy を自動探索する補助 ------------------------------
.l02_alt_dirs_for_year <- function(base_dir, year) {
  .l02_require("fs")
  yy <- .l02_yy(year)
  cand <- fs::path(base_dir, "shapefile", paste0("l02_", yy))
  if (fs::dir_exists(cand)) return(cand)
  character(0)
}

# ---- 年→2桁コード（例：2016 → "16"） -----------------------------------------
.l02_yy <- function(year) {
  sprintf("%02d", as.integer(year) %% 100L)
}

# ---- 年フォルダ内の zip を（安全に）解凍 --------------------------------------
# ・年フォルダ直下にある zip（複数可）を探索し、まだ展開されていなければ解凍
# ・解凍先は年フォルダ直下（zip 名に応じたサブフォルダを作成）
.l02_unzip_if_needed <- function(year_dir, verbose = TRUE) {
  .l02_require("fs"); .l02_require("stringr")
  zips <- fs::dir_ls(year_dir, glob = "*.zip", recurse = FALSE)
  if (length(zips) == 0L) {
    .l02_msg("  - zip は見つかりませんでした（既に解凍済みか、最初から展開されている想定）", verbose)
    return(invisible(NULL))
  }
  for (zp in zips) {
    # 解凍先候補：zip 名（拡張子除く）と同名フォルダ
    outdir <- fs::path(year_dir, fs::path_ext_remove(fs::path_file(zp)))
    if (fs::dir_exists(outdir) && length(fs::dir_ls(outdir, all = TRUE)) > 0L) {
      .l02_msg(paste0("  - 既に解凍済みと判断：", fs::path_file(zp)), verbose)
      next
    }
    .l02_msg(paste0("  - 解凍中：", fs::path_file(zp)), verbose)
    utils::unzip(zp, exdir = outdir)
  }
  invisible(NULL)
}

# ---- sf 読み込み（型に応じて st_read） ----------------------------------------
.l02_read_sf <- function(path, verbose = TRUE) {
  .l02_require("sf")
  .l02_msg(paste0("  - 読み込み：", path), verbose)
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "shp")) {
    # SHP は日本語環境で CP932 のことが多い
    sf::st_read(path, quiet = TRUE, options = "ENCODING=CP932")
  } else {
    # GeoJSON/GML 等は UTF-8 を前提
    sf::st_read(path, quiet = TRUE, options = "ENCODING=UTF-8")
  }
}

# ---- 都道府県コードによるフィルタ（列があれば） -------------------------------
# ・列候補： "pref", "prefecture", "ken", "PREF", "KEN" 等（大文字小文字無視）
# ・値の候補：数値コード（例：13）または文字列コード（"13"）や名称（"東京都"）のケースにも粗く対応
.l02_filter_pref_if_possible <- function(sfobj, pref_code, verbose = TRUE) {
  .l02_require("stringr"); .l02_require("dplyr")
  nm <- names(sfobj)
  nm_low <- stringr::str_to_lower(nm)

  # 使えそうな列名を探索
  cand_idx <- which(nm_low %in% c("pref", "pref_code", "prefecture", "ken", "ken_code", "todofuken", "todofuken_cd", "todofuken_code"))
  if (length(cand_idx) == 0L) {
    .l02_msg("  - 都道府県列が特定できなかったため、フィルタはスキップします。", verbose)
    return(sfobj)
  }
  col <- nm[[cand_idx[[1]]]]

  # 比較用に、pref_code を数値/文字の両方で用意
  target_chr <- as.character(pref_code)
  suppressWarnings(target_int <- as.integer(pref_code))

  # 列側も可能なら文字化
  col_val <- sfobj[[col]]
  col_val_chr <- as.character(col_val)

  # 粗い一致判定：数値一致 or 文字一致（前後空白除去）
  keep <- rep(FALSE, length(col_val_chr))
  if (!any(is.na(target_int))) {
    suppressWarnings(keep <- keep | (suppressWarnings(as.integer(col_val)) == target_int))
  }
  keep <- keep | (stringr::str_trim(col_val_chr) == stringr::str_trim(target_chr))

  kept_n <- sum(keep, na.rm = TRUE)
  if (kept_n == 0L) {
    .l02_msg("  - 都道府県列は見つかったが、値が一致しなかったためフィルタを見送りました。", verbose)
    return(sfobj)
  }
  .l02_msg(paste0("  - 都道府県フィルタ適用：", kept_n, " 件を保持（列：", col, "）"), verbose)
  dplyr::filter(sfobj, keep)
}

# ---- 列の絞り込み（存在する列のみ） ------------------------------------------
.l02_select_cols_if_requested <- function(sfobj, select_cols, verbose = TRUE) {
  .l02_require("dplyr")
  if (is.null(select_cols) || length(select_cols) == 0L) return(sfobj)
  keep <- intersect(select_cols, names(sfobj))
  if (length(keep) == 0L) {
    .l02_msg("  - 指定された列名が見つからなかったため、列の絞り込みはスキップします。", verbose)
    return(sfobj)
  }
  .l02_msg(paste0("  - 列を絞り込み：", paste(keep, collapse = ", ")), verbose)
  dplyr::select(sfobj, dplyr::all_of(keep))
}

# ---- ランダムサンプリング（randplace 相当／再現性オプションあり） ------------
.l02_sample_if_requested <- function(sfobj, sample_n, seed = NULL, verbose = TRUE) {
  .l02_require("dplyr")
  if (is.null(sample_n) || is.na(sample_n)) return(sfobj)
  n <- nrow(sfobj)
  if (n == 0L) return(sfobj)
  k <- min(as.integer(sample_n), n)
  if (!is.null(seed)) set.seed(as.integer(seed))
  .l02_msg(paste0("  - ランダム抽出：", k, " / ", n, " 件"), verbose)
  idx <- sample.int(n, size = k, replace = FALSE)
  sfobj[idx, ]
}

# ---- 座標系変換（target_crs が NA ならスキップ） -----------------------------
.l02_transform_if_requested <- function(sfobj, target_crs, verbose = TRUE) {
  .l02_require("sf")
  if (is.null(target_crs) || (length(target_crs) == 1L && is.na(target_crs))) {
    .l02_msg("  - 座標系変換なし（target_crs = NA）", verbose)
    return(sfobj)
  }
  if (is.na(sf::st_crs(sfobj))) {
    .l02_msg("  - 入力にCRSがないため EPSG:4612(JGD2000) を仮定して付与します。", verbose)
    sfobj <- sf::st_set_crs(sfobj, 4612)
  }
  .l02_msg(paste0("  - 座標系変換：", as.character(target_crs)), verbose)
  sf::st_transform(sfobj, crs = target_crs)
}

# ---- 2009〜2011年の L01_* 列名を L02_* に正規化 --------------------------------
# ・古い年度（2009〜2011）は属性列プレフィックスが L01_* の場合があるため、
#   解析側の整合性確保のために L02_* へ置換する（存在しない場合のみ）。
# ・既に同名の L02_* 列が存在する場合は上書きせず、元の列名を維持する。
.l02_fix_l01_to_l02_if_needed <- function(sfobj, year, verbose = TRUE) {
  yr <- suppressWarnings(as.integer(year))
  if (is.na(yr) || yr > 2011) return(sfobj)

  nms <- names(sfobj)
  if (!any(grepl("^L01_", nms))) return(sfobj)

  new_nms <- nms
  changed <- 0L
  for (i in seq_along(nms)) {
    nm <- nms[i]
    if (grepl("^L01_", nm)) {
      cand <- sub("^L01_", "L02_", nm)
      # 既に cand が存在する場合は上書きしない（安全側）
      if (!(cand %in% nms)) {
        new_nms[i] <- cand
        changed <- changed + 1L
      }
    }
  }
  if (changed > 0L) {
    names(sfobj) <- new_nms
    .l02_msg(paste0("  - L01→L02 列名置換：", changed, " 列（年=", yr, "）"), verbose)
  } else {
    .l02_msg("  - L01 列は検出されたが、置換は不要でした（衝突回避のため）。", verbose)
  }
  sfobj
}

# ---- 単年処理のメイン関数 -----------------------------------------------------
# year        : 西暦（例：2016）
# pref_code   : 都道府県コード（例："13"）。数値/文字どちらでも可。
# base_dir    : 年フォルダの親（既定 "data_raw/ksj_l02_landprice"）
# target_crs  : 出力座標系（EPSG 番号や proj4 文字列）。NA で変換スキップ。
# select_cols : 出力に含めたい列名ベクトル（存在するものだけ採用）
# sample_n    : 出力前にランダム抽出する件数（randplace 相当）。NULL なら抽出なし。
# seed        : ランダム抽出の再現性用シード。NULL なら固定しない。
# overwrite   : 既存 GeoJSON がある場合に上書きするか（FALSE ならスキップ）。
# verbose     : 進捗メッセージを出すか。
convert_l02_geojson <- function(
  year,
  pref_code = "13",
  base_dir = "data_raw/ksj_l02_landprice",
  target_crs = 4326,
  select_cols = NULL,
  sample_n = NULL,
  seed = NULL,
  overwrite = FALSE,
  verbose = TRUE,
  input_priority = c("geojson","shp","gml")   # 日本語: 入力の優先順（既定は GeoJSON→SHP→GML）
) {
  .l02_require("fs"); .l02_require("glue")

  # 年フォルダの絶対パス
  year_dir <- .l02_path(base_dir, as.character(year))
  if (!fs::dir_exists(year_dir)) {
    stop(glue::glue("年フォルダが見つかりません：{year_dir}"), call. = FALSE)
  }

  # 出力ファイル名（年2桁 + 都道府県コード）
  yy <- .l02_yy(year)
  pref_chr <- as.character(pref_code)
  out_path <- fs::path(year_dir, glue::glue("L02-{yy}_{pref_chr}.geojson"))

  # 既存スキップ判定
  if (fs::file_exists(out_path) && !isTRUE(overwrite)) {
    .l02_msg(glue::glue("既存の出力を検出：{fs::path_file(out_path)}（overwrite=FALSE のためスキップ）"), verbose)
    return(out_path)
  }

  .l02_msg(glue::glue("[{year}] 変換開始（pref={pref_chr}）"), verbose)

  # zip があれば必要に応じて解凍
  .l02_unzip_if_needed(year_dir, verbose = verbose)

  # 入力データを探索（日本語: 年フォルダ + shapefile/l02_yy をサーチ）
  alt_dirs <- .l02_alt_dirs_for_year(base_dir, year)
  inp <- .l02_detect_input(year_dir, pref_code = pref_chr,
                           input_priority = input_priority,
                           verbose = verbose,
                           alt_dirs = alt_dirs)

  # 読み込み
  sfobj <- .l02_read_sf(inp$path, verbose = verbose)

  # 2009〜2011年の L01_* → L02_* 列名置換（必要時のみ実施）
  sfobj <- .l02_fix_l01_to_l02_if_needed(sfobj, year = year, verbose = verbose)

  # 都道府県でのフィルタ（列が見つかる場合のみ）
  sfobj <- .l02_filter_pref_if_possible(sfobj, pref_code = pref_chr, verbose = verbose)

  # 列の絞り込み（存在する列のみ）
  sfobj <- .l02_select_cols_if_requested(sfobj, select_cols = select_cols, verbose = verbose)

  # ランダム抽出（必要時）
  sfobj <- .l02_sample_if_requested(sfobj, sample_n = sample_n, seed = seed, verbose = verbose)

  # 座標系変換（必要時）
  sfobj <- .l02_transform_if_requested(sfobj, target_crs = target_crs, verbose = verbose)

  # 出力（GeoJSON）
  .l02_require("sf")
  # overwrite の場合は既存ファイルを削除してから書き出し
  if (fs::file_exists(out_path) && isTRUE(overwrite)) fs::file_delete(out_path)
  .l02_msg(glue::glue("  - 書き出し：{out_path}"), verbose)
  sf::st_write(sfobj, out_path, driver = "GeoJSON", quiet = TRUE)

  .l02_msg(glue::glue("[{year}] 完了：{fs::path_file(out_path)}"), verbose)
  return(out_path)
}

# ---- 複数年を一括処理 ---------------------------------------------------------
# 戻り値：データフレーム（year, pref_code, status, output, message）
convert_l02_geojson_bulk <- function(
  years,
  pref_code = "13",
  base_dir = "data_raw/ksj_l02_landprice",
  target_crs = 4326,
  select_cols = NULL,
  sample_n = NULL,
  seed = NULL,
  overwrite = FALSE,
  verbose = TRUE,
  input_priority = c("geojson","shp","gml")
) {
  .l02_require("purrr"); .l02_require("dplyr"); .l02_require("tibble")

  ys  <- unique(as.integer(years))
  pcs <- unique(sprintf("%02d", as.integer(pref_code)))

  res <- purrr::map_dfr(ys, function(y) {
    purrr::map_dfr(pcs, function(p) {
      tryCatch({
        out <- convert_l02_geojson(
          year = y, pref_code = p, base_dir = base_dir,
          target_crs = target_crs, select_cols = select_cols,
          sample_n = sample_n, seed = seed,
          overwrite = overwrite, verbose = verbose,
          input_priority = input_priority
        )
        tibble::tibble(year = y, pref_code = p, status = "ok", output = out, message = NA_character_)
      }, error = function(e) {
        tibble::tibble(year = y, pref_code = p, status = "error", output = NA_character_, message = conditionMessage(e))
      })
    })
  })

  if (verbose) {
    n_ok <- sum(res$status == "ok")
    n_ng <- sum(res$status == "error")
    message(glue::glue("一括処理 完了：成功 {n_ok} / 失敗 {n_ng}"))
  }
  res
}

# ---- 後方互換：randplace 関数名で呼ばれても動くラッパー -----------------------
# 旧来の「ランダム座標抽出 → GeoJSON 出力」を想定し、内部で convert_l02_geojson[_bulk]
# を呼び出して同等の成果物を作る。
convert_randplace_geojson <- function(
  years,
  pref_code = "13",
  base_dir = "data_raw/ksj_l02_landprice",
  sample_n = 100L,
  seed = 42L,
  target_crs = 4326,
  select_cols = NULL,
  overwrite = FALSE,
  verbose = TRUE
) {
  convert_l02_geojson_bulk(
    years = years,
    pref_code = pref_code,
    base_dir = base_dir,
    target_crs = target_crs,
    select_cols = select_cols,
    sample_n = sample_n,
    seed = seed,
    overwrite = overwrite,
    verbose = verbose
  )
}

# -----------------------------------------------------------------------------
# 使い方（例）：
# -----------------------------------------------------------------------------
# # 1) 2009〜2016年を一括で東京(13)のみ処理、既存はスキップ
# convert_l02_geojson_bulk(2009:2016, pref_code = "13")
#
# # 2) 列を絞って軽量化（例：IDと価格のみ）、WGS84(EPSG:4326)へ統一
# convert_l02_geojson_bulk(
#   years = 2009:2016, pref_code = "13",
#   target_crs = 4326,
#   select_cols = c("L01_001", "L01_006")
# )
#
# # 3) 各年100点にランダム抽出（旧 randplace 相当）
# convert_l02_geojson_bulk(
#   years = 2009:2016, pref_code = "13",
#   sample_n = 100, seed = 42
# )
#
# # 4) 単年だけ上書き再出力したい
# convert_l02_geojson(2016, pref_code = "13", overwrite = TRUE)
#
# # 5) 後方互換ラッパー（可能なら新関数の利用を推奨）
# convert_randplace_geojson(2009:2016, pref_code = "13", sample_n = 100, seed = 1)
# -----------------------------------------------------------------------------

# ---- 47都道府県を一括処理するラッパー -----------------------------------------
# 日本語: 年度ベクトル years と、処理対象の都道府県 pref_codes（既定は 01〜47）を受け取り、
#         各都道府県ごとに convert_l02_geojson_bulk() を呼び出して GeoJSON を再生成する。
#         2009〜2011の L01_* → L02_* 列名正規化は単年処理側で自動適用される。
# 戻り値: tibble(year, pref_code, status, output, message)
convert_l02_geojson_for_all_prefs <- function(
  years,
  pref_codes = sprintf("%02d", 1:47),   # 既定: "01","02",...,"47"
  base_dir   = "data_raw/ksj_l02_landprice",
  target_crs = 4326,
  select_cols = NULL,
  sample_n    = NULL,
  seed        = NULL,
  overwrite   = FALSE,
  verbose     = TRUE
) {
  .l02_require("dplyr"); .l02_require("tibble"); .l02_require("purrr")

  # 日本語: 引数が数値でも文字でも、最終的に2桁文字列へ正規化
  pc <- sprintf("%02d", as.integer(pref_codes))

  res <- purrr::map(pc, function(p) {
    if (isTRUE(verbose)) message(sprintf("=== 都道府県 %s の一括処理を開始 ===", p))
    convert_l02_geojson_bulk(
      years       = years,
      pref_code   = p,
      base_dir    = base_dir,
      target_crs  = target_crs,
      select_cols = select_cols,
      sample_n    = sample_n,
      seed        = seed,
      overwrite   = overwrite,
      verbose     = verbose
    ) |>
      dplyr::mutate(pref_code = p, .before = 1)
  }) |>
  dplyr::bind_rows()

  if (isTRUE(verbose)) {
    ok <- sum(res$status == "ok")
    ng <- sum(res$status == "error")
    message(sprintf("=== 全都道府県 一括処理 完了：成功 %d / 失敗 %d ===", ok, ng))
  }
  res
}
# ---- SHP が存在する都道府県コードを年フォルダから抽出 -------------------------
# 日本語: 年フォルダ内および追加ディレクトリを再帰的に探索し、拡張子 .shp のベース名から都道府県コード(2桁)を推定
.l02_list_pref_codes_from_shp <- function(year_dir, alt_dirs = NULL) {
  .l02_require("fs"); .l02_require("stringr")
  dirs <- unique(c(year_dir, alt_dirs))
  dirs <- dirs[fs::dir_exists(dirs)]
  if (length(dirs) == 0L) return(character(0))
  shp <- unlist(lapply(dirs, function(d) fs::dir_ls(d, recurse = TRUE, type = "file", glob = "*.shp")))
  if (length(shp) == 0L) return(character(0))
  base <- fs::path_file(shp)

  # まず「_(2桁)」パターン（末尾/ハイフン/ドット等の前）を取りに行く
  # 例: L02-14_13.shp / L02-09_13-g_PrefectureLandPriceResearch.shp
  m <- stringr::str_match(base, ".*[_-]([0-9]{2})(?:[^0-9]|$)")
  codes <- m[,2]
  # NA を除去し2桁文字列に正規化
  codes <- sprintf("%02d", as.integer(stats::na.omit(codes)))
  unique(codes)
}

# ---- 単年: 年フォルダ内の SHP が存在する都道府県のみ一括変換 -------------------
# 日本語: SHP が無い都道府県はスキップ。入力優先は SHP→GML→GeoJSON に切替。
convert_l02_geojson_from_shp_year <- function(
  year,
  base_dir   = "data_raw/ksj_l02_landprice",
  target_crs = 4326,
  select_cols = NULL,
  sample_n    = NULL,
  seed        = NULL,
  overwrite   = FALSE,
  verbose     = TRUE
) {
  .l02_require("fs"); .l02_require("tibble"); .l02_require("dplyr")

  year_dir <- .l02_path(base_dir, as.character(year))
  alt_dirs <- .l02_alt_dirs_for_year(base_dir, year)

  pcs <- .l02_list_pref_codes_from_shp(year_dir, alt_dirs = alt_dirs)
  if (length(pcs) == 0L) {
    if (isTRUE(verbose)) message(sprintf("[{%s}] SHP が見つからないためスキップしました。", as.character(year)))
    return(tibble::tibble(year = integer(0), pref_code = character(0), status = character(0),
                          output = character(0), message = character(0)))
  }

  convert_l02_geojson_bulk(
    years         = year,
    pref_code     = pcs,
    base_dir      = base_dir,
    target_crs    = target_crs,
    select_cols   = select_cols,
    sample_n      = sample_n,
    seed          = seed,
    overwrite     = overwrite,
    verbose       = verbose,
    input_priority = c("shp","gml","geojson")  # 日本語: SHP を最優先にする
  )
}

# ---- 複数年: 各年について「SHP のある都道府県」だけ一括変換 --------------------
convert_l02_geojson_from_shp_years <- function(
  years,
  base_dir   = "data_raw/ksj_l02_landprice",
  target_crs = 4326,
  select_cols = NULL,
  sample_n    = NULL,
  seed        = NULL,
  overwrite   = FALSE,
  verbose     = TRUE
) {
  .l02_require("purrr"); .l02_require("dplyr")
  ys <- unique(as.integer(years))
  purrr::map(ys, function(y) {
    if (isTRUE(verbose)) message(sprintf("=== 年 %d: SHP のある都道府県だけを一括変換 ===", y))
    convert_l02_geojson_from_shp_year(
      year        = y,
      base_dir    = base_dir,
      target_crs  = target_crs,
      select_cols = select_cols,
      sample_n    = sample_n,
      seed        = seed,
      overwrite   = overwrite,
      verbose     = verbose
    )
  }) %>% dplyr::bind_rows()
}