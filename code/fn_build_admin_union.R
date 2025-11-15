# code/fn_build_admin_union.R
# ------------------------------------------------------------------------------
# 目的:
#   ・ADM_DIR（例: data_raw/ksj_n03_adm/）配下の都道府県ごとの行政界 GeoJSON
#     をまとめて読み込み、座標系を統一し、トポロジ整備（有効化）を行った上で
#     全域をひとつのジオメトリにユニオン（st_union）します。
#   ・結果は GeoJSON（ADM_UNION_GJ）と RDS（ADM_UNION_RDS）の両方に保存します。
#   ・QAが有効（qa_on() が TRUE）なら、行政界とユニオン輪郭の確認図をPNG出力します。
#
# 依存:
#   ・code/01_config_paths.R で ADM_DIR, ADM_UNION_GJ, ADM_UNION_RDS が定義されている前提
#   ・code/02_config_params.R で EPSG_WGS84 が定義されている前提
#   ・code/04_util_qa.R に qa_on(), qa_png() がある前提（無くても実行自体は可）
#
# 使い方:
#   library(here)
#   source(here("code","01_config_paths.R"))
#   source(here("code","02_config_params.R"))
#   source(here("code","04_util_qa.R"))     # 任意（QA画像を出すなら）
#   source(here("code","fn_build_admin_union.R"))
#   build_admin_union()                      # 既定の入出力パスで実行
#
# 備考:
#   ・出力をRDSで持つ理由は「再読込の高速化」「sfをそのまま保持できる」ため。
#     大きめのポリゴンを何度も st_read するより、readRDS の方が速いケースが多いです。
#   ・GeoJSONは可視化用・他ツール連携用に出力しています。
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)  # QA図の作図に使用（qa_on() のみなら不要）
})

# 内部ユーティリティ: st_make_valid を安全に呼ぶ（lwgeom の有無も考慮）
.safe_make_valid <- function(x) {
  # sf >= 1.0 では sf::st_make_valid が同梱。lwgeom があればそちらでもOK。
  if ("lwgeom" %in% .packages(all.available = TRUE)) {
    try({
      return(lwgeom::st_make_valid(x))
    }, silent = TRUE)
  }
  # lwgeom が無い/失敗した場合は sf 側を使う
  sf::st_make_valid(x)
}

# メイン関数 -------------------------------------------------------------------
build_admin_union <- function(adm_dir  = ADM_DIR,
                              out_gj   = ADM_UNION_GJ,
                              out_rds  = ADM_UNION_RDS,
                              epsg_out = EPSG_WGS84,
                              do_qa    = if (exists("qa_on")) qa_on() else FALSE) {
  # --- 入力チェック -----------------------------------------------------------
  # adm_dir が存在するか
  if (!dir.exists(adm_dir)) {
    stop("行政界の入力ディレクトリが見つかりません: ", adm_dir)
  }

  # adm_dir 配下の GeoJSON を列挙
  files <- list.files(adm_dir, pattern = "\\.geojson$", full.names = TRUE)
  if (length(files) == 0) {
    stop("行政界GeoJSONが見つかりません（*.geojson が 0 件）: ", adm_dir)
  }

  message(sprintf("[build_admin_union] %d 件の行政界GeoJSONを読み込みます。", length(files)))

  # --- 読み込み & 座標系統一 & 型整形 ----------------------------------------
  # ・各ファイルを st_read → 指定EPSG（既定: WGS84）に変換
  # ・st_zm() でZ/M次元をドロップ（3D/M座標が混じると結合で失敗することがあるため）
  # ・st_cast("MULTIPOLYGON") で型のブレを吸収（POLYGON/MULTIPOLYGONが交ざる対策）
  adm_list <- lapply(files, function(f) {
    g <- sf::st_read(f, quiet = TRUE)
    g <- sf::st_transform(g, epsg_out)
    g <- sf::st_zm(g, drop = TRUE, what = "ZM")  # Z/Mがあれば落とす
    g <- suppressWarnings(sf::st_cast(g, "MULTIPOLYGON"))
    g
  })

  # do.call(rbind, ...) で1つの sf に結合
  adm <- do.call(rbind, adm_list)

  # --- トポロジ整備（自己交差などの修復） -----------------------------------
  # ・有効性チェック → 無効なジオメトリがあれば st_make_valid
  invalid_n <- sum(!sf::st_is_valid(adm))
  if (invalid_n > 0) {
    message(sprintf("[build_admin_union] 無効ジオメトリ %d 件 → st_make_valid を実行します。", invalid_n))
    adm <- .safe_make_valid(adm)
  }

  # --- ユニオン生成（複数県の境界を溶かす） ----------------------------------
  # ・sf::st_geometry(adm) でジオメトリ列だけを取り出し、st_union で全域を一体化
  adm_u <- sf::st_union(sf::st_geometry(adm))

  # --- 出力ディレクトリ作成 & 保存 ------------------------------------------
  # ・GeoJSON と RDS の両方を出力。既存ファイルがある場合は上書き。
  dir.create(dirname(out_gj),  recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_rds), recursive = TRUE, showWarnings = FALSE)

  sf::st_write(sf::st_as_sf(adm_u), out_gj, delete_dsn = TRUE, quiet = TRUE)
  saveRDS(adm_u, out_rds)

  message("[build_admin_union] 書き出し完了：")
  message("  - GeoJSON: ", out_gj)
  message("  - RDS    : ", out_rds)

  # --- QA出力（任意） --------------------------------------------------------
  # ・ADM（個別県の境界）と UNION（赤枠）を重ねた確認図を PNG で保存
  if (isTRUE(do_qa) && exists("qa_png")) {
    p <- ggplot() +
      geom_sf(data = adm, color = "grey70", fill = NA, linewidth = 0.3) +
      geom_sf(data = sf::st_as_sf(adm_u), color = "red", fill = NA, linewidth = 0.6) +
      labs(title = "Admin union (outline check)")
    qa_png(p, "11_admin_union_outline")  # 章番号に合わせて "11_..." と命名
  }

  # 戻り値は（使うなら）受け取れるように返しておく
  invisible(adm_u)
}