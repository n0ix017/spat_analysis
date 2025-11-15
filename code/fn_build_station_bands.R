# code/fn_build_station_bands.R
library(sf)
library(dplyr)
library(here)

source(here("code","01_config_paths.R"))
source(here("code","02_config_params.R"))
source(here("code","04_util_qa.R"))

# ------------------------------------------------------------
# 駅ごとの距離帯（リング）ポリゴンを作成する関数
# 引数:
#   stn_flag   : のぞみ/ひかり/こだまの停車フラグを持つ駅点(sf)。
#                NULL の場合は readRDS(STATIONS_OUT_RDS) で読み込む
#   breaks_km  : 距離帯の境界 (km)。例: c(0, 0.5, 1, 2, 3)
#   preview    : TRUE のとき、mapview が利用可能なら簡易QA可視化を出力
#   preview_n  : プレビューに使用する駅の件数
#   preview_id : 可視化の保存ID（ファイル名に利用）
# 返り値:
#   駅 × 距離帯リング（ポリゴン）の sf
# ------------------------------------------------------------
build_station_bands <- function(stn_flag  = NULL,
                                breaks_km = BAND_BREAKS_KM,
                                preview   = qa_on(),
                                preview_n = 3L,
                                preview_id = "32_bands_sample") {

  # 1) 入力の駅点を用意（NULLなら既定RDSから読み込み）
  if (is.null(stn_flag)) stn_flag <- readRDS(STATIONS_OUT_RDS)

  # 2) バッファ計算のため、平面直交座標へ変換（メートル単位）
  stn_bufs <- stn_flag |>
    sf::st_transform(EPSG_METRIC)

  # 3) 距離帯の境界(km)を確認（昇順・重複なし）
  kms <- unique(sort(breaks_km))
  stopifnot(length(kms) >= 2L)

  # 4) 各境界の「外側半径」で連続バッファを作成（外側シェル）
  outer_list <- lapply(seq_len(length(kms) - 1L), function(i) {
    r2_m <- kms[i + 1L] * 1000  # km → m
    sf::st_buffer(stn_bufs, r2_m)
  })

  # 5) リング化（外側 − 一つ内側）して WGS84 に戻す
  ring_list <- lapply(seq_along(outer_list), function(i) {
    r_from <- kms[i]
    r_to   <- kms[i + 1L]
    outer  <- outer_list[[i]]
    inner  <- if (i == 1L) sf::st_buffer(stn_bufs, 0) else outer_list[[i - 1L]]
    ring   <- sf::st_difference(outer, inner)           # ドーナツ状の帯
    ring   <- sf::st_transform(ring, EPSG_WGS84)        # 表示・共有しやすい地理座標へ戻す
    ring$band_from_km <- r_from
    ring$band_to_km   <- r_to
    ring
  })

  # 6) 結合して、駅属性＋帯メタデータを整える
  bands <- do.call(rbind, ring_list) |>
    dplyr::mutate(band_id = paste0("[", band_from_km, ",", band_to_km, ")")) |>
    dplyr::select(
      station_jp = station, station_key, nozomi, hikari, kodama,
      service_tier, band_from_km, band_to_km, band_id, geometry
    )

  #qa処理
  if (preview && requireNamespace("mapview", quietly = TRUE)) {
    sample_keys <- utils::head(unique(bands$station_key), preview_n)
    m <- mapview::mapview(
           bands |> dplyr::filter(station_key %in% sample_keys),
           zcol = "band_id", layer.name = "bands"
         ) +
         mapview::mapview(
           stn_flag |> dplyr::filter(station_key %in% sample_keys),
           color = "black", layer.name = "stations"
         )
    qa_html(m, preview_id)
  }

  # 8) sf を返す（保存は呼び出し側で実施）
  bands
}