#build_station.R

#path
STATION_GJ <- "spat_analysis/data_raw/ksj_n02_railway/N02-24_Station.geojson" #鉄道データ
ADM_DIR    <- "spat_analysis/data_raw/ksj_n03"　#行政界データ（ディレクトリ）
OUT_GJ     <- "spat/analysis/data_fmt/tokaidostation_withflags.geojson" #出力先
#dir.create(dirname(OUT_GJ), showWarnings = FALSE, recursive = TRUE)

#駅名等表記揺れ対応措置
norm_ja <- function(x){
  x %>%
    stringi::stri_trans_nfkc() %>%                     # 全角半角
    str_replace_all("（.*?）|\\(.*?\\)", "") %>%       # 括弧除去
    str_replace_all("駅", "") %>%
    str_replace_all("[ 　\t]", "") %>%
    str_replace_all("・", "") %>%
    str_replace_all("−|―|ー", "-") %>%
    str_to_lower()
}

#停車駅属性を読み込み
source("code/ref_stops_shinkansen.R", encoding = "UTF-8")

