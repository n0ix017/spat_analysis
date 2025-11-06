#build_station.R

#path
station_gj <- "spat_analysis/data_raw/ksj_n02_railway/N02-24_Station.geojson" #鉄道データ
adm_dir    <- "spat_analysis/data_raw/ksj_n03"　#行政界データ（ディレクトリ）
out_gj     <- "spat/analysis/data_fmt/tokaidostation_withflags.geojson" #出力先