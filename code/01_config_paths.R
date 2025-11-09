#01_config_paths.R

library(here)

#path
RAW_DIR    <- here::here("data_raw")
ADM_DIR    <- here::here("data_raw", "ksj_n03_adm")　#行政界データ（ディレクトリ）
STATION_GJ <- here::here("data_raw", "ksj_n02_railway","N02-24_Station.geojson") #鉄道データ

#出力先
OUT_DIR_GJ  <- here::here("data_fmt", "fmt_gj")
OUT_DIR_RDS <- here::here("data_fmt", "fmt_rds")
