#code/01_config_paths.R

library(here)

#path
RAW_DIR    <- here::here("data_raw")
ADM_DIR    <- here::here("data_raw", "ksj_n03_adm")　#行政界データ（ディレクトリ）
STATION_GJ <- here::here("data_raw", "ksj_n02_railway","N02-24_Station.geojson") #鉄道データ

#出力先
OUT_DIR_GJ  <- here::here("data_fmt", "fmt_gj")
OUT_DIR_RDS <- here::here("data_fmt", "fmt_rds")

# --- 入出力の規約 ---
L02_STD_DIR   <- here::here("data_fmt", "l02_std")  # 年ごとにgeojsonを格納している親
PANEL_DIR     <- here::here("data_fmt", "panel")    # 集計済みパネルの格納先
MATCH_DIR     <- here::here("data_fmt", "matched")  # マッチング結果の格納先
FIG_DIR       <- here::here("output", "fig")        # 図の出力先

#qa用のpath
OUTPUT_DIR    <- here::here("output")
QA_ROOT_DIR <- Sys.getenv("SPAT_QA_ROOT", unset = file.path(OUTPUT_DIR, "qa"))
QA_PNG_DIR  <- file.path(QA_ROOT_DIR, "png")
QA_HTML_DIR <- file.path(QA_ROOT_DIR, "html")

# 初回はディレクトリ生成
dir.create(QA_PNG_DIR,  recursive = TRUE, showWarnings = FALSE)
dir.create(QA_HTML_DIR, recursive = TRUE, showWarnings = FALSE)