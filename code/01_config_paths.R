#code/01_config_paths.R

library(here)

#path
RAW_DIR    <- here::here("data_raw")
ADM_DIR    <- here::here("data_raw", "ksj_n03_adm")　#行政界データ（ディレクトリ）
STATION_GJ <- here::here("data_raw", "ksj_n02_railway","N02-24_Station.geojson") #鉄道データ

#出力先
OUT_DIR_GJ  <- here::here("data_fmt", "fmt_gj")
OUT_DIR_RDS <- here::here("data_fmt", "fmt_rds")

# 東海道新幹線 停車フラグ付き駅
STATIONS_OUT_GJ <- file.path(OUT_DIR_GJ,  "tokaido_stations_flags.geojson")
STATIONS_OUT_RDS <- file.path(OUT_DIR_RDS, "tokaido_stations_flags.rds")

# 行政界ユニオン（8県）
ADM_UNION_GJ <- file.path(OUT_DIR_GJ,  "adm_union_8pref.geojson")
ADM_UNION_RDS <- file.path(OUT_DIR_RDS, "adm_union_8pref.rds")

# L02（都道府県地価調査）年別GeoJSONの親ディレクトリ（関数の既定引数が参照）
DATA_L02_GJ_ROOT <- L02_STD_DIR

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