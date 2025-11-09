#build_station_buffer_1.R

library(sf)
library(here)

source(here("code","config_paths.R"))
source(here("code","config_params.R"))

#駅データ（フラグ付き）読み込み
stn_flag <- readRDS(STATIONS_OUT_RDS)
#まずは1kmで作成
buf_m <- 1000

stn_buf <- stn_flag |>
  st_transform(6697) |>     #距離計測用（日本の平面座標）
  st_buffer(buf_m) |>
  st_transform(EPSG_WGS84)

STATIONS_BUF_GJ <- here::here("data_fmt", "fmt_gj","tokaido_stations_buffer_1km.geojson")
STATIONS_BUF_RDS <- here::here("data_fmt", "fmt_rds", "tokaido_stations_buffer_1km.rds")
dir.create(dirname(STATIONS_BUF_GJ), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(STATIONS_BUF_RDS), recursive = TRUE, showWarnings = FALSE)
st_write(stn_buf, STATIONS_BUF_GJ, delete_dsn=TRUE, quiet=TRUE)
saveRDS(stn_buf, STATIONS_BUF_RDS)