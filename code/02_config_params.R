#code/02_config_params.R

#座標
EPSG_WGS84 <- 4326
EPSG_METRIC <- 6697   # Japan plane metric（距離計測用）

# --- 年・距離帯 ---
YEARS_ANALYZE <- 2009:2025
BAND_BREAKS_KM <- c(0, 1, 3, 5)  # 近:0-1 / 中:1-3 / 遠:3-5