#build_admin_union.R

library(sf)
library(purrr)


#行政界データをまとめる
adm_files <- list.files(ADM_DIR, pattern="\\.geojson$", full.names=TRUE)

#list化・座標系統一
adm_list <- map(adm_files, ~ sf::st_read(.x, quiet = TRUE) |>
                  sf::st_transform(EPSG_WGS84))
#結合
adm <- do.call(rbind, adm_list)

# トポロジ整備
adm <- sf::st_make_valid(adm)

#複数県の結合を統合
adm_u <- sf::st_union(sf::st_geometry(adm))

#出力ファイル名を定義
ADM_UNION_GJ <- file.path(OUT_DIR_GJ, "adm_union_8pref.geojson")
ADM_UNION_RDS <- file.path(OUT_DIR_RDS, "adm_union_8pref.rds")

# 出力
dir.create(dirname(ADM_UNION_GJ), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(ADM_UNION_RDS), showWarnings = FALSE, recursive = TRUE)

sf::st_write(sf::st_as_sf(adm_u), ADM_UNION_GJ, delete_dsn = TRUE, quiet = TRUE)
saveRDS(adm_u, ADM_UNION_RDS)
