#build_admin_union.R

#行政界データをまとめる
adm_files <- list.files(ADM_DIR, pattern="\\.geojson$", full.names=TRUE)
adm <- purrr::map(adm_files, ~ st_read(.x, quiet=TRUE)) |> purrr::list_rbind() |> st_transform(4326)
adm_u <- st_union(st_geometry(adm))

