#code/fn_convert_randplace_geojson.R
#2017年以前のものがgeojson形式で配布されていなかったためshapefileを変換して形式を揃える
#2013・2014年, 2015年の一部は他とディレクトリ構成が違うぽい

library(sf)
library(stringr)
library(here)

convert_l02_to_geojson <- function(year_short) {
  base_dir <- here("data_raw", "ksj_l02_landprice")
  in_dir   <- here("data_raw", "ksj_l02_landprice", "shapefile", paste0("l02_", year_short))
  out_dir  <- here("data_raw", "ksj_l02_landprice", paste0("20", year_short))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # tmp_L02-XX_YY または tmp_L02-XX_YY_GML の両方に対応
  gml_dirs <- list.dirs(in_dir, recursive = FALSE, full.names = TRUE)
  gml_dirs <- gml_dirs[str_detect(basename(gml_dirs), "^tmp_L02-\\d{2}_\\d{2}(_GML)?$")]
  
  if (length(gml_dirs) == 0) {
    warning(paste("⚠️ No tmp_L02 directories found for year", year_short))
    return(NULL)
  }
  
  for (dir_path in gml_dirs) {
    dir_name <- basename(dir_path)
    matches <- str_match(dir_name, "tmp_L02-(\\d{2})_(\\d{2})(?:_GML)?")
    year <- matches[,2]
    pref_code <- matches[,3]
    
    # .shp を再帰的に探索（大文字・小文字両対応）
    shp_files <- list.files(dir_path, pattern = "\\.[sS][hH][pP]$", full.names = TRUE, recursive = TRUE)
    
    if (length(shp_files) == 0) {
      message("❌ No shapefile found in ", dir_path)
      next
    }
    
    shp_path <- shp_files[1]
    message("📂 Reading ", basename(shp_path), " ...")
    
    gdf <- tryCatch(
      st_read(shp_path, options = "ENCODING=CP932", quiet = TRUE),
      error = function(e) { message("Error reading: ", shp_path); return(NULL) }
    )
    if (is.null(gdf)) next
    
    # CRSがない場合はJGD2000と仮定
    if (is.na(st_crs(gdf))) {
      message("⚠️ No CRS detected, assigning EPSG:4612 (JGD2000)")
      gdf <- st_set_crs(gdf, 4612)
    }
    
    # JGD2011に統一
    gdf <- st_transform(gdf, 6668)
    
    out_path <- file.path(out_dir, paste0("L02-", year, "_", pref_code, ".geojson"))
    st_write(gdf, out_path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE)
    message("✅ Saved: ", out_path)
  }
  
  message("🎉 Conversion completed for year 20", year_short)
}

# 複数年（例：2009〜2017）
years <- sprintf("%02d", 9:17)
logs  <- lapply(years, convert_l02_to_geojson)
