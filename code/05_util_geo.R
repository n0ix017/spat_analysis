#code/05_util_geo.R
to_metric <- function(x, epsg_metric = EPSG_METRIC) sf::st_transform(x, epsg_metric)
back_to   <- function(x, crs_orig)                 sf::st_transform(x, crs_orig)

with_metric <- function(x, fun, epsg_metric = EPSG_METRIC) {
  crs0 <- sf::st_crs(x); xm <- to_metric(x, epsg_metric)
  ym  <- fun(xm)
  back_to(ym, crs0)
}

with_metric2 <- function(a, b, fun, epsg_metric = EPSG_METRIC) {
  crs0 <- sf::st_crs(a); am <- to_metric(a, epsg_metric); bm <- to_metric(b, epsg_metric)
  ym <- fun(am, bm)
  back_to(ym, crs0)
}