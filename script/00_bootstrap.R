#script/00_bootstrap.R
suppressPackageStartupMessages({
  library(here); library(sf); library(dplyr)
})
source(here::here("code","02_config_params.R"))  # 先に定数
source(here::here("code","00_utils_geo.R"))      # 次に関数
sf::sf_use_s2(TRUE)  # デフォルトでOK（必要に応じて切替）