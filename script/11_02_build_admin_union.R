## script/11_02_build_admin_union.R
suppressPackageStartupMessages({ library(here) })
source(here("code","01_config_paths.R"))
source(here("code","02_config_params.R"))
source(here("code","04_util_qa.R"))
source(here("code","fn_build_admin_union.R"))

build_admin_union()