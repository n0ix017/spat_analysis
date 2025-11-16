# script/22_04_sensitivity.R
suppressPackageStartupMessages({
  library(here); library(dplyr); library(purrr); library(tibble)
  library(fixest); library(readr)
})

# 元データ（n_obsが必要なので enriched を読む）
panel <- readRDS(here::here("data_fmt","fmt_rds","panel_band_l02_enriched.rds"))

# DID用の絞り込み（[0,1)と[3,5)）
dat_w <- panel %>%
  filter(band_id %in% c("[0,1)","[3,5)")) %>%
  group_by(station_key) %>% mutate(treated = as.integer(any(nozomi == 1))) %>%
  ungroup() %>%
  mutate(
    near  = as.integer(band_id == "[0,1)"),
    log_p = log(median_price)
  )

stations <- unique(dat_w$station_key)

get_coef <- function(s, weighted = FALSE){
  d <- filter(dat_w, station_key != s)
  fit <- feols(
    log_p ~ near + treated:near | station_key + year,
    data = d,
    weights = if (weighted) ~ n_obs else NULL,
    cluster = ~ station_key
  )
  co <- coef(fit)
  int_name <- grep("near:treated|treated:near", names(co), value = TRUE)[1]

  tibble(
    station_excluded = s,
    near             = unname(co["near"]),
    near_treated     = unname(if (!is.na(int_name)) co[int_name] else NA_real_)
  )
}

sens   <- map_dfr(stations, ~ get_coef(.x, weighted = FALSE)) %>%
  mutate(
    near_pct         = 100 * (exp(near) - 1),
    near_treated_pct = 100 * (exp(near_treated) - 1)
  )

sens_w <- map_dfr(stations, ~ get_coef(.x, weighted = TRUE)) %>%
  mutate(
    near_pct         = 100 * (exp(near) - 1),
    near_treated_pct = 100 * (exp(near_treated) - 1)
  )

out_dir <- here::here("output","model","sensitivity")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(sens,   file.path(out_dir,"leave1out_unweighted.rds"))
saveRDS(sens_w, file.path(out_dir,"leave1out_weighted.rds"))
write_csv(sens,   file.path(out_dir,"leave1out_unweighted.csv"))
write_csv(sens_w, file.path(out_dir,"leave1out_weighted.csv"))

message("Saved sensitivity tables to: ", out_dir)
print(head(arrange(sens,   desc(near_treated_pct)), 10))
print(head(arrange(sens_w, desc(near_treated_pct)), 10))