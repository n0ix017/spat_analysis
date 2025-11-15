## code/05_treatment_definitions.R
suppressPackageStartupMessages({ library(dplyr) })

# 処置定義ヘルパ：のぞみ停車駅 vs ひかり/こだま駅（距離帯で絞る）
# panel: 駅×距離帯×地点×年のパネル（join後）
# band_max_km: 処置に含める最大距離帯（例: 1kmなら [0,1)）
# include_kodama: 処置側に「こだまのみ」を含めるか（TRUEなら含む）
# post_year: ポスト期の閾年（例：2024）
make_treatment_flags <- function(panel,
                                 band_max_km   = 1,
                                 include_kodama = TRUE,
                                 post_year      = 2024) {
  stopifnot(all(c("year","band_from_km","band_to_km",
                  "nozomi","hikari","kodama") %in% names(panel)))

  # 0〜band_max_km のリングだけ採用（例：0-1km）
  df <- panel %>%
    filter(band_from_km >= 0, band_to_km <= band_max_km)

  # 処置=「のぞみが停まらず、ひかりが停まる（＋必要ならこだまのみも）」
  if (include_kodama) {
    treat <- (df$nozomi == 0L) & ((df$hikari == 1L) | (df$kodama == 1L))
  } else {
    treat <- (df$nozomi == 0L) & (df$hikari == 1L)
  }

  df %>%
    mutate(
      treat     = as.integer(treat),
      post_2014 = as.integer(year >= 2014),
      post_2024 = as.integer(year >= post_year)  # 既定では 2024
    )
}