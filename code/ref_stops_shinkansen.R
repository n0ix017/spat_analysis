# ================================
# 東海道新幹線 停車駅 定義（手入力・明示）
# 更新日: 2025-11-06
# 方針:
#  - ALL_TOKAIDO: 東海道新幹線の全駅（17駅）
#  - NOZOMI: のぞみ停車駅（代表6駅）
#  - HIKARI: ひかり停車駅（代表パターン：のぞみ + 追加8駅）
#  - KODAMA: こだま停車駅（= 全駅）
#  - 変更時は下の「整合チェック」でエラーが出るように。
# ================================

LINE_NAME <- "東海道新幹線"

ALL_TOKAIDO <- c(
  "東京", "品川", "新横浜", "小田原", "熱海", "三島", "新富士", "静岡",
  "掛川", "浜松", "豊橋", "三河安城", "名古屋", "岐阜羽島", "米原", "京都", "新大阪"
)

NOZOMI <- c("東京", "品川", "新横浜", "名古屋", "京都", "新大阪")

HIKARI <- union(
  NOZOMI, c("小田原", "熱海", "三島", "静岡", "浜松", "豊橋", "岐阜羽島", "米原")
)

KODAMA <- ALL_TOKAIDO

#整合チェック
stopifnot(length(ALL_TOKAIDO) == length(unique(ALL_TOKAIDO)))
if (length(setdiff(NOZOMI, HIKARI))) stop("NOZOMI⊄HIKARI: ", paste(setdiff(NOZOMI, HIKARI), collapse=", "))
if (length(setdiff(HIKARI, KODAMA))) stop("HIKARI⊄KODAMA: ", paste(setdiff(HIKARI, KODAMA), collapse=", "))
if (length(setdiff(ALL_TOKAIDO, KODAMA))) stop("ALL⊄KODAMA: ", paste(setdiff(ALL_TOKAIDO, KODAMA), collapse=", "))

# ---- 出力用テーブル（このオブジェクトを他スクリプトで使用）----
library(dplyr)
stops_tokaido <- tibble(
  station_jp = ALL_TOKAIDO
) |>
  mutate(
    nozomi = as.integer(station_jp %in% NOZOMI),
    hikari = as.integer(station_jp %in% HIKARI),
    kodama = as.integer(station_jp %in% KODAMA),
    stop_class = case_when(
      nozomi==1 ~ "nozomi",
      nozomi==0 & hikari==1 ~ "hikari_only",
      nozomi==0 & hikari==0 & kodama==1 ~ "kodama_only",
      TRUE ~ "unknown"
    )
  )