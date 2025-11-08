#util_strings.R

library(stringr)
library(stringi)

#駅名等表記揺れ対応措置
norm_ja <- function(x){
  s <- stringi::stri_trans_nfkc(x)                      # 全角/半角・合成文字の正規化
  s <- stringr::str_replace_all(s, "（.*?）|\\(.*?\\)", "")  # 丸括弧内を除去
  s <- stringr::str_replace_all(s, "駅", "")            # 「駅」を除去
  s <- stringr::str_replace_all(s, "[ 　\t]", "")       # 空白を除去（全角/半角/タブ）
  s <- stringr::str_replace_all(s, "・", "")            # 中点を除去
  s <- stringr::str_replace_all(s, "[−―ー]", "-")       # 長音の表記ゆれ統一
  stringr::str_to_lower(s)                              # 小文字化
}