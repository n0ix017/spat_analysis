#util_strings.R

#駅名等表記揺れ対応措置
norm_ja <- function(x){
  x |>
    stringi::stri_trans_nfkc() |>
    gsub("（.*?）|\\(.*?\\)", "", ., perl = TRUE) |>
    gsub("駅", "", ., perl = TRUE) |>
    gsub("[ 　\t]", "", ., perl = TRUE) |>
    gsub("・", "", ., perl = TRUE) |>
    gsub("−|―|ー", "-", ., perl = TRUE) |>
    tolower()
}