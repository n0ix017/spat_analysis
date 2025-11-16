suppressPackageStartupMessages({
  library(here); library(fs); library(readr); library(dplyr)
  library(stringr); library(purrr); library(tibble); library(ggplot2)
})

# 1) 年付きサマリTXTを列挙
files <- fs::dir_ls(
  here::here("output","model"),
  regexp = "did_linear_anticipation_ddd_summary_\\d{4}\\.txt$",
  type = "file"
)

if (length(files) == 0) {
  stop("年付きサマリTXTが見つかりません。まず 22_05_linear_anticipation.R を年ごとに回して",
       " did_linear_anticipation_ddd_summary_YYYY.txt を作ってください。")
}

# 2) 1ファイルを安全にパースする関数
extract_num <- function(x, pat) {
  if (is.na(x)) return(NA_real_)
  m <- str_match(x, pat)
  if (is.na(m[,2])) NA_real_ else suppressWarnings(as.numeric(m[,2]))
}
parse_summary <- function(f){
  ln <- read_lines(f)
  # 足りない行があってもNAで返す
  get_ln <- function(i) if (length(ln) >= i) ln[i] else NA_character_

  tibble(
    post_year = extract_num(basename(f), "(\\d{4})"),
    near_pct  = extract_num(get_ln(2), "near \\(%\\)\\s*=\\s*([-0-9.]+)\\s*$"),
    ddd_pct   = extract_num(get_ln(3), "DDD near:post:treated \\(%\\)\\s*=\\s*([-0-9.]+)\\s*$"),
    p_unw     = extract_num(get_ln(4), "Wald p \\(unweighted\\)\\s*=\\s*([0-9.]+|NA)\\s*$"),
    p_w       = extract_num(get_ln(5), "Wald p \\(weighted\\)\\s*=\\s*([0-9.]+|NA)\\s*$")
  )
}

# 3) 全ファイルを集計
res <- map_dfr(files, parse_summary) %>%
  arrange(post_year) %>%
  mutate(
    sig_unw = !is.na(p_unw) & p_unw < 0.05,
    sig_w   = !is.na(p_w)   & p_w   < 0.05
  )

print(res, n = Inf)

# 4) CSV保存
out_csv <- here::here("output","model","linear_anticipation_grid_results.csv")
write_csv(res, out_csv)
message("Saved: ", out_csv)

# 5) 図も保存（DDD効果％の閾年グリッド）
fig_dir <- here::here("output","fig","model")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
p <- ggplot(res, aes(post_year, ddd_pct)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  geom_point(aes(shape = sig_w)) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), drop = FALSE) +
  labs(x = "post_year（閾年）", y = "DDD効果（%）",
       shape = "weightedで\np<0.05?",
       title = "リニア先行便益（閾年グリッド・DDD効果）")
print(p)
ggsave(filename = file.path(fig_dir, "linear_anticipation_grid_results.png"),
       plot = p, width = 7, height = 4.5, dpi = 300)
message("Saved: ", file.path(fig_dir, "linear_anticipation_grid_results.png"))