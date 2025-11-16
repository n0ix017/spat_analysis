# script/22_05_linear_anticipation.R
suppressPackageStartupMessages({
  library(here); library(dplyr); library(readr); library(fixest)
})

# === 1) データ読み込み（集計済みのバンド×駅×年） ===
panel <- readRDS(here::here("data_fmt","fmt_rds","panel_band_l02_enriched.rds"))

# === 2) 設定：基準年（リニア関連の節目） ===
# 基準年（上書き可能）：呼び出し側で post_year を定義していればそれを採用
post_year <- get0("post_year", ifnotfound = 2014, inherits = TRUE)

# === 3) DID/DDD 用データ整形（0–1km vs 3–5km） ===
dat <- panel %>%
  filter(band_id %in% c("[0,1)","[3,5)")) %>%
  mutate(
    near    = as.integer(band_id == "[0,1)"),        # 近帯
    post    = as.integer(year >= post_year),         # 節目以降
    treated = as.integer(nozomi == 0),               # のぞみ「非停車」= 先行便益候補
    log_p   = log(median_price)
  )

# 参考: QC（欠損や分布）
message(sprintf("rows=%d, years=%d, stations=%d",
                nrow(dat), dplyr::n_distinct(dat$year), dplyr::n_distinct(dat$station_key)))
stopifnot(all(is.finite(dat$log_p)))

# === 4) 推定：3重差分（駅FE, 年FE, クラスタSE=駅） ===
# 係数 'near:post:treated' が「先行便益」の本丸
m_ddd  <- feols(log_p ~ near * post * treated | station_key + year,
                data = dat, cluster = ~ station_key)

# 重み付き版（n_obs）
m_ddd_w <- feols(log_p ~ near * post * treated | station_key + year,
                 data = dat, weights = ~ n_obs, cluster = ~ station_key)

# === 5) 主効果の解釈用（百分率） ===
coef_to_pct <- function(b) 100*(exp(b) - 1)
getb <- function(m, nm){
  cf <- coef(m)
  if (nm %in% names(cf)) unname(cf[nm]) else NA_real_
}
eff_main <- c(
  near_pct              = coef_to_pct(getb(m_ddd, "near")),
  ddd_near_post_treated = coef_to_pct(getb(m_ddd, "near:post:treated"))
)

# === 5’) DDD係数のp値を安全に取得（wald失敗時のフォールバック） ===
get_p <- function(m, term){
  # 1) wald をまず試す（変数名の順序の揺れに備えてバッククォートで囲む）
  p_try <- suppressWarnings(try({
    pvalue(wald(m, paste0("`", term, "` = 0")))
  }, silent = TRUE))
  if (is.numeric(p_try) && length(p_try) == 1 && !is.na(p_try)) return(p_try)

  # 2) 係数表から安全に拾う（順序 permutation も許容）
  ct <- suppressWarnings(try(fixest::coeftable(m), silent = TRUE))
  if (!inherits(ct, "try-error") && !is.null(ct)) {
    rn <- rownames(ct)

    # near/post/treated の並びが入れ替わる場合があるので全 permutation を候補にする
    cands <- c("near:post:treated","near:treated:post",
               "post:near:treated","post:treated:near",
               "treated:near:post","treated:post:near")

    idx <- which(rn %in% cands)
    if (length(idx) >= 1) {
      # p値列名は環境により異なる可能性があるので、最後の列を p 値とみなすフォールバックを用意
      pv_col <- if ("Pr(>|t|)" %in% colnames(ct)) "Pr(>|t|)" else tail(colnames(ct), 1)
      pv <- suppressWarnings(as.numeric(ct[idx[1], pv_col]))
      if (is.finite(pv)) return(pv)
    }
  }

  # 3) それでもダメなら NA
  NA_real_
}

p_ddd   <- get_p(m_ddd,   "near:post:treated")
p_ddd_w <- get_p(m_ddd_w, "near:post:treated")

# === 6) 出力保存 ===
out_dir <- here::here("output","model")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(m_ddd,  file.path(out_dir, "did_linear_anticipation_ddd.rds"))
saveRDS(m_ddd_w,file.path(out_dir, "did_linear_anticipation_ddd_weighted.rds"))

readr::write_lines(
  c(
    sprintf("post_year = %d", post_year),
    sprintf("near (%%)                = %.1f", eff_main["near_pct"]),
    sprintf("DDD near:post:treated (%%) = %.1f", eff_main["ddd_near_post_treated"]),
    sprintf("Wald p (unweighted)      = %s", ifelse(is.na(p_ddd), "NA", sprintf("%.4f", p_ddd))),
    sprintf("Wald p (weighted)        = %s", ifelse(is.na(p_ddd_w), "NA", sprintf("%.4f", p_ddd_w)))
  ),
  file.path(out_dir, "did_linear_anticipation_ddd_summary.txt")
)

# 画面にも簡易表示
print(summary(m_ddd))
print(summary(m_ddd_w))
message("Saved: ",
        file.path(out_dir, "did_linear_anticipation_ddd.rds"), " / ",
        file.path(out_dir, "did_linear_anticipation_ddd_weighted.rds"))