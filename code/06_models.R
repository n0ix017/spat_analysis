## code/06_models.R
suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)   # feols
})

# ベースラインDID： log_price ~ treat * post_2024 + 個体FE + 年FE
# cluster_var でクラスタSE（例：市区町村コード L02_020）
run_did_baseline <- function(df, cluster_var = NULL, price_var = "log_price") {
  stopifnot(all(c("treat","post_2024",price_var) %in% names(df)))
  fml <- as.formula(sprintf("%s ~ treat:post_2024 | id_l02 + year", price_var))
  if (is.null(cluster_var)) {
    feols(fml, data = df)
  } else {
    stopifnot(cluster_var %in% names(df))
    feols(fml, data = df, cluster = df[[cluster_var]])
  }
}

# イベントスタディ（年×処置の動学）:
# 参照期を 2023 に置き、2024,2025…の相対効果を可視化
run_event_study <- function(df, price_var = "log_price",
                            ref_years = 2019:2023, cluster_var = NULL) {
  stopifnot(all(c("treat","year",price_var) %in% names(df)))
  fml <- as.formula(sprintf("%s ~ i(year, treat, ref = c(%s)) | id_l02 + year",
                            price_var, paste(ref_years, collapse=",")))
  if (is.null(cluster_var)) {
    feols(fml, data = df)
  } else {
    feols(fml, data = df, cluster = df[[cluster_var]])
  }
}