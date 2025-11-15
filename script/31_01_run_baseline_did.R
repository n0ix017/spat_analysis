## script/31_01_run_baseline_did.R
suppressPackageStartupMessages({
  library(here); library(dplyr); library(ggplot2)
  library(modelsummary)     # 表出力（任意）
})

# 設定・関数
source(here("code","01_config_paths.R"))
source(here("code","02_config_params.R"))
source(here("code","05_treatment_definitions.R"))
source(here("code","06_models.R"))
source(here("code","04_util_qa.R"))

# ▼ 入力：駅×距離帯×地点×年（L02 連結済み）のパネルRDS
#   ※ build_full_panel.R で作った RDS を config に定義しておく
panel <- readRDS(PANEL_RDS)  # 例：data_fmt/fmt_rds/panel_band_l02.rds

# ▼ 処置付与（まずは 0-1km 帯、こだま含む、ポスト=2024）
panel_did <- make_treatment_flags(
  panel,
  band_max_km   = getOption("DID_BAND_MAX_KM", default = 1),
  include_kodama = TRUE,
  post_year      = 2024
)

# ▼ クリーニング（欠損除去・必要列の存在確認）
panel_did <- panel_did %>%
  filter(!is.na(log_price)) %>%
  mutate(id_l02 = as.factor(id_l02))  # 個体FE用のID列（作成済み前提）

# ▼ 推定（クラスターは市区町村コードがあれば推奨：L02_020）
mod_did  <- run_did_baseline(panel_did, cluster_var = "L02_020", price_var = "log_price")
mod_evt  <- run_event_study(panel_did,  price_var = "log_price",
                            ref_years = 2019:2023, cluster_var = "L02_020")

# ▼ 保存
dir.create(MODEL_OUT_DIR, showWarnings = FALSE, recursive = TRUE)
saveRDS(list(did = mod_did, event = mod_evt), file = file.path(MODEL_OUT_DIR, "did_baseline_0to1km.rds"))

# ▼ 簡易の表/図（任意）
if (qa_on()) {
  qa_tbl_path <- qa_path("31_did_table.html")
  modelsummary(list("DID (Post2024×Treat)" = mod_did),
               output = qa_tbl_path)
  message("QA table: ", qa_tbl_path)

  # 係数プロット（イベントスタディ）
  evt_tidy <- broom::tidy(mod_evt) %>% filter(grepl("^year::", term))
  p <- ggplot(evt_tidy, aes(x = term, y = estimate)) +
    geom_point() + geom_errorbar(aes(ymin = estimate-1.96*std.error,
                                     ymax = estimate+1.96*std.error), width = .2) +
    coord_flip() + labs(title = "Event-study (ref: 2019–2023)",
                        y = "coef (log price)", x = "year×treat")
  qa_png(p, "31_event_study_coefs")
}