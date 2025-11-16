suppressPackageStartupMessages({library(dplyr); library(yaml)})

.load_l02_cfg <- function(){
  yaml::read_yaml(here::here("code","07_config_column.yml"))
}

# 型変換の小ヘルパ
.coerce <- function(v, to){
  if (to == "integer") return(as.integer(v))
  if (to == "numeric") return(as.numeric(v))
  if (to == "character") return(as.character(v))
  v
}

standardize_l02_columns <- function(x, cfg = .load_l02_cfg()){
  nm <- names(x)
  for (def in cfg){
    canon <- def$canonical
    aliases <- def$aliases
    type <- def$type %||% "character"
    na_if <- def$na_if %||% list()

    # どの別名が入っているか探す
    hit <- aliases[aliases %in% nm]
    if (length(hit) == 0) next

    src <- hit[[1]]
    # まずNA置換
    if (length(na_if) > 0){
      for (v in na_if){
        x[[src]][x[[src]] %in% v] <- NA
      }
    }
    # 型
    x[[src]] <- .coerce(x[[src]], type)
    # リネーム
    if (!(canon %in% nm)) {
      names(x)[names(x) == src] <- canon
      nm <- names(x)
    } else {
      # 既にcanonがあればsrcを捨てる/上書きしない
    }
  }
  x
}

# L02→コントロール用に最小限へ整形（任意）
clean_l02_for_controls <- function(x){
  x <- standardize_l02_columns(x)
  if ("landuse_code" %in% names(x)) {
    x <- dplyr::filter(x, !(landuse_code %in% c("020","20")))  # 林地除外（必要なら）
  }
  x <- x |>
    mutate(
      pref_code     = if ("admin_code" %in% names(x)) substr(admin_code,1,2) else NA_character_,
      log_price     = if ("price_yen_m2" %in% names(x)) log(price_yen_m2) else NA_real_,
      log_site_area = if ("site_area_m2" %in% names(x)) log(site_area_m2) else NA_real_
    )
  keep <- c("year","price_yen_m2","log_price",
            "admin_code","pref_code","city_name",
            "site_area_m2","log_site_area",
            "use_big","use_detail","use_code",
            "bldg_structure","water_flag","gas_flag")
  select(x, any_of(keep), everything())
}