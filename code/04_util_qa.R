library(here)
library(ggplot2)

# QAモード（環境変数でON/OFF）
qa_on <- function() identical(Sys.getenv("QA", "0"), "1")

# 出力先を options() から取得。未設定ならフォールバック。
qa_dir <- function(kind = c("png","html")) {
  kind <- match.arg(kind)
  root <- getOption("spat.qa_root",  here::here("output","qa"))
  dir  <- if (kind == "png")  getOption("spat.qa_png",  file.path(root, "png")) 
          else                 getOption("spat.qa_html", file.path(root, "html"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

qa_png <- function(p, name, width_px = 1600, height_px = 1000, dpi = 96) {
  if (!qa_on()) return(invisible(NULL))
  stopifnot(inherits(p, "ggplot"))
  f <- file.path(qa_dir("png"), paste0(name, ".png"))
  ggsave(filename = f, plot = p,
         width = width_px / dpi, height = height_px / dpi, dpi = dpi)
  message("QA png -> ", f); invisible(f)
}

qa_html <- function(m, name) {
  if (!qa_on()) return(invisible(NULL))
  if (!requireNamespace("mapview", quietly = TRUE)) {
    message("mapview 未導入のため qa_html をスキップ"); return(invisible(NULL))
  }
  f <- file.path(qa_dir("html"), paste0(name, ".html"))
  mapview::mapshot(m, url = f)
  message("QA html -> ", f); invisible(f)
}