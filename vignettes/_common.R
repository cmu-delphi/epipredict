knitr::opts_chunk$set(
  digits = 3,
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  dev.args = list(bg = "transparent"),
  dpi = 300,
  cache.lazy = FALSE,
  out.width = "90%",
  fig.align = "center",
  fig.width = 9,
  fig.height = 6
)
ggplot2::theme_set(ggplot2::theme_bw())
options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  stringr.view_n = 6,
  pillar.bold = TRUE,
  width = 77
)
