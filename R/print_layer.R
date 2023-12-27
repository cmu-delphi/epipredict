print_layer <- function(
    layer_obj = NULL, title = NULL, width = max(20, options()$width - 30),
    conjunction = NULL, extra_text = NULL, ...) {
  title <- trimws(title)
  width_title <- nchar(paste0("99 ", title, ":", " "))
  remaining_width <- cli::console_width() * 1 - width_title
  elements <- lapply(layer_obj, function(x) {
    rlang::expr_deparse(rlang::quo_get_expr(x), width = Inf)
  })
  elements <- vctrs::list_unchop(elements, ptype = character())
  elements <- unname(elements)
  more_dots_elements <- ""
  if (length(elements) == 0L) {
    fmt_elements <- ""
  } else {
    fmt_elements <- cli::cli_vec(elements, list("vec-trunc" = 1L))
    nchar_elements <- cli::ansi_nchar(cli::cli_fmt(
      cli::cli_text("{.pkg {fmt_elements}}")
    )) + 2L
    if (nchar_elements >= remaining_width) {
      fmt_elements <- elements[1]
      more_dots_elements <- ", ..."
      nchar_elements <- nchar(fmt_elements) + 5L
    }
    remaining_width <- remaining_width - nchar(fmt_elements)
  }
  if (is.null(extra_text)) {
    cli::cli_text("{title}: {.pkg {fmt_elements}}{more_dots_elements}")
    return(invisible(NULL))
  }

  nchar_extra_text <- 0L
  if (is.character(extra_text)) extra_text <- trimws(extra_text)
  fmt_extra_text <- cli::cli_vec(
    extra_text,
    style = list("vec-trunc" = 1L, digits = 2L)
  )
  nchar_extra_text <- cli::ansi_nchar(cli::cli_fmt(
    cli::cli_text("{.val {fmt_extra_text}}")
  ))
  nchar_conjunction <- ifelse(is.null(conjunction), 0L, nchar(conjunction) + 2L)
  nchar_extra_text <- nchar_extra_text + nchar_conjunction
  if (nchar_extra_text > remaining_width) {
    cli::cli_text("{title}: {.pkg {fmt_elements}}{more_dots_elements}")
  } else {
    cli::cli_text(
      "{title}: {.pkg {fmt_elements}}{more_dots_elements} {conjunction} {.val {fmt_extra_text}}"
    )
  }
  invisible(NULL)
}

unquote <- function(x) {
  rlang::enexpr(x) %>% rlang::quo_get_expr()
}
