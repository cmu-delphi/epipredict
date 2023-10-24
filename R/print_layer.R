print_layer <- function(
    layer_obj = NULL, title = NULL, width = max(20, options()$width - 30),
    conjunction = NULL, extra_text = NULL, ...) {
  title <- trimws(title)
  width_title <- nchar(paste0("* ", title, ":", " "))
  extra_text <- recipes::format_ch_vec(extra_text)
  width_title <- nchar(paste0(
    "* ", title, ":", " ", conjunction, " ", extra_text
  ))
  width_diff <- cli::console_width() * 1 - width_title
  elements <- lapply(layer_obj, function(x) {
    rlang::expr_deparse(rlang::quo_get_expr(x), width = Inf)
  })
  elements <- vctrs::list_unchop(elements, ptype = character())
  if (length(elements) == 0L) elements <- ""
  element_print_lengths <- cumsum(nchar(elements)) +
    c(0L, cumsum(rep(2L, length(elements) - 1))) +
    c(rep(5L, length(elements) - 1), 0L)
  first_line <- which(width_diff >= element_print_lengths)
  first_line <- unname(first_line)
  first_line <- ifelse(
    test = identical(first_line, integer(0)),
    yes = length(element_print_lengths),
    no = max(first_line)
  )
  more_dots <- ifelse(first_line == length(elements), "", ", ...")
  cli::cli_bullets(
    c("\n    {title}: \\\n    {.pkg {elements[seq_len(first_line)]}}\\\n    {more_dots} \\\n    {conjunction} \\\n    {.pkg {extra_text}}")
  )

  invisible(NULL)
}
