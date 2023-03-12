print_epi_step <- function(
    tr_obj = NULL, untr_obj = NULL, trained = FALSE, title = NULL,
    width = max(20, options()$width - 30), case_weights = NULL,
    conjunction = NULL, extra_text = NULL) {
  title <- trimws(title)
  trained_text <- dplyr::if_else(trained, "Trained", "")
  case_weights_text <- dplyr::case_when(
    is.null(case_weights) ~ "",
    isTRUE(case_weights) ~ "weighted",
    isFALSE(case_weights) ~ "ignored weights")
  vline_seperator <- dplyr::if_else(trained_text == "", "", "|")
  comma_seperator <- dplyr::if_else(
    trained_text != "" && case_weights_text != "", true = ",", false = "")
  extra_text <- recipes::format_ch_vec(extra_text)
  width_title <- nchar(paste0(
    "* ", title, ":", " ", conjunction, " ", extra_text, " ", vline_seperator,
    " ", trained_text, " ", comma_seperator, " ", case_weights_text
  ))
  width_diff <- cli::console_width() * 1 - width_title
  if (trained) {
    elements <- tr_obj
  } else {
    elements <- lapply(untr_obj, function(x) {
      rlang::expr_deparse(rlang::quo_get_expr(x), width = Inf)
    })
    elements <- vctrs::list_unchop(elements, ptype = character())
  }
  if (length(elements) == 0L) elements <- "<none>"

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
    c(`*` = "\n    {title}: \\\n    {.pkg {elements[seq_len(first_line)]}}\\\n    {more_dots} \\\n    {conjunction} \\\n    {.pkg {extra_text}} \\\n    {vline_seperator} \\\n    {.emph {trained_text}}\\\n    {comma_seperator} \\\n    {.emph {case_weights_text}}\n    "))
  invisible(NULL)
}
