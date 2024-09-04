#' Adapt the model to latent data
#'
#' In the standard case, the arx models assume that the last observation is also
#' the day from which the forecast is being made. But if the data has latency,
#' then you may wish to adjust the predictors (lags) and/or the outcome (ahead)
#' to compensate. This allows the user to create models on the most recent data,
#' regardless of latency patterns. Instead of using the last observation date,
#' `step_adjust_latency` uses the `as_of` date of the `epi_df` as the
#' `forecast_date`, potentially using different dates depending on the
#' `epi_keys`, such as geography. This is most useful in realtime and
#' pseudo-prospective forecasting for data where there is some delay between the
#' event occurring and the event being reported.
#'
#' @param method a character. Determines the method by which the
#'   forecast handles latency. The options are:
#'   - `"extend_ahead"`: Lengthen the ahead so that forecasting from the last
#'   observation results in a forecast `ahead` after the `forecast_date` date.
#'   E.g. if there are 3 days of latency between the last observation and the
#'   `forecast_date` date for a 4 day ahead forecast, the ahead used in practice
#'   is actually 7.
#'   - `"locf"`: carries forward the last observed value(s) up to the forecast
#'   date.
#'   - `"extend_lags"`: per `epi_key` and `predictor`, adjusts the lag so that
#'   the shortest lag at predict time is at the last observation. E.g. if the
#'   lags are `c(0,7,14)` for data that is 3 days latent, the actual lags used
#'   become `c(3,10,17)`.
#' @param epi_keys_checked a character vector. A list of keys to group by before
#'   finding the `max_time_value`.  The default value of this is
#'   `c("geo_value")`, but it can be any collection of `epi_keys`.  Different
#'   locations may have different latencies; to produce a forecast at every
#'   location, we need to use the largest latency across every location; this
#'   means taking `max_time_value` to be the minimum of the `max_time_value`s
#'   for each `geo_value` (or whichever collection of keys are specified).  If
#'   `NULL` or an empty character vector, it will take the maximum across all
#'   values, irrespective of any keys.
#' @param fixed_latency either a positive integer, or a labeled positive integer
#'   vector. Cannot be set at the same time as `fixed_forecast_date`. If
#'   non-`NULL`, the amount to offset the ahead or lag by. If a single integer,
#'   this is used for all columns; if a labeled vector, the labels must
#'   correspond to the base column names (before lags/aheads).  If `NULL`, the
#'   latency is the distance between the `epi_df`'s `max_time_value` and either
#'   the `fixed_forecast_date` or the `epi_df`'s `as_of` field (the default for
#'   `forecast_date`).
#' @param fixed_forecast_date either a date of the same kind used in the
#'   `epi_df`, or `NULL`. Exclusive with `fixed_latency`. If a date, it gives
#'   the date from which the forecast is actually occurring. If `NULL`, the
#'   `forecast_date` is determined either via the `fixed_latency`, or is set to
#'   the `epi_df`'s `as_of` value if `fixed_latency` is also `NULL`.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned? `lag` is a predictor while `ahead` is an outcome.  It
#'   should be correctly inferred and not need setting
#' @param default Determines what fills empty rows
#'   left by leading/lagging (defaults to NA).
#' @template step-return
#' @inheritParams recipes::step_lag
#'
#' @details The step assumes that the pipeline has already applied either
#'   `step_epi_ahead` or `step_epi_lag` depending on the value of `"method"`,
#'   and that `step_epi_naomit` has NOT been run.  By default, the latency will
#'   be determined using the arguments below, but can be set explicitly using
#'   either `fixed_latency` or `fixed_forecast_date`.
#'
#' The `prefix` and `id` arguments are unchangeable to ensure that the code runs
#' properly and to avoid inconsistency with naming. For `step_epi_ahead`, they
#' are always set to `"ahead_"` and `"epi_ahead"` respectively, while for
#' `step_epi_lag`, they are set to `"lag_"` and `"epi_lag`, respectively.
#'
#' @family row operation steps
#' @rdname step_adjust_latency
#' @export
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' # setting the `as_of` to something realistic
#' attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
#'
#' r <- epi_recipe(case_death_rate_subset) %>%
#'   step_adjust_latency(method = "extend_ahead") %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14))
#' r
#'
#' jhu_fit <- epi_workflow() %>%
#'   add_epi_recipe(r) %>%
#'   add_model(linear_reg()) %>%
#'   fit(data = jhu)
#' jhu_fit
#'
#' @importFrom recipes detect_step
#' @importFrom rlang enquos is_empty
step_adjust_latency <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           method = c(
             "extend_ahead",
             "locf",
             "extend_lags"
           ),
           epi_keys_checked = c("geo_value"),
           fixed_latency = NULL,
           fixed_forecast_date = NULL,
           default = NA,
           skip = FALSE,
           columns = NULL,
           id = recipes::rand_id("adjust_latency")) {
    arg_is_chr_scalar(id, method)
    if (!is_epi_recipe(recipe)) {
      cli::cli_abort("This recipe step can only operate on an {.cls epi_recipe}.", class = "epipredict__step_adjust_latency__epi_recipe_only")
    }
    if (!is.null(columns)) {
      cli::cli_abort(c("The `columns` argument must be `NULL`.",
        i = "Use `tidyselect` methods to choose columns to lag."
      ), class = "epipredict__step_adjust_latency__cols_not_null")
    }
    if ((method == "extend_ahead") && (detect_step(recipe, "epi_ahead"))) {
      cli::cli_warn(
        "If `method` is {.val extend_ahead}, then the previous `step_epi_ahead` won't be modified.",
        class = "epipredict__step_adjust_latency__misordered_step_warning"
      )
    } else if ((method == "extend_lags") && detect_step(recipe, "epi_lag")) {
      cli::cli_warn(
        "If `method` is {.val extend_lags} or {.val locf},
then the previous `step_epi_lag`s won't work with modified data.",
        class = "epipredict__step_adjust_latency__misordered_step_warning"
      )
    } else if ((method == "locf") && (length(recipe$steps) > 0)) {
      cli::cli_warn("There are steps before `step_adjust_latency`. With the method {.val locf}, it is recommended to include this step before any others",
        class = "epipredict__step_adjust_latency__misordered_step_warning"
      )
    }
    if (detect_step(recipe, "naomit")) {
      cli::cli_abort("adjust_latency needs to occur before any `NA` removal,
                      as columns may be moved around", class = "epipredict__step_adjust_latency__post_NA_error")
    }
    if (!is.null(fixed_latency) && !is.null(fixed_forecast_date)) {
      cli::cli_abort("Only one of `fixed_latency` and `fixed_forecast_date`
 can be non-`NULL` at a time!", class = "epipredict__step_adjust_latency__too_many_args_error")
    }
    if (length(fixed_latency > 1)) {
      template <- recipe$template
      data_names <- names(template)[!names(template) %in% key_colnames(template)]
      wrong_names <- names(fixed_latency)[!names(fixed_latency) %in% data_names]
      if (length(wrong_names) > 0) {
        cli::cli_abort("{.val fixed_latency} contains names not in the template dataset: {wrong_names}", class = "epipredict__step_adjust_latency__undefined_names_error")
      }
    }

    method <- rlang::arg_match(method)
    terms_used <- recipes_eval_select(enquos(...), recipe$template, recipe$term_info)
    if (is_empty(terms_used)) {
      terms_used <- recipe$term_info %>%
        filter(role == "raw") %>%
        pull(variable)
    }
    if (method == "extend_ahead") {
      rel_step_type <- "step_epi_ahead"
      shift_name <- "ahead"
    } else if (method == "extend_lags") {
      rel_step_type <- "step_epi_lag"
      shift_name <- "lag"
    }

    recipes::add_step(
      recipe,
      step_adjust_latency_new(
        terms = enquos(...),
        role = role,
        method = method,
        epi_keys_checked = epi_keys_checked,
        trained = trained,
        forecast_date = fixed_forecast_date,
        latency = fixed_latency,
        latency_table = NULL,
        default = default,
        keys = key_colnames(recipe),
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_adjust_latency_new <-
  function(terms, role, trained, forecast_date, latency, latency_table, time_type, default,
           keys, method, epi_keys_checked, columns, skip, id) {
    step(
      subclass = "adjust_latency",
      terms = terms,
      role = role,
      method = method,
      epi_keys_checked = epi_keys_checked,
      trained = trained,
      forecast_date = forecast_date,
      latency = latency,
      latency_table = latency_table,
      default = default,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id
    )
  }
#' @importFrom recipes recipes_eval_select
construct_latency_table <- function(x, latency, training, info) {
  return(latency_table)
}
# lags introduces max(lags) NA's after the max_time_value.
#' @export
#' @importFrom glue glue
prep.step_adjust_latency <- function(x, training, info = NULL, ...) {
  sign_shift <- get_sign(x)
  latency <- x$latency
  forecast_date <- x$forecast_date %||% set_forecast_date(training, info, x$epi_keys_checked, latency)
  # construct the latency table
  latency_table <- names(training)[!names(training) %in% key_colnames(training)] %>%
    tibble(col_name = .)
  if (length(recipes_eval_select(x$terms, training, info)) > 0) {
    latency_table <- latency_table %>% filter(col_name %in%
      recipes_eval_select(x$terms, training, info))
  }

  if (is.null(latency)) {
    latency_table <- latency_table %>%
      mutate(latency = get_latency(training, forecast_date, col_name, sign_shift, x$epi_keys_checked))
  } else if (length(latency) > 1) {
    # if latency has a length, it must also have named elements. We assign based on comparing the name in the list
    # with the column names, and drop any which don't have a latency assigned
    latency_table <- latency_table %>%
      filter(col_name %in% names(latency)) %>%
      rowwise() %>%
      mutate(latency = unname(latency[names(latency) == col_name])) %>%
      ungroup()
  } else {
    latency_table <- latency_table %>% mutate(latency = latency)
  }
  attributes(training)$metadata$latency_table <- latency_table
  # get the columns used, even if it's all of them
  terms_used <- x$terms
  # TODO replace with is_empty as in bake.recipe
  if (is_empty(terms_used)) {
    terms_used <- info %>%
      filter(role == "raw") %>%
      pull(variable)
  }

  # check that the shift amount isn't too extreme
  latency_max <- max(abs(latency_table$latency))
  time_type <- attributes(training)$metadata$time_type
  i_latency <- which.max(latency_table$latency)
  if (
    (grepl("day", time_type) && (latency_max >= 10)) ||
      (grepl("week", time_type) && (latency_max >= 4)) ||
      ((time_type == "yearmonth") && (latency_max >= 2)) ||
      ((time_type == "yearquarter") && (latency_max >= 1)) ||
      ((time_type == "year") && (latency_max >= 1))
  ) {
    cli::cli_warn(paste(
      "!" = paste(
        "The maximum latency is {latency_max}, ",
        "which is questionable for it's `time_type` of ",
        "{time_type}."
      ),
      "i" = "latency: {latency_table$latency[[i_latency]]}",
      "i" = "`max_time` = {max(training$time_value)} -> `forecast_date` = {forecast_date}"
    ), class = "epipredict__prep.step_latency__very_large_latency")
  }

  step_adjust_latency_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    forecast_date = forecast_date,
    latency = unique(latency_table$latency),
    latency_table = latency_table,
    default = x$default,
    keys = x$keys,
    method = x$method,
    epi_keys_checked = x$epi_keys_checked,
    columns = recipes_eval_select(latency_table$col_name, training, info),
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom dplyr %>% pull group_by_at
#' @importFrom tidyr fill
#' @export
bake.step_adjust_latency <- function(object, new_data, ...) {
  if (!isa(new_data, "epi_df")) {
    # TODO if new_data actually has keys other than geo_value and time_value, this is going to cause problems
    new_data <- new_data %>% as_epi_df(as_of = object$forecast_date)
  }
  if (object$method == "extend_ahead" || object$method == "extend_lags") {
    attributes(new_data)$metadata$latency_method <- object$method
    attributes(new_data)$metadata$shift_sign <- get_sign(object)
    attributes(new_data)$metadata$latency_table <- object$latency_table
    keys <- object$keys
  } else if (object$method == "locf") {
    # locf doesn't need to mess with the metadata at all, it just forward-fills the requested columns
    rel_keys <- setdiff(key_colnames(new_data), "time_value")
    unnamed_columns <- object$columns %>% unname()
    new_data %>%
      pad_to_end(rel_keys, object$forecast_date) %>%
      # group_by_at(rel_keys) %>%
      arrange(time_value) %>%
      as_tibble() %>%
      tidyr::fill(.direction = "down", any_of(unnamed_columns)) %>%
      ungroup()
  }
  return(new_data)
}
#' @export
print.step_adjust_latency <-
  function(x, width = max(20, options$width - 35), ...) {
    if (!is.null(x$forecast_date)) {
      conj <- "with forecast date"
      extra_text <- x$forecast_date
    } else if (!is.null(x$latency)) {
      conj <- if (length(x$latency == 1)) {
        "with latency"
      } else {
        "with latencies"
      }
      extra_text <- x$latency
    } else {
      conj <- "with latency"
      extra_text <- "set at train time"
    }
    # what follows is a somewhat modified version of print_epi_step, since the case of no arguments for adjust_latency means apply to all relevant columns, and not none of them
    theme_div_id <- cli::cli_div(
      theme = list(.pkg = list(`vec-trunc` = Inf, `vec-last` = ", "))
    )
    # this is a slightly modified copy of
    title <- trimws(x$method)
    trained_text <- dplyr::if_else(x$trained, "Trained", "")
    vline_seperator <- dplyr::if_else(trained_text == "", "", "|")
    comma_seperator <- dplyr::if_else(
      trained_text != "", true = ",", false = ""
    )
    extra_text <- recipes::format_ch_vec(extra_text)
    width_title <- nchar(paste0(
      "* ", title, ":", " ", conj, " ", extra_text, " ", vline_seperator,
      " ", trained_text, " "
    ))
    width_diff <- cli::console_width() * 1 - width_title
    if (x$trained) {
      elements <- x$columns
    } else {
      if (is_empty(x$terms)) {
        elements <- "all future predictors"
      } else {
        elements <- lapply(x$terms, function(x) {
          rlang::expr_deparse(rlang::quo_get_expr(x), width = Inf)
        })
        elements <- vctrs::list_unchop(elements, ptype = character())
      }
    }

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
      c("\n    {title}: \\\n    {.pkg {cli::cli_vec(elements[seq_len(first_line)])}}\\\n    {more_dots} \\\n    {conj} \\\n    {.pkg {extra_text}} \\\n    {vline_seperator} \\\n    {.emph {trained_text}}")
    )

    cli::cli_end(theme_div_id)
    invisible(x)
  }
