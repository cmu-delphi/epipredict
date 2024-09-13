#' Adapt the model to latent data
#'
#' @description
#' In the standard case, the arx models assume that the last observation is also
#' the day from which the forecast is being made. But if the data has latency,
#' then you may wish to adjust the predictors (lags) and/or the outcome (ahead)
#' to compensate.
#' This is most useful in realtime and
#' pseudo-prospective forecasting for data where there is some delay between the
#' event occurring and the event being reported.
#'
#' @details
#' This step allows the user to create models on the most recent
#' data, automatically accounting for latency patterns. Instead of using the last observation
#' date, `step_adjust_latency` uses the `as_of` date of the `epi_df` as the
#' `forecast_date`, and adjusts the model so that there is data available. To
#'   demonstrate some of the subtleties, let's consider a toy dataset:
#' ```{r toy_df}
#' toy_df <- tribble(
#'  ~geo_value, ~time_value, ~a, ~b,
#'  "ma", as.Date("2015-01-11"), 20, 6,
#'  "ma", as.Date("2015-01-12"), 23, NA,
#'  "ma", as.Date("2015-01-13"), 25, NA,
#'  "ca", as.Date("2015-01-11"), 100, 5,
#'  "ca", as.Date("2015-01-12"), 103, 10,
#' ) %>%
#'    as_epi_df(as_of = as.Date("2015-01-14"))
#' ```
#' If we're looking to predict the value on the 15th, forecasting from the 14th (the `as_of` date above),
#'   there are two issues we will need to address:
#' 1. `"ca"` is latent by 2 days, whereas `"ma"` is latent by 1
#' 2. if we want to use `b` as an exogenous variable, for `"ma"` it is latent by 3 days instead of just 1.
#'
#' Regardless of `method`, `epi_keys_checked="geo_value"` guarantees tha the
#'   difference between `"ma"` and `"ca"` is accounted for by making  the
#'   latency adjustment at least 2. For some comparison, here's what the various
#'   methods will do:
#'
#' ## `locf`
#' Short for "last observation carried forward", `locf` assumes that every day
#'   between the last observation and the forecast day is exactly the same.
#'   This is a very straightforward assumption, but wrecks any features that
#'   depend on changes in value over time, such as the growth rate, or even
#'   adjacent lags. A more robust version of this falls under the heading of
#'   nowcasting, an eventual aim for this package. On the toy dataset, it
#'   doesn't matter which day we're trying to predict, since it just fills
#'   forward to the `forecast_date`:
#'   ```{r toy_df}
#'   toy_recipe <- epi_recipe(toy_df) %>%
#'     step_adjust_latency(method="locf")
#'
#'   toy_recipe %>%
#'     prep(toy_df) %>%
#'     bake(toy_df) %>%
#'     arrange(geo_value, time_value)
#'   ```
#'
#' ## `extend_lags`
#' `extend_lags` increases the lags so that they are guaranteed to have
#'   data. This has the advantage of being applicable on
#'   a per-column basis; if cases and deaths are reported at different
#'   latencies, the lags for each are adjusted separately. In the toy example:
#'   ```{r toy_df}
#'   toy_recipe <- epi_recipe(toy_df) %>%
#'     step_adjust_latency(method="extend_lags") %>%
#'     step_epi_lag(a,lag=1) %>%
#'     step_epi_lag(b,lag=1) %>%
#'     step_epi_ahead(a, ahead=1)
#'
#'   toy_recipe %>%
#'     prep(toy_df) %>%
#'     bake(toy_df) %>%
#'     arrange(geo_value, time_value)
#'   ```
#' The maximum latency in column `a` is 2 days, so the lag is increased to 3,
#'   while the max latency in column `b` is 3, so the same lag is increased to
#'   4; both of these changes are reflected in the column names. Meanwhile the
#'   ahead is uneffected.
#'
#'   As a side-note, lag/ahead can be somewhat ambiguous about direction. Here,
#'   the values are brought forward in time, so that for a given row, column
#'   `lag_3_a` represents the value 3 days before.
#'
#' ## `extend_ahead`
#' `extend_ahead` increases the ahead, turning a 3 day ahead forecast
#'   into a 7 day one; this has the advantage of simplicity and is reflective of
#'   the actual modelling task, but potentially leaves information unused if
#'   different data sources have different latencies; it must use the latency of
#'   the most latent data to insure there is data available. In the toy example:
#'   ```{r toy_df}
#'   toy_recipe <- epi_recipe(toy_df) %>%
#'     step_adjust_latency(method="extend_ahead") %>%
#'     step_epi_lag(a,lag=0) %>%
#'     step_epi_ahead(a, ahead=1)
#'
#'   toy_recipe %>%
#'     prep(toy_df) %>%
#'     bake(toy_df) %>%
#'     arrange(geo_value, time_value)
#'   ```
#'   Even though we're doing a 1 day ahead forecast, because our worst latency
#'   is 3 days from column `b`'s `"ma"` data, our outcome column is `ahead_4_a`
#'   (so 4 days ahead). If we want to ignore any latency in column `b`, we need
#'   to explicitly set the columns to consider while adjusting like this:
#'   `step_adjust_latency(a, method="extend_ahead")`.
#'
#' # Programmatic details
#' `step_adjust_latency` uses the metadata, such as `time_type` and `as_of`, of
#'   the `epi_df` used in the initial prep step, rather than baking or
#'   prediction. This means reusing the same forecaster on new data is not
#'   advised, though typically it is not advised in general.
#'
#' The latency adjustment only applies to columns created after this step, so
#'   this step should go before both `step_epi_ahead` and `step_epi_lag`. This will work:
#'  ```{r}
#'  toy_recipe <- epi_recipe(toy_df) %>%
#'     # non-lag steps
#'     step_adjust_latency(a, method = "extend_lags") %>%
#'     step_epi_lag(a, lag=0) # other steps
#'  ```
#'  while this will not:
#'  ```{r}
#'  toy_recipe <- epi_recipe(toy_df) %>%
#'     step_epi_lag(a, lag=0) %>%
#'     step_adjust_latency(a, method = "extend_lags")
#'  ```
#' If you create columns that you then apply lags to (such as
#'   `step_growth_rate()`), these should be created before
#'   `step_adjust_latency`, so any subseqent latency can be addressed.
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
#'   finding the `max_time_value` (the last day of data), defaulting to
#'   `geo_value`. Different locations may have different latencies; to produce a
#'   forecast at every location, we need to guarantee data at every location by
#'   using the largest latency across every location; this means taking
#'   `max_time_value` to be the minimum of the `max_time_value`s for each set of
#'   key values (so the earliest date).  If `NULL` or an empty character vector,
#'   it will take the maximum across all values, irrespective of any keys.
#'
#'   Note that this is a separate concern from different latencies across
#'   different *data columns*, which is only handled by the choice of `method`.
#' @param fixed_latency either a positive integer, or a labeled positive integer
#'   vector. Cannot be set at the same time as `fixed_forecast_date`. If
#'   non-`NULL`, the amount to offset the ahead or lag by. If a single integer,
#'   this is used for all columns; if a labeled vector, the labels must
#'   correspond to the base column names (before lags/aheads).  If `NULL`, the
#'   latency is the distance between the `epi_df`'s `max_time_value` and the `forecast_date`.
#' @param fixed_forecast_date either a date of the same kind used in the
#'   `epi_df`, or `NULL`. Exclusive with `fixed_latency`. If a date, it gives
#'   the date from which the forecast is actually occurring. If `NULL`, the
#'   `forecast_date` is determined either via the `fixed_latency`, or is set to
#'   the `epi_df`'s `as_of` value if `fixed_latency` is also `NULL`.
#' @param check_latency_length bool, determines whether to warn if the latency
#'   is unusually high. Turn off if you know your forecast is going to be far
#'   into the future.
#' @template step-return
#' @inheritParams recipes::step_lag
#'
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
#' @importFrom dplyr tribble n
step_adjust_latency <-
  function(recipe,
           ...,
           method = c(
             "extend_ahead",
             "locf",
             "extend_lags"
           ),
           epi_keys_checked = NULL,
           fixed_latency = NULL,
           fixed_forecast_date = NULL,
           check_latency_length = TRUE,
           id = rand_id("adjust_latency")) {
    arg_is_chr_scalar(id, method)
    if (!is_epi_recipe(recipe)) {
      cli_abort("This recipe step can only operate on an {.cls epi_recipe}.",
        class = "epipredict__step_adjust_latency__epi_recipe_only"
      )
    }
    if ((method == "extend_ahead") && (detect_step(recipe, "epi_ahead"))) {
      cli_warn(
        "If `method` is {.val extend_ahead}, then the previous `step_epi_ahead` won't be modified.",
        class = "epipredict__step_adjust_latency__misordered_step_warning"
      )
    } else if ((method == "extend_lags") && detect_step(recipe, "epi_lag")) {
      cli_warn(
        "If `method` is {.val extend_lags} or {.val locf},
then the previous `step_epi_lag`s won't work with modified data.",
        class = "epipredict__step_adjust_latency__misordered_step_warning"
      )
    } else if ((method == "locf") && (length(recipe$steps) > 0)) {
      cli_warn("There are steps before `step_adjust_latency`. With the method {.val locf}, it is recommended to include this step before any others",
        class = "epipredict__step_adjust_latency__misordered_step_warning"
      )
    }
    if (detect_step(recipe, "naomit")) {
      cli_abort("adjust_latency needs to occur before any `NA` removal,
                      as columns may be moved around", class = "epipredict__step_adjust_latency__post_NA_error")
    }
    if (!is.null(fixed_latency) && !is.null(fixed_forecast_date)) {
      cli_abort("Only one of `fixed_latency` and `fixed_forecast_date`
 can be non-`NULL` at a time!", class = "epipredict__step_adjust_latency__too_many_args_error")
    }
    if (length(fixed_latency > 1)) {
      template <- recipe$template
      data_names <- names(template)[!names(template) %in% key_colnames(template)]
      wrong_names <- names(fixed_latency)[!names(fixed_latency) %in% data_names]
      if (length(wrong_names) > 0) {
        cli_abort("{.val fixed_latency} contains names not in the template dataset: {wrong_names}", class = "epipredict__step_adjust_latency__undefined_names_error")
      }
    }
    method <- rlang::arg_match(method)
    if (method == "extend_ahead") {
      rel_step_type <- "step_epi_ahead"
      shift_name <- "ahead"
    } else if (method == "extend_lags") {
      rel_step_type <- "step_epi_lag"
      shift_name <- "lag"
    }
    if (is.null(epi_keys_checked)) {
      epi_keys_checked <- kill_time_value(key_colnames(recipe$template))
    }
    recipes::add_step(
      recipe,
      step_adjust_latency_new(
        terms = enquos(...),
        role = NA,
        method = method,
        epi_keys_checked = epi_keys_checked,
        check_latency_length = check_latency_length,
        trained = FALSE,
        forecast_date = fixed_forecast_date,
        latency = fixed_latency,
        latency_table = NULL,
        metadata = NULL,
        keys = key_colnames(recipe),
        columns = NULL,
        skip = FALSE,
        id = id
      )
    )
  }

step_adjust_latency_new <-
  function(terms, role, trained, forecast_date, latency, latency_table,
           metadata, time_type, keys, method, epi_keys_checked, check_latency_length, columns, skip,
           id) {
    step(
      subclass = "adjust_latency",
      terms = terms,
      role = role,
      method = method,
      epi_keys_checked = epi_keys_checked,
      check_latency_length = check_latency_length,
      trained = trained,
      forecast_date = forecast_date,
      latency = latency,
      latency_table = latency_table,
      metadata = metadata,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id
    )
  }
# lags introduces max(lags) NA's after the max_time_value.
#' @export
#' @importFrom glue glue
#' @importFrom dplyr rowwise
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
      rowwise() %>%
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
  if (is_empty(terms_used)) {
    terms_used <- info %>%
      filter(role == "raw") %>%
      pull(variable)
  }



  step_adjust_latency_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    forecast_date = forecast_date,
    latency = unique(latency_table$latency),
    latency_table = latency_table,
    metadata = attributes(training)$metadata,
    keys = x$keys,
    method = x$method,
    epi_keys_checked = x$epi_keys_checked,
    check_latency_length = x$check_latency_length,
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
    new_data <- as_epi_df(new_data)
    attributes(new_data)$metadata <- object$metadata
    attributes(new_data)$metadata$as_of <- object$forecast_date
  }
  if (object$method == "extend_ahead" || object$method == "extend_lags") {
    attributes(new_data)$metadata$latency_method <- object$method
    attributes(new_data)$metadata$shift_sign <- get_sign(object)
    attributes(new_data)$metadata$latency_table <- object$latency_table
    attributes(new_data)$metadata$forecast_date <- object$forecast_date
    keys <- object$keys
  } else if (object$method == "locf") {
    # locf doesn't need to mess with the metadata at all, it just forward-fills the requested columns
    rel_keys <- setdiff(key_colnames(new_data), "time_value")
    modified_columns <- object$columns %>% unname()
    if (object$check_latency_length) {
      check_interminable_latency(new_data, object$latency_table, modified_columns, object$forecast_date)
    }

    new_data <- new_data %>%
      pad_to_end(rel_keys, object$forecast_date, modified_columns) %>%
      # group_by_at(rel_keys) %>%
      arrange(time_value) %>%
      as_tibble() %>%
      tidyr::fill(.direction = "down", any_of(modified_columns)) %>%
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
