#' Yeo-Johnson transformation
#'
#' `step_epi_YeoJohnson()` creates a *specification* of a recipe step that will
#' transform data using a Yeo-Johnson transformation. This fork works with panel
#' data and is meant for epidata.
#'
#' @inheritParams step_population_scaling
#' @param trained A logical for whether the selectors in `...`
#' have been resolved by [prep()].
#' @param yj_params Internal. A numeric vector of transformation values. This
#'  is `NULL` until computed by [prep()].
#' @param na_fill A numeric value to fill in for any geos where a Yeo-Johnson
#'  parameter cannot be estimated.
#' @param limits A length 2 numeric vector defining the range to compute the
#'  transformation parameter.
#' @param num_unique An integer where data that have fewer than this many unique
#'  values will not be evaluated for a transformation.
#' @param na_rm A logical indicating whether missing values should be removed
#'  before estimating the transformation parameter.
#' @template step-return
#' @family individual transformation steps
#' @export
#' @details The Yeo-Johnson transformation is variance-stabilizing
#'  transformation, similar to the Box-Cox but does not require the input
#'  variables to be strictly positive. In the package, the partial
#'  log-likelihood function is directly optimized within a reasonable set of
#'  transformation values (which can be changed by the user). The optimization
#'  finds a lambda parameter for each group in the data that minimizes the
#'  variance of the transformed data.
#'
#' This transformation is typically done on the outcome variable
#'  using the residuals for a statistical model (such as ordinary
#'  least squares). Here, a simple null model (intercept only) is
#'  used to apply the transformation to the *predictor*
#'  variables individually. This can have the effect of making the
#'  variable distributions more symmetric.
#'
#' If the transformation parameters are estimated to be very
#'  close to the bounds, or if the optimization fails, a value of
#'  `NA` is used and no transformation is applied.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `value` , and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{value}{numeric, the lambda estimate}
#'   \item{id}{character, id of this step}
#' }
#'
#' @references Yeo, I. K., and Johnson, R. A. (2000). A new family of power
#'   transformations to improve normality or symmetry. *Biometrika*.
#' @examples
#' jhu <- cases_deaths_subset %>%
#'   filter(time_value > "2021-01-01", geo_value %in% c("ca", "ny")) %>%
#'   select(geo_value, time_value, cases)
#' filtered_data <- jhu
#'
#' r <- epi_recipe(filtered_data) %>%
#'   step_epi_YeoJohnson(cases)
#' # View the recipe
#' r
#' # Fit the recipe
#' tr <- r %>% prep(filtered_data)
#' # View the parameter values
#' tr$steps[[1]]$yj_params
#' # View the transformed data
#' df <- tr %>% bake(filtered_data)
#' plot(density(df$cases))
#' plot(density(filtered_data$cases))
step_epi_YeoJohnson <- function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    yj_params = NULL,
    na_fill = 1 / 4,
    limits = c(-5, 5),
    num_unique = 5,
    na_rm = TRUE,
    skip = FALSE,
    id = rand_id("epi_YeoJohnson")) {
  checkmate::assert_numeric(limits, len = 2)
  checkmate::assert_numeric(na_fill, lower = min(limits), upper = max(limits), len = 1)
  checkmate::assert_numeric(num_unique, lower = 2, upper = Inf, len = 1)
  checkmate::assert_logical(na_rm, len = 1)
  checkmate::assert_logical(skip, len = 1)
  add_step(
    recipe,
    step_epi_YeoJohnson_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      yj_params = yj_params,
      na_fill = na_fill,
      limits = sort(limits)[1:2],
      num_unique = num_unique,
      na_rm = na_rm,
      forecast_date = NULL,
      metadata = NULL,
      columns = NULL,
      skip = skip,
      id = id
    )
  )
}

step_epi_YeoJohnson_new <- function(
    terms,
    role,
    trained,
    yj_params,
    na_fill,
    limits,
    num_unique,
    na_rm,
    forecast_date,
    metadata,
    columns,
    skip,
    id) {
  step(
    subclass = "epi_YeoJohnson",
    terms = terms,
    role = role,
    trained = trained,
    yj_params = yj_params,
    na_fill = na_fill,
    limits = limits,
    num_unique = num_unique,
    na_rm = na_rm,
    forecast_date = forecast_date,
    metadata = metadata,
    columns = columns,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_epi_YeoJohnson <- function(x, training, info = NULL, ...) {
  # Check that the columns selected for transformation are numeric.
  col_names <- recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names], types = c("double", "integer"))

  yj_params <- compute_yj_params(
    training,
    col_names,
    x$limits,
    x$num_unique,
    x$na_fill,
    x$na_rm,
    key_colnames(training, exclude = "time_value")
  )

  step_epi_YeoJohnson_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    yj_params = yj_params,
    na_fill = x$na_fill,
    limits = x$limits,
    num_unique = x$num_unique,
    na_rm = x$na_rm,
    forecast_date = attr(training, "metadata")$as_of,
    metadata = attr(training, "metadata"),
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_epi_YeoJohnson <- function(object, new_data, ...) {
  # If not an epi_df, make it one assuming the template of training data.
  # If it is an epi_df, check that the keys match.
  # Imitating the pattern in step_adjust_latency().
  if (!inherits(new_data, "epi_df") || is.null(attr(new_data, "metadata")$as_of)) {
    new_data <- as_epi_df(
      new_data,
      as_of = object$forecast_date,
      other_keys = object$metadata$other_keys %||% character()
    )
    attr(new_data, "metadata") <- object$metadata
  }
  # Check that the columns for transformation are present in new_data.
  if (!all(object$columns %in% colnames(new_data))) {
    cli::cli_abort(
      "The columns for transformation are not present in the new data."
    )
  }
  # Check that the columns for transformation are present in new_data.
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  # Check that the keys match.
  check <- hardhat::check_column_names(new_data, object$yj_params %>% select(-starts_with(".yj_param_")) %>% colnames())
  if (!check$ok) {
    cli_abort(c(
      "Some variables used for training are not available in {.arg x}.",
      i = "The following required columns are missing: {check$missing_names}"
    ))
  }
  # Transform each column, using the appropriate yj_param column per row.
  new_data <- left_join(new_data, object$yj_params, by = key_colnames(new_data, exclude = "time_value"))
  for (col in col_names) {
    new_data <- new_data %>%
      mutate(!!col := yj_transform(!!sym(col), !!sym(paste0(".yj_param_", col))))
  }
  # Remove the yj_param columns.
  new_data %>%
    select(-starts_with(".yj_param_")) %>%
    ungroup()
}

#' @export
print.step_epi_YeoJohnson <- function(x, width = max(20, options()$width - 39), ...) {
  title <- "Yeo-Johnson transformation (see `yj_params` object for values) on "
  print_epi_step(x$terms, x$terms, title = title, width = width)
  invisible(x)
}

# Compute the yj_param values per group for each column.
compute_yj_params <- function(training, col_names, limits, num_unique, na_fill, na_rm, epi_keys_checked) {
  # Estimate the yj_param for each column, creating a .yj_param_<col> column for
  # each. Note that estimate_yj() operates on each column.
  yj_params <- training %>%
    summarise(
      across(all_of(col_names), ~ estimate_yj(.x, limits, num_unique, na_rm)),
      .by = all_of(epi_keys_checked)
    ) %>%
    dplyr::rename_with(~ paste0(".yj_param_", .x), -all_of(epi_keys_checked))

  # Check for NAs in any of the yj_param_ columns.
  # EDIT: This warning was too noisy. Keeping code around, in case we want it.
  # for (col in col_names) {
  #   if (any(is.na(values[[paste0(".yj_param_", col)]]))) {
  #     cli::cli_warn(
  #       c(
  #         x = "Yeo-Johnson parameter could not be estimated for some geos for {col}.",
  #         i = "Using parameter={x$na_fill} in these cases."
  #       ),
  #       call = rlang::caller_call()
  #     )
  #   }
  # }

  # Fill in NAs with the default yj_param.
  yj_params %>%
    mutate(across(starts_with(".yj_param_"), \(col) ifelse(is.na(col), na_fill, col)))
}


yj_input_type_management <- function(x_in, lambda) {
  if (x_in %>% inherits("quantile_pred")) {
    x <- as.matrix(x_in)
    if (length(lambda) == 1) {
      lambda <- lambda %>%
        rep(prod(dim(x))) %>%
        matrix(dim(x))
    } else if (length(x_in) == length(lambda)) {
      lambda <- lambda %>%
        rep(dim(x)[[2]]) %>%
        matrix(dim(x))
    } else if (length(x) != length(lambda)) {
      cli::cli_abort("Length of `x` must be equal to length of `lambda`.", call = rlang::caller_call(n = 2))
    }
  } else if (!inherits(x_in, "tbl_df") || is.data.frame(x_in)) {
    x <- unlist(x_in, use.names = FALSE)
  } else {
    if (!is.vector(x_in)) {
      x <- as.vector(x_in)
    } else {
      x <- x_in
    }
  }

  # these only apply if x_in isn't a quantile distribution
  if (length(x) > 1 && length(lambda) == 1) {
    lambda <- rep(lambda, length(x))
  } else if (length(x) != length(lambda)) {
    cli::cli_abort("Length of `x` must be equal to length of `lambda`.", call = rlang::caller_call(n = 2))
  }
  list(x, lambda)
}
### Code below taken from recipes::step_YeoJohnson.
### We keep "lambda" here, but above we renamed it to "yj_param".
### Modified yj_transform() to be vectorized in lambda. Also modified to work on distributions.
### https://github.com/tidymodels/recipes/blob/v1.1.1/R/YeoJohnson.R#L172
# Yeo-Johnson transformation
yj_transform <- function(x_in, lambda, ind_neg = NULL, eps = 0.001) {
  if (any(is.na(lambda))) {
    cli::cli_abort("`lambda` cannot be `NA`.", call = rlang::caller_call())
  }
  x_lambda <- yj_input_type_management(x_in, lambda)
  x <- x_lambda[[1]]
  lambda <- x_lambda[[2]]

  transformed <- ifelse(
    x < 0,
    # for negative values we test if lambda is ~2
    ifelse(
      abs(lambda - 2) < eps,
      -log(abs(x) + 1),
      -((abs(x) + 1)^(2 - lambda) - 1) / (2 - lambda)
    ),
    # for non-negative values we test if lambda is ~0
    ifelse(
      abs(lambda) < eps,
      log(abs(x) + 1),
      ((abs(x) + 1)^lambda - 1) / lambda
    )
  )

  if (x_in %>% inherits("quantile_pred")) {
    transformed <- transformed %>% quantile_pred(x_in %@% "quantile_levels")
  }
  transformed
}

## Helper for the log-likelihood calc for eq 3.1 of Yeo, I. K.,
## & Johnson, R. A. (2000). A new family of power transformations
## to improve normality or symmetry. Biometrika. page 957
ll_yj <- function(lambda, y, ind_neg, const, eps = 0.001) {
  n <- length(y)
  y_t <- yj_transform(y, lambda, ind_neg)
  # EDIT: Unused in the original recipes code.
  # mu_t <- mean(y_t)
  var_t <- var(y_t) * (n - 1) / n
  res <- -.5 * n * log(var_t) + (lambda - 1) * const
  res
}

## eliminates missing data and returns -llh
yj_obj <- function(lam, dat, ind_neg, const) {
  ll_yj(lambda = lam, y = dat, ind_neg = ind_neg, const = const)
}

## estimates the values
estimate_yj <- function(dat, limits = c(-5, 5), num_unique = 5, na_rm = TRUE) {
  na_rows <- which(is.na(dat))
  if (length(na_rows) > 0) {
    if (na_rm) {
      dat <- dat[-na_rows]
    } else {
      cli::cli_abort(
        c(
          x = "Missing values are not allowed for the YJ transformation.",
          i = "See {.arg na_rm} option."
        ),
        call = rlang::caller_call(n = 2)
      )
    }
  }

  eps <- .001
  if (length(unique(dat)) < num_unique) {
    return(NA)
  }
  dat_neg <- dat < 0
  ind_neg <- list(is = which(dat_neg), not = which(!dat_neg))

  const <- sum(sign(dat) * log(abs(dat) + 1))

  suppressWarnings(
    res <- optimize(
      yj_obj,
      interval = limits,
      maximum = TRUE,
      dat = dat,
      ind_neg = ind_neg,
      const = const,
      tol = .0001
    )
  )
  lam <- res$maximum
  if (abs(limits[1] - lam) <= eps | abs(limits[2] - lam) <= eps) {
    lam <- NA
  }
  lam
}

# Copied from recipes::tidy.step_BoxCox
#
#' @export
tidy.step_epi_YeoJohnson <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$yj_params),
      value = unname(x$yj_params)
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_dbl
    )
  }
  res$id <- x$id
  res
}
