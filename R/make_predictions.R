make_predictions <- function(obj, dat, time_value, key_vars = NULL) {
  # TODO: validate arguments
  #
  stopifnot(is.data.frame(dat))
  if (is.null(key_vars)) keys <- rep("empty", length(time_value))
  else keys <- key_vars
  time_keys <- data.frame(keys, time_value)
  common_names <- names(time_keys)
  key_names <- setdiff(common_names, "time_value")

  dat <- dplyr::left_join(time_keys, dat, by = common_names) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(key_names))) %>%
    tidyr::fill(tidyselect::starts_with("x"))
  ## DJM: Old version below. Replaced with tidyr version above
  #data.table::setDT(dat) # Convert to a data.table object by reference
  #cols <- setdiff(names(dat), common_names)
  #dat[, (cols) := data.table::nafill(.SD, type = "locf"),
  #    .SDcols = cols, by = key_names]
  test_time_value <- max(time_value)
  newdata <- dat %>%
    dplyr::filter(time_value == test_time_value)


  point <- predict(obj, newdata = newdata)
  point
}
