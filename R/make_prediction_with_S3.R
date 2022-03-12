make_prediction_with_S3 <- function(obj, dat, key_vars, time_value) {
  # TODO: validate arguments
  #
  stopifnot(is.data.frame(dat))
  data.table::setnafill(dat, type = "locf")
  dat <- cbind(dat, data.frame(key_vars, time_value))
  newdata <- dat %>%
    dplyr::group_by(key_vars) %>%
    dplyr::filter(time_value == max(time_value))
  point <- predict(obj, newdata = newdata)
  point
}
