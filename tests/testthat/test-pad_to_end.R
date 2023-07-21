test_that("test set padding works", {
  dat <- tibble::tibble(
    gr1 = rep(c("a", "b"), times = c(3, 4)),
    time_value = c(1:3, 1:4),
    value = 1:7
  ) %>% arrange(time_value, gr1)
  expect_identical(pad_to_end(dat, "gr1", 3), dat)
  expect_equal(nrow(pad_to_end(dat, "gr1", 4)), 8L)
  p <- pad_to_end(dat, "gr1", 5)
  expect_equal(nrow(p), 10L)
  expect_identical(p$gr1, rep(c("a", "b"), times = 5))
  expect_identical(p$time_value, rep(1:5, each = 2))
  expect_identical(p$value, as.integer(c(1, 4, 2, 5, 3, 6, NA, 7, NA, NA)))

  dat <- dat %>% arrange(gr1)
  dat$gr2 <- c("c", "c", "d", "c", "c", "d", "d")
  dat <- dat %>% arrange(time_value)
  # don't treat it as a group
  p <- pad_to_end(dat, "gr1", 4)
  expect_identical(nrow(p), 8L)
  expect_identical(p$gr2, c(rep("c", 4), "d", "d", NA, "d"))

  # treat it as a group (needs different time_value)
  dat$time_value <- c(1, 1, 2, 2, 1, 1, 2) # double
  p <- pad_to_end(dat, c("gr1", "gr2"), 2)
  expect_equal(nrow(p), 8L)
  expect_identical(p$gr1, rep(c("a", "a", "b", "b"), times = 2))
  expect_identical(p$gr2, rep(c("c", "d"), times = 4))
  expect_identical(p$time_value, rep(c(1, 2), each = 4))
  expect_identical(p$value, as.integer(c(1, 3, 4, 6, 2, NA, 5, 7)))

  # make sure it maintains the epi_df
  dat <- dat %>% dplyr::rename(geo_value = gr1) %>% as_epi_df(dat)
  expect_s3_class(pad_to_end(dat, "geo_value", 2), "epi_df")
})
