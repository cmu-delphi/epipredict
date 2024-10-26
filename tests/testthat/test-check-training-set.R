test_that("training set validation works", {
  template <- epidatasets::cases_deaths_subset[1, ]
  rec <- list(template = template)
  t1 <- template

  expect_silent(validate_meta_match(template, template, "geo_type", "blah"))
  expect_silent(validate_meta_match(template, template, "time_type", "blah"))
  attr(t1, "metadata")$geo_type <- "county"
  expect_warning(validate_meta_match(t1, template, "geo_type"), "county")
  expect_snapshot(error = TRUE, validate_meta_match(t1, template, "geo_type", "abort"))


  expect_identical(template, epi_check_training_set(template, rec))
  expect_warning(epi_check_training_set(t1, rec), "county")
  attr(t1, "metadata")$time_type <- "weekly"
  expect_warning(
    expect_warning(epi_check_training_set(t1, rec), "county"),
    "weekly"
  )
  t2 <- template
  attr(t2, "metadata")$other_keys <- "cases"
  expect_silent(epi_check_training_set(t2, rec))
  rec$template <- t2
  t3 <- template
  expect_warning(t4 <- epi_check_training_set(t3, rec))
  expect_identical(rec$template, t4)
  attr(rec$template, "metadata")$other_keys <- "missing_col"
  expect_snapshot(error = TRUE, epi_check_training_set(t4, rec))
})
