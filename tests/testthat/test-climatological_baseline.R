test_that("epiwindow function works", {
  skip()

  t0 <- ymd("2024-01-08")
  time_value <- ymd(c("2024-01-01", "2023-12-30", "2023-12-01", "2022-01-08"))
  expect_snapshot(error = TRUE, epiwindow(t0, time_value, 1.5))
  expect_snapshot(error = TRUE, epiwindow(t0, time_value, 1, 1.5))
  expect_snapshot(error = TRUE, epiwindow(t0, time_value, seasons = "other"))
  expect_snapshot(error = TRUE, epiwindow(c(t0, t0), time_value))

  expect_identical(epiwindow(t0, time_value), c(TRUE, FALSE, FALSE, TRUE))
  time_value <- ymd(c("2024-01-01", "2023-12-31", "2023-12-01", "2022-01-08"))
  expect_identical(epiwindow(t0, time_value), c(TRUE, TRUE, FALSE, TRUE))
  t0 <- ymd("20231231")
  expect_identical(epiwindow(t0, time_value), c(TRUE, TRUE, FALSE, TRUE))
  expect_identical(
    epiwindow(t0, time_value, seasons = "current"),
    c(TRUE, TRUE, FALSE, FALSE)
  )

  # 2025 has 53 epiweeks in the year
  t0 <- ymd("2026-01-08") # week 1
  # second is week 53
  time_value <- ymd(c("2026-01-01", "2025-12-27", "2025-12-01", "2022-01-08"))
  expect_identical(epiwindow(t0, time_value), c(TRUE, FALSE, FALSE, TRUE))
  # second is week 52
  time_value <- ymd(c("2025-01-01", "2024-12-30", "2024-12-01", "2022-01-08"))
  expect_identical(epiwindow(t0, time_value), c(TRUE, TRUE, FALSE, TRUE))
  t0 <- ymd("20251231")
  expect_identical(epiwindow(t0, time_value), c(TRUE, TRUE, FALSE, TRUE))
  expect_identical(
    epiwindow(t0, time_value, seasons = "current"),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})
