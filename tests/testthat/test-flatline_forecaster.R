test_that("flatline_forecaster returns one prediction per geo with trailing NAs (issue #454)", {
  jhu <- epidatasets::covid_case_death_rates %>%
    dplyr::filter(time_value >= as.Date("2021-11-01"))
  geos <- unique(jhu$geo_value)
  one_day <- tibble::tibble(
    geo_value = geos,
    time_value = as.Date("2022-01-01"),
    case_rate = NA_real_,
    death_rate = 0
  )
  two_day <- one_day %>% dplyr::mutate(time_value = as.Date("2022-01-02"))
  jhu_nad <- jhu %>%
    tibble::as_tibble() %>%
    dplyr::bind_rows(one_day, two_day) %>%
    epiprocess::as_epi_df()

  res <- flatline_forecaster(jhu_nad, "case_rate")

  expect_equal(nrow(res$predictions), length(geos))
  counts <- res$predictions %>% dplyr::count(geo_value, target_date)
  expect_true(all(counts$n == 1L))
})

test_that("flatline_forecaster errors on invalid quantile_by_key columns (issue #229)", {
  jhu <- epidatasets::covid_case_death_rates
  expect_error(
    flatline_forecaster(jhu, "death_rate",
      flatline_args_list(quantile_by_key = "nonexistent_column")
    ),
    class = "epipredict__flatline_forecaster__quantile_by_key_invalid"
  )
})
