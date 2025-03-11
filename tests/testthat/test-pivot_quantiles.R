test_that("quantile pivotting wider behaves", {
  tib <- tibble::tibble(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, pivot_quantiles_wider(tib, a))

  d1 <- quantile_pred(rbind(1:3, 2:4), 1:3 / 4)
  d2 <- quantile_pred(rbind(2:4, 3:5), 2:4 / 5)
  tib <- tibble(g = c("a", "b"), d1 = d1, d2 = d2)

  # too many columns
  expect_snapshot(error = TRUE, pivot_quantiles_wider(tib, d1, d2))
  expect_snapshot(error = TRUE, pivot_quantiles_longer(tib, d1, d2))


  expect_length(pivot_quantiles_wider(tib, d1), 5L)
  expect_length(pivot_quantiles_wider(tib, tidyselect::ends_with("1")), 5L)
  expect_equal(vctrs::vec_size(pivot_quantiles_longer(tib, d2)), 6L)
})

test_that("pivotting wider still works if there are duplicates", {
  # previously this would produce a warning if pivotted because the
  # two rows of the result are identical
  tb <- tibble(.pred = quantile_pred(rbind(1:3, 1:3), c(.1, .5, .9)))
  res <- tibble(`0.1` = c(1, 1), `0.5` = c(2, 2), `0.9` = c(3, 3))
  expect_equal(tb %>% pivot_quantiles_wider(.pred), res)
  res_longer <- tibble(
    .pred_value = rep(1:3, 2),
    .pred_quantile_level = rep(c(0.1, 0.5, 0.9), 2)
  )
  expect_equal(tb %>% pivot_quantiles_longer(.pred), res_longer)
})


test_that("quantile pivotting longer behaves", {
  tib <- tibble::tibble(a = 1:5, b = 6:10)
  expect_snapshot(error = TRUE, pivot_quantiles_longer(tib, a))

  d1 <- quantile_pred(rbind(1:3, 2:4), 1:3 / 4)
  d2 <- quantile_pred(rbind(2:4, 3:5), 2:4 / 5)
  tib <- tibble(g = c("a", "b"), d1 = d1, d2 = d2)

  # too many columns
  expect_snapshot(error = TRUE, pivot_quantiles_longer(tib, d1, d2))

  # different quantiles
  expect_length(pivot_quantiles_longer(tib, d1), 4L)
  expect_identical(nrow(pivot_quantiles_longer(tib, d1)), 6L)
  expect_identical(pivot_quantiles_longer(tib, d1)$d1_value, c(1:3, 2:4))
})

test_that("nested_quantiles is deprecated, but works where possible", {
  expect_snapshot(d <- dist_quantiles(list(1:4, 2:5), 1:4 / 5))
  expect_snapshot(o <- nested_quantiles(d))
  res <- as_tibble(hardhat::quantile_pred(
    matrix(c(1:4, 2:5), nrow = 2, byrow = TRUE), 1:4 / 5
  ))
  expect_identical(o |> mutate(.row = dplyr::row_number()) |> unnest(data), res)
})
