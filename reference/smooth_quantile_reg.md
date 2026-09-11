# Smooth quantile regression

`smooth_quantile_reg()` generates a quantile regression model
*specification* for the [tidymodels](https://www.tidymodels.org/)
framework. Currently, the only supported engine is
[`smoothqr::smooth_qr()`](https://dajmcdon.github.io/smoothqr/,%20https://github.com/dajmcdon/smoothqr/reference/smooth_qr.html).

## Usage

``` r
smooth_quantile_reg(
  mode = "regression",
  engine = "smoothqr",
  outcome_locations = NULL,
  quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
  degree = 3L
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- engine:

  Character string naming the fitting function. Currently, only "rq" and
  "grf" are supported.

- outcome_locations:

  Defaults to the vector `1:ncol(y)` but if the responses are observed
  at a different spacing (or appear in a different order), that
  information should be used here. This argument will be mapped to the
  `ahead` argument of
  [`smoothqr::smooth_qr()`](https://dajmcdon.github.io/smoothqr/,%20https://github.com/dajmcdon/smoothqr/reference/smooth_qr.html).

- quantile_levels:

  A scalar or vector of values in (0, 1) to determine which quantiles to
  estimate (default is the set 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95).

- degree:

  the number of polynomials used for response smoothing. Must be no more
  than the number of responses.

## See also

[`parsnip::fit.model_spec()`](https://generics.r-lib.org/reference/fit.html),
[`parsnip::set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)

## Examples

``` r
library(smoothqr)
#> 
#> Attaching package: ‘smoothqr’
#> The following object is masked from ‘package:epidatasets’:
#> 
#>     covid_case_death_rates
tib <- data.frame(
  y1 = rnorm(100), y2 = rnorm(100), y3 = rnorm(100),
  y4 = rnorm(100), y5 = rnorm(100), y6 = rnorm(100),
  x1 = rnorm(100), x2 = rnorm(100)
)
qr_spec <- smooth_quantile_reg(quantile_levels = c(.2, .5, .8), outcome_locations = 1:6)
ff <- qr_spec %>% fit(cbind(y1, y2, y3, y4, y5, y6) ~ ., data = tib)
p <- predict(ff, new_data = tib)

x <- -99:99 / 100 * 2 * pi
y <- sin(x) + rnorm(length(x), sd = .1)
fd <- x[length(x) - 20]
XY <- smoothqr::lagmat(y[1:(length(y) - 20)], c(-20:20))
XY <- as_tibble(XY)
qr_spec <- smooth_quantile_reg(quantile_levels = c(.2, .5, .8), outcome_locations = 20:1)
tt <- qr_spec %>% fit_xy(x = XY[, 21:41], y = XY[, 1:20])

pl <- predict(
  object = tt,
  new_data = XY[max(which(complete.cases(XY[, 21:41]))), 21:41]
)
pl <- pl %>%
  unnest(.pred) %>%
  pivot_quantiles_wider(distn) %>%
  mutate(
    x = x[length(x) - 20] + ahead / 100 * 2 * pi,
    ahead = NULL
  )
plot(x, y, pch = 16, xlim = c(pi, 2 * pi), col = "lightgrey")
curve(sin(x), add = TRUE)
abline(v = fd, lty = 2)
lines(pl$x, pl$`0.2`, col = "blue")
lines(pl$x, pl$`0.8`, col = "blue")
lines(pl$x, pl$`0.5`, col = "red")


library(ggplot2)
ggplot(data.frame(x = x, y = y), aes(x)) +
  geom_ribbon(data = pl, aes(ymin = `0.2`, ymax = `0.8`), fill = "cornflowerblue") +
  geom_point(aes(y = y), colour = "grey") + # observed data
  geom_function(fun = sin, colour = "black") + # truth
  geom_vline(xintercept = fd, linetype = "dashed") + # end of training data
  geom_line(data = pl, aes(y = `0.5`), colour = "orange") + # median prediction
  theme_bw() +
  coord_cartesian(xlim = c(0, NA)) +
  ylab("y")
```
