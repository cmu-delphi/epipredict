# Direct autoregressive classifier with covariates

This is an autoregressive classification model for continuous data. It
does "direct" forecasting, meaning that it estimates a class at a
particular target horizon.

## Usage

``` r
arx_classifier(
  epi_data,
  outcome,
  predictors,
  trainer = logistic_reg(),
  args_list = arx_class_args_list()
)
```

## Arguments

- epi_data:

  An `epi_df` object

- outcome:

  A character (scalar) specifying the outcome (in the `epi_df`). Note
  that as with
  [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_forecaster.md),
  this is expected to be real-valued. Conversion of this data to
  unordered classes is handled internally based on the `breaks` argument
  to
  [`arx_class_args_list()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_class_args_list.md).
  If discrete classes are already in the `epi_df`, it is recommended to
  code up a classifier from scratch using
  [`epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/epi_recipe.md).

- predictors:

  A character vector giving column(s) of predictor variables. This
  defaults to the `outcome`. However, if manually specified, only those
  variables specifically mentioned will be used, and the `outcome` will
  not be added.

- trainer:

  A `{parsnip}` model describing the type of estimation. For now, we
  enforce `mode = "classification"`. Typical values are
  [`parsnip::logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.html)
  or
  [`parsnip::multinom_reg()`](https://parsnip.tidymodels.org/reference/multinom_reg.html).
  More complicated trainers like
  [`parsnip::naive_Bayes()`](https://parsnip.tidymodels.org/reference/naive_Bayes.html)
  or
  [`parsnip::rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  can also be used.

- args_list:

  A list of customization arguments to determine the type of forecasting
  model. See
  [`arx_class_args_list()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_class_args_list.md).

## Value

A list with (1) `predictions` an `epi_df` of predicted classes and (2)
`epi_workflow`, a list that encapsulates the entire estimation workflow

## Details

The `arx_classifier()` is an autoregressive classification model for
`epi_df` data that is used to predict a discrete class for each case
under consideration. It is a direct forecaster in that it estimates the
classes at a specific horizon or ahead value.

To get a sense of how the `arx_classifier()` works, let's consider a
simple example with minimal inputs. For this, we will use the built-in
`covid_case_death_rates` that contains confirmed COVID-19 cases and
deaths from JHU CSSE for all states over Dec 31, 2020 to Dec 31, 2021.
From this, we'll take a subset of data for five states over June 4, 2021
to December 31, 2021. Our objective is to predict whether the case rates
are increasing when considering the 0, 7 and 14 day case rates:

    jhu <- covid_case_death_rates %>%
      filter(
        time_value >= "2021-06-04",
        time_value <= "2021-12-31",
        geo_value %in% c("ca", "fl", "tx", "ny", "nj")
      )

    out <- arx_classifier(jhu, outcome = "case_rate", predictors = "case_rate")

    out$predictions
    #> # A tibble: 5 x 4
    #>   geo_value .pred_class forecast_date target_date
    #>   <chr>     <fct>       <date>        <date>
    #> 1 ca        (-Inf,0.25] 2021-12-31    2022-01-07
    #> 2 fl        (-Inf,0.25] 2021-12-31    2022-01-07
    #> 3 nj        (-Inf,0.25] 2021-12-31    2022-01-07
    #> 4 ny        (-Inf,0.25] 2021-12-31    2022-01-07
    #> 5 tx        (-Inf,0.25] 2021-12-31    2022-01-07

The key takeaway from the predictions is that there are two prediction
classes: `(-Inf, 0.25]` and `(0.25, Inf)`: the classes to predict must
be discrete. The discretization of the real-valued outcome is controlled
by the `breaks` argument, which defaults to `0.25`. Such breaks will be
automatically extended to cover the entire real line. For example, the
default break of `0.25` is silently extended to
`breaks = c(-Inf, .25, Inf)` and, therefore, results in two classes:
`[-Inf, 0.25]` and `(0.25, Inf)`. These two classes are used to
discretize the outcome. The conversion of the outcome to such classes is
handled internally. So if discrete classes already exist for the outcome
in the `epi_df`, then we recommend to code a classifier from scratch
using the `epi_workflow` framework for more control.

The `trainer` is a `parsnip` model describing the type of estimation
such that `mode = "classification"` is enforced. The two typical
trainers that are used are
[`parsnip::logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.html)
for two classes or
[`parsnip::multinom_reg()`](https://parsnip.tidymodels.org/reference/multinom_reg.html)
for more than two classes.

    workflows::extract_spec_parsnip(out$epi_workflow)
    #> Logistic Regression Model Specification (classification)
    #>
    #> Computational engine: glm

From the parsnip model specification, we can see that the trainer used
is logistic regression, which is expected for our binary outcome. More
complicated trainers like
[`parsnip::naive_Bayes()`](https://parsnip.tidymodels.org/reference/naive_Bayes.html)
or
[`parsnip::rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
may also be used (however, we will stick to the basics in this gentle
introduction to the classifier).

If you use the default trainer of logistic regression for binary
classification and you decide against using the default break of 0.25,
then you should only input one break so that there are two
classification bins to properly dichotomize the outcome. For example,
let's set a break of 0.5 instead of relying on the default of 0.25. We
can do this by passing 0.5 to the `breaks` argument in
[`arx_class_args_list()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_class_args_list.md)
as follows:

    out_break_0.5 <- arx_classifier(
      jhu,
      outcome = "case_rate",
      predictors = "case_rate",
      args_list = arx_class_args_list(
        breaks = 0.5
      )
    )
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    out_break_0.5$predictions
    #> # A tibble: 5 x 4
    #>   geo_value .pred_class forecast_date target_date
    #>   <chr>     <fct>       <date>        <date>
    #> 1 ca        (-Inf,0.5]  2021-12-31    2022-01-07
    #> 2 fl        (-Inf,0.5]  2021-12-31    2022-01-07
    #> 3 nj        (-Inf,0.5]  2021-12-31    2022-01-07
    #> 4 ny        (-Inf,0.5]  2021-12-31    2022-01-07
    #> 5 tx        (-Inf,0.5]  2021-12-31    2022-01-07

Indeed, we can observe that the two `.pred_class` are now (-Inf, 0.5\]
and (0.5, Inf). See
[`help(arx_class_args_list)`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_class_args_list.md)
for other available modifications.

Additional arguments that may be supplied to
[`arx_class_args_list()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_class_args_list.md)
include the expected `lags` and `ahead` arguments for an
autoregressive-type model. These have default values of 0, 7, and 14
days for the lags of the predictors and 7 days ahead of the forecast
date for predicting the outcome. There is also `n_training` to indicate
the upper bound for the number of training rows per key. If you would
like some practice with using this, then remove the filtering command to
obtain data within "2021-06-04" and "2021-12-31" and instead set
`n_training` to be the number of days between these two dates, inclusive
of the end points. The end results should be the same. In addition to
`n_training`, there are `forecast_date` and `target_date` to specify the
date that the forecast is created and intended, respectively. We will
not dwell on such arguments here as they are not unique to this
classifier or absolutely essential to understanding how it operates. The
remaining arguments will be discussed organically, as they are needed to
serve our purposes. For information on any remaining arguments that are
not discussed here, please see the function documentation for a complete
list and their definitions.

## See also

[`arx_class_epi_workflow()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_class_epi_workflow.md),
[`arx_class_args_list()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_class_args_list.md)

## Examples

``` r
tiny_geos <- c("as", "mp", "vi", "gu", "pr")
jhu <- covid_case_death_rates %>%
  filter(time_value >= as.Date("2021-11-01"), !(geo_value %in% tiny_geos))

out <- arx_classifier(jhu, "death_rate", c("case_rate", "death_rate"))
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

out <- arx_classifier(
  jhu,
  "death_rate",
  c("case_rate", "death_rate"),
  trainer = parsnip::multinom_reg(),
  args_list = arx_class_args_list(
    breaks = c(-.05, .1), ahead = 14,
    horizon = 14, method = "linear_reg"
  )
)
```
