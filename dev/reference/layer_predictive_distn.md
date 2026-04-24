# Returns predictive distributions

**\[deprecated\]**

## Usage

``` r
layer_predictive_distn(
  frosting,
  ...,
  dist_type = c("gaussian", "student_t"),
  truncate = c(-Inf, Inf),
  name = ".pred_distn",
  id = rand_id("predictive_distn")
)
```

## Arguments

- frosting:

  a `frosting` postprocessor

- ...:

  Unused, include for consistency with other layers.

- dist_type:

  Gaussian or Student's t predictive intervals

- truncate:

  Do we truncate the distribution to an interval

- name:

  character. The name for the output column.

- id:

  a random id string

## Value

an updated `frosting` postprocessor with additional columns of the
residual quantiles added to the prediction

## Details

This function calculates an *approximation* to a parametric predictive
distribution. Predictive distributions from linear models require
`x* (X'X)^{-1} x*` along with the degrees of freedom. This function
approximates both. It should be reasonably accurate for models fit using
`lm` when the new point `x*` isn't too far from the bulk of the data.
Outside of that specific case, it is recommended to use
[`layer_residual_quantiles()`](https://cmu-delphi.github.io/epipredict/dev/reference/layer_residual_quantiles.md),
or if you are working with a model that produces distributional
predictions, use
[`layer_quantile_distn()`](https://cmu-delphi.github.io/epipredict/dev/reference/layer_quantile_distn.md).
