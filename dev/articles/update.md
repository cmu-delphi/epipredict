# Using the add/update/remove and adjust functions

``` r

library(epipredict)
library(recipes)
library(dplyr)
library(workflows)
library(parsnip)
```

In this vignette, we will state the main goal of the add/update/remove
and adjust functions and describe what part of the processing each
function is intended for. We will then demonstrate how to use the sets
of add/update/remove functions, followed by the adjust functions, and
end with a brief discussion on the tidy methods for recipe and frosting
objects.

## Main goal of the add/update/remove and adjust functions

The primary goal of the update and adjust functions is to allow the user
to modify a `step`, `layer`, `epi_recipe`, `frosting`, or a part of an
`epi_workflow` so that they do not have to create a new object each time
they wish to make a change to the pre-processing, fitting, or
post-processing.

In the context of pre-processing, the goal of the update functions is to
add/remove/update an `epi_recipe` or a step in it. For this, we have
[`add_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md),
[`update_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md),
and
[`remove_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md)
to add/update/remove an entire `epi_recipe` in an `epi_workflow` as well
as
[`adjust_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/adjust_epi_recipe.md)
to adjust a particular step in an `epi_recipe` or `epi_workflow` by the
step number or name. For a model, one may
[`Add_model()`](https://cmu-delphi.github.io/epipredict/dev/reference/Add_model.md),
[`Update_model()`](https://cmu-delphi.github.io/epipredict/dev/reference/Add_model.md),
or
[`Remove_model()`](https://cmu-delphi.github.io/epipredict/dev/reference/Add_model.md)
in an `epi_workflow`.[^1] For post-processing, where the goal is to
update a frosting object or a layer in it, we have
[`add_frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_frosting.md),
[`remove_frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_frosting.md),
and
[`update_frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_frosting.md)
to add/update/remove an entire `frosting` object in an `epi_workflow` as
well as
[`adjust_frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/adjust_frosting.md)
to adjust a particular layer in a `frosting` or `epi_workflow` by its
number or name. A summary of the function uses by processing step is
shown by the following table:

|  | Add/update/remove functions | adjust functions |
|----|----|----|
| Pre-processing | [`add_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md), [`update_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md), [`remove_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md) | [`adjust_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/adjust_epi_recipe.md) |
| Model specification | [`Add_model()`](https://cmu-delphi.github.io/epipredict/dev/reference/Add_model.md), [`Update_model()`](https://cmu-delphi.github.io/epipredict/dev/reference/Add_model.md) [`Remove_model()`](https://cmu-delphi.github.io/epipredict/dev/reference/Add_model.md) |  |
| Post-processing | [`add_frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_frosting.md), [`remove_frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_frosting.md), [`update_frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_frosting.md) | [`adjust_frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/adjust_frosting.md) |

Since adding/removing/updating frosting as well as adjusting a layer in
a `frosting` object proceeds in the same way as performing those tasks
on an `epi_recipe`, we will focus on implementing those for an
`epi_recipe` in this vignette and only briefly go through some examples
for a `frosting` object.

## Add/update/remove an `epi_recipe` in an `epi_workflow`

We start with the built-in `covid_case_death_rates` dataset that
contains JHU daily COVID-19 cases and deaths by state and take a subset
of it from Nov. 1, 2021 to Dec. 31, 2021 for the four states of Alaska,
California, New York, and South Carolina.

``` r

jhu <- covid_case_death_rates %>%
  filter(time_value >= as.Date("2021-11-01"), geo_value %in% c("ak", "ca", "ny", "sc"))

jhu
#> An `epi_df` object, 244 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 434 days
#> 
#> # A tibble: 244 × 4
#>   geo_value time_value case_rate death_rate
#>   <chr>     <date>         <dbl>      <dbl>
#> 1 ak        2021-11-01      87.9      0.494
#> 2 ca        2021-11-01      15.6      0.241
#> 3 ny        2021-11-01      19.9      0.177
#> 4 sc        2021-11-01      16.0      0.531
#> 5 ak        2021-11-02      83.2      0.395
#> 6 ca        2021-11-02      15.4      0.200
#> # ℹ 238 more rows
```

Then, we construct a simple `epi_recipe` object named `r`, where we lag
the death rates by 0, 7, and 14 days, lead the death rate by 14 days,
omit NA values in all predictors and then in all outcomes (and set
`skip = TRUE` to skip over this processing of the outcome variable when
the recipe is baked).

``` r

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 14) %>%
  step_naomit(all_predictors()) %>%
  step_naomit(all_outcomes(), skip = TRUE)
```

We add this recipe to an `epi_workflow` object by inputting `r` into the
[`add_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md)
function:

``` r

wf <- epi_workflow() %>%
  add_epi_recipe(r)

wf
#> 
#> ══ Epi Workflow ═════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: None
#> Postprocessor: None
#> 
#> ── Preprocessor ─────────────────────────────────────────────────────────────
#> 
#> 4 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_ahead()
#> 3. step_naomit()
#> 4. step_naomit()
#> 
```

We may then go on to add the fitted linear model to our `epi_workflow`:

``` r

# Fit a linear model
wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)

wf
#> 
#> ══ Epi Workflow [trained] ═══════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> Postprocessor: None
#> 
#> ── Preprocessor ─────────────────────────────────────────────────────────────
#> 
#> 4 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_ahead()
#> 3. step_naomit()
#> 4. step_naomit()
#> 
#> ── Model ────────────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#>       (Intercept)   lag_0_death_rate   lag_7_death_rate  lag_14_death_rate  
#>           0.43314           -0.75303            0.02903            0.09068
#> 
```

At this stage, suppose we decide to overhaul our recipe so that we have
a different set of pre-processing steps or we want to make multiple
changes to existing steps, but we desire to keep the remainder of the
`epi_workflow` the same. We can use the
[`update_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md)
function to trade our current recipe `r` for another recipe `r2` in `wf`
as follows:

``` r

r2 <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 1, 7, 14)) %>%
  step_epi_lag(case_rate, lag = c(0:7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

wf <- update_epi_recipe(wf, r2)
wf
#> 
#> ══ Epi Workflow ═════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> Postprocessor: None
#> 
#> ── Preprocessor ─────────────────────────────────────────────────────────────
#> 
#> 5 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_lag()
#> 3. step_epi_ahead()
#> 4. step_naomit()
#> 5. step_naomit()
#> 
#> ── Model ────────────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#>       (Intercept)   lag_0_death_rate   lag_7_death_rate  lag_14_death_rate  
#>           0.43314           -0.75303            0.02903            0.09068
#> 
```

You can see that the output of `wf` depicts the sequence of steps in
`r2` instead of `r`, which indicates that the update was successful.

A longer approach to achieve the same end is to use
[`remove_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md)
to remove the old recipe and then
[`add_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md)
to add the new one. Under the hood, the
[`update_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md)
function operates in this way.

The
[`add_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md)
and
[`remove_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md)
functions offload to the
[workflows](https://github.com/tidymodels/workflows) versions of the
functions as much as possible. The main reason for using the
[epipredict](https://github.com/cmu-delphi/epipredict/) version is so
that we ensure that we retain the `epi_workflow` class.

To see this, let’s look at what happens if we remove our current
`epi_recipe` using
[`workflows::remove_recipe()`](https://workflows.tidymodels.org/reference/add_recipe.html)
and then inspect the class of `wf`:

``` r

wf %>% class() # class before
#> [1] "epi_workflow" "workflow"
remove_recipe(wf) %>% class() # class after removing recipe using workflows function
#> [1] "workflow"
```

We can observe that `wf` is no longer an `epi_workflow` and a
`workflow`. It has been demoted to only a `workflow`. While all
`epi_workflow`s are `workflow`s, not all `workflow`s are
`epi_workflow`s, meaning that there may be compatibility issues and
limitations to the tools that may be used from the
[epipredict](https://github.com/cmu-delphi/epipredict/) package on a
plain `workflow` object.

Now, while we checked what happens to the above `epi_recipe` if we
remove it, note that we did not actually store that change to `wf`.
Hence, our `epi_workflow` remains unchanged.

``` r

wf
#> 
#> ══ Epi Workflow ═════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> Postprocessor: None
#> 
#> ── Preprocessor ─────────────────────────────────────────────────────────────
#> 
#> 5 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_lag()
#> 3. step_epi_ahead()
#> 4. step_naomit()
#> 5. step_naomit()
#> 
#> ── Model ────────────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#>       (Intercept)   lag_0_death_rate   lag_7_death_rate  lag_14_death_rate  
#>           0.43314           -0.75303            0.02903            0.09068
#> 
```

One thing to notice about this workflow output is that is that the model
fit remains the same as when we had `r` as the recipe. This illustrates
an important point - Any operations performed using the old recipe are
not updated automatically. So we should be careful to fit the model
using the new recipe, `r2`. Similarly, if predictions were made using
the old recipe, then they should be re-generated using the version
`epi_workflow` that contains the updated recipe. We can use
[`Update_model()`](https://cmu-delphi.github.io/epipredict/dev/reference/Add_model.md)
to replace the model used in `wf`, and then fit as before:

``` r

# fit linear model
wf <- Update_model(wf, linear_reg()) %>% fit(jhu)
wf
#> 
#> ══ Epi Workflow [trained] ═══════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> Postprocessor: None
#> 
#> ── Preprocessor ─────────────────────────────────────────────────────────────
#> 
#> 5 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_lag()
#> 3. step_epi_ahead()
#> 4. step_naomit()
#> 5. step_naomit()
#> 
#> ── Model ────────────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#>       (Intercept)   lag_0_death_rate   lag_1_death_rate   lag_7_death_rate  
#>          0.290875          -0.156658           0.054776          -0.348165  
#> lag_14_death_rate    lag_0_case_rate    lag_1_case_rate    lag_2_case_rate  
#>         -0.303906           0.009033          -0.009466          -0.005362  
#>   lag_3_case_rate    lag_4_case_rate    lag_5_case_rate    lag_6_case_rate  
#>         -0.006236           0.004849           0.005536          -0.013340  
#>   lag_7_case_rate   lag_14_case_rate  
#>          0.011320           0.011674
#> 
```

Alternatively, we may use the
[`Remove_model()`](https://cmu-delphi.github.io/epipredict/dev/reference/Add_model.md)
followed by
[`Add_model()`](https://cmu-delphi.github.io/epipredict/dev/reference/Add_model.md)
combination for the same effect.

## Add/update/remove a `frosting` object in an `epi_workflow`

We will now generate and create a `frosting` object for post-processing
predictions. In our initial frosting object, `f`, we simply implement
predictions on the fitted `epi_workflow`:

``` r

f <- frosting() %>%
  layer_predict()

wf1 <- wf %>% add_frosting(f)
p1 <- forecast(wf1)
p1
#> An `epi_df` object, 4 x 3 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 4 × 3
#>   geo_value time_value   .pred
#>   <chr>     <date>       <dbl>
#> 1 ak        2021-12-31 -0.206 
#> 2 ca        2021-12-31  0.0992
#> 3 ny        2021-12-31  0.295 
#> 4 sc        2021-12-31  0.351
```

Suppose we decide to augment our post-processing to include a threshold
to enforce that the predictions are at least 0. As well, let’s include
the forecast and target dates as separate columns.

To update the `frosting` while leaving the remainder of the
`epi_workflow` the same, we can use the
[`update_frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_frosting.md)
function as follows:

``` r

# Update frosting in a workflow and predict
f2 <- frosting() %>%
  layer_predict() %>%
  layer_threshold(.pred) %>%
  layer_add_forecast_date() %>%
  layer_add_target_date()

wf2 <- wf1 %>% update_frosting(f2)
p2 <- forecast(wf2)
p2
#> An `epi_df` object, 4 x 5 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 4 × 5
#>   geo_value time_value  .pred forecast_date target_date
#>   <chr>     <date>      <dbl> <date>        <date>     
#> 1 ak        2021-12-31 0      2021-12-31    2022-01-07 
#> 2 ca        2021-12-31 0.0992 2021-12-31    2022-01-07 
#> 3 ny        2021-12-31 0.295  2021-12-31    2022-01-07 
#> 4 sc        2021-12-31 0.351  2021-12-31    2022-01-07
```

Internally, this works by removing the old frosting followed by adding
the new frosting, just like when we update a recipe or model.

``` r

update_frosting
#> function (x, frosting, ...) 
#> {
#>     rlang::check_dots_empty()
#>     x <- remove_frosting(x)
#>     add_frosting(x, frosting)
#> }
#> <bytecode: 0x5615a4235b50>
#> <environment: namespace:epipredict>
```

If we decide that we do not want the `frosting` post-processing at all,
we can remove the `frosting` object from the workflow and make
predictions as follows:

``` r

wf3 <- wf2 %>% remove_frosting()
p3 <- forecast(wf3)
p3
#> An `epi_df` object, 4 x 3 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 4 × 3
#>   geo_value time_value   .pred
#>   <chr>     <date>       <dbl>
#> 1 ak        2021-12-31 -0.206 
#> 2 ca        2021-12-31  0.0992
#> 3 ny        2021-12-31  0.295 
#> 4 sc        2021-12-31  0.351
```

You can see that the above results from `p3` are the same as from `p1`,
when we simply have a prediction layer in the `frosting` post-processing
container.

## Adjust a single step of an `epi_recipe`

Suppose that we just want to change a single step in an `epi_recipe`
(that is either standalone or a part of an `epi_workflow`). Instead of
replacing an entire `epi_recipe`, we can use the
[`adjust_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/adjust_epi_recipe.md)
function. In this function, the step to be adjusted is indicated either
the step number or name in the `which_step` parameter. Then, the
parameter name and update value must be inputted as `...`.

For instance, suppose that we decide to lead the `death_rate` by 14 days
instead of 7. We may adjust this step in `wf` recipe by setting
`which_step` to the step number in the order of operations, which can be
obtained by inspecting `r2` or the tidy summary of it:

``` r

extract_preprocessor(wf) # step_epi_ahead is the third step in r2
#> 
#> ── Epi Recipe ───────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> raw:        2
#> geo_value:  1
#> time_value: 1
#> 
#> ── Operations
#> 1. Lagging: death_rate by 0, 1, 7, 14
#> 2. Lagging: case_rate by 0, 1, 2, 3, 4, 5, 6, 7, 14
#> 3. Leading: death_rate by 7
#> 4. • Removing rows with NA values in: all_predictors()
#> 5. • Removing rows with NA values in: all_outcomes()
tidy(extract_preprocessor(wf)) # tidy tibble summary of r2
#> # A tibble: 5 × 6
#>   number operation type      trained skip  id             
#>    <int> <chr>     <chr>     <lgl>   <lgl> <chr>          
#> 1      1 step      epi_lag   FALSE   FALSE epi_lag_9QeuR  
#> 2      2 step      epi_lag   FALSE   FALSE epi_lag_8HfdV  
#> 3      3 step      epi_ahead FALSE   FALSE epi_ahead_HIyvQ
#> 4      4 step      naomit    FALSE   FALSE naomit_vjF0H   
#> 5      5 step      naomit    FALSE   TRUE  naomit_gF0fi

wf <- wf %>% adjust_epi_recipe(which_step = 3, ahead = 14)
```

Alternatively, we may adjust that step by name by specifying the full
name of the step, `step_epi_ahead`, in `which_step`:

``` r

wf %>% adjust_epi_recipe(which_step = "step_epi_ahead", ahead = 14) # not overwrite r2 because same result
#> 
#> ══ Epi Workflow ═════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> Postprocessor: None
#> 
#> ── Preprocessor ─────────────────────────────────────────────────────────────
#> 
#> 5 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_lag()
#> 3. step_epi_ahead()
#> 4. step_naomit()
#> 5. step_naomit()
#> 
#> ── Model ────────────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#>       (Intercept)   lag_0_death_rate   lag_1_death_rate   lag_7_death_rate  
#>          0.290875          -0.156658           0.054776          -0.348165  
#> lag_14_death_rate    lag_0_case_rate    lag_1_case_rate    lag_2_case_rate  
#>         -0.303906           0.009033          -0.009466          -0.005362  
#>   lag_3_case_rate    lag_4_case_rate    lag_5_case_rate    lag_6_case_rate  
#>         -0.006236           0.004849           0.005536          -0.013340  
#>   lag_7_case_rate   lag_14_case_rate  
#>          0.011320           0.011674
#> 
```

If there are at least two steps in a recipe that share the same name,
specifying the name in `which_step` will throw an error as
[`adjust_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/adjust_epi_recipe.md)
is not intended to be used to modify multiple steps at once. The way,
then, to modify a step that has the same name as another is to indicate
what number it is in the ordering of the steps. For example, in `r2`
there are two steps named `step_epi_lag` - the first step where we lag
the death rate, and the second where we lag the case rate. If we want to
modify the lags for the `case_rate` variable, we would specify the step
number of 2 in `which_step`.

``` r

wf <- wf %>% adjust_epi_recipe(which_step = 2, lag = c(0, 1, 7, 14, 21))

extract_preprocessor(wf)
#> 
#> ── Epi Recipe ───────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> raw:        2
#> geo_value:  1
#> time_value: 1
#> 
#> ── Operations
#> 1. Lagging: death_rate by 0, 1, 7, 14
#> 2. Lagging: case_rate by 0, 1, 7, 14, 21
#> 3. Leading: death_rate by 14
#> 4. • Removing rows with NA values in: all_predictors()
#> 5. • Removing rows with NA values in: all_outcomes()
```

We could adjust a recipe directly in the same way as we adjust a recipe
in a workflow. The main difference is that we would not input `wf` as
the first argument to
[`adjust_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/adjust_epi_recipe.md)
but rather `r2`.

``` r

adjust_epi_recipe(r2, which_step = 2, lag = c(0, 1, 7, 14, 21)) # should be same result as above
#> 
#> ── Epi Recipe ───────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> raw:        2
#> geo_value:  1
#> time_value: 1
#> 
#> ── Operations
#> 1. Lagging: death_rate by 0, 1, 7, 14
#> 2. Lagging: case_rate by 0, 1, 7, 14, 21
#> 3. Leading: death_rate by 7
#> 4. • Removing rows with NA values in: all_predictors()
#> 5. • Removing rows with NA values in: all_outcomes()
```

Note that when we adjust the `r2` object directly, we are not adjusting
the recipe in the `epi_workflow`. That is, if we modify a step in `r2`,
the change will not automatically transfer over to `wf`. We would need
to modify the recipe in `wf` directly
([`adjust_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/adjust_epi_recipe.md)
on `wf`) or update the recipe in `wf` with a new `epi_recipe` that has
undergone the adjustment (using
[`update_epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_epi_recipe.md)):

``` r

r2 <- adjust_epi_recipe(r2, which_step = 2, lag = 0:21)

extract_preprocessor(wf)
#> 
#> ── Epi Recipe ───────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> raw:        2
#> geo_value:  1
#> time_value: 1
#> 
#> ── Operations
#> 1. Lagging: death_rate by 0, 1, 7, 14
#> 2. Lagging: case_rate by 0, 1, 7, 14, 21
#> 3. Leading: death_rate by 14
#> 4. • Removing rows with NA values in: all_predictors()
#> 5. • Removing rows with NA values in: all_outcomes()
```

## Adjust a single layer of a `frosting`

Adjusting a layer of a `frosting` object proceeds in the same way as
adjusting a step in an `epi_recipe` does. So if we want to change a
single layer in a `frosting` (that is either in a standalone object or
part of an `epi_workflow`), we can use the
[`adjust_frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/adjust_frosting.md)
function wherein the layer to be adjusted is indicated by either its
number or name in the `which_layer` parameter. In addition, the argument
name and update value must be inputted as `...`.

Let’s work with the frosting object directly instead of working on it
through the `epi_workflow` in a simple, illustrative example. Recall
frosting `f2` which has the following layers:

``` r

f2
#> 
#> ── Frosting ─────────────────────────────────────────────────────────────────
#> 
#> ── Layers
#> 1. Creating predictions: "<calculated>"
#> 2. Thresholding predictions: .pred to [0, Inf)
#> 3. Adding forecast date: "<calculated>"
#> 4. Adding target date: "<calculated>"
```

Suppose that we decide to change the upper bound of the prediction
threshold to 10 instead of `Inf`. We can adjust this layer in frosting
object by setting `which_layer` to the layer number, 3 (which can be
found by inspecting `f2` or `tidy(f2)`):

``` r

f2 <- f2 %>% adjust_frosting(which_layer = 2, upper = 10)

f2
#> 
#> ── Frosting ─────────────────────────────────────────────────────────────────
#> 
#> ── Layers
#> 1. Creating predictions: "<calculated>"
#> 2. Thresholding predictions: .pred to [0, 10]
#> 3. Adding forecast date: "<calculated>"
#> 4. Adding target date: "<calculated>"
```

Alternatively, we may adjust that layer by specifying its full name,
`layer_threshold`, in `which_layer`, to achieve the same result:

``` r

f2 %>% adjust_frosting(which_layer = "layer_threshold", upper = 10) # not overwrite f2 because same result
#> 
#> ── Frosting ─────────────────────────────────────────────────────────────────
#> 
#> ── Layers
#> 1. Creating predictions: "<calculated>"
#> 2. Thresholding predictions: .pred to [0, 10]
#> 3. Adding forecast date: "<calculated>"
#> 4. Adding target date: "<calculated>"
```

## On the tidy method to inspect an `epi_recipe` or a `frosting` object

The tidy method, when used on an `epi_recipe`, will return a data frame
that contains specific overview information about the recipe including
the operation number, the operation class (either “step” or “check”),
the type of method, a boolean value to indicate whether
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) has been
used to estimate the operation, a boolean value to indicate whether the
step is applied when
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) is
called, and the id of the operation.

``` r

tidy(r2)
#> # A tibble: 5 × 6
#>   number operation type      trained skip  id             
#>    <int> <chr>     <chr>     <lgl>   <lgl> <chr>          
#> 1      1 step      epi_lag   FALSE   FALSE epi_lag_9QeuR  
#> 2      2 step      epi_lag   FALSE   FALSE epi_lag_8HfdV  
#> 3      3 step      epi_ahead FALSE   FALSE epi_ahead_HIyvQ
#> 4      4 step      naomit    FALSE   FALSE naomit_vjF0H   
#> 5      5 step      naomit    FALSE   TRUE  naomit_gF0fi
```

In contrast, printing the `epi_recipe` object shows the inputs (number
and roles of the variables) as well as the ordering and a brief written
summary of the operations:

``` r

r2
#> 
#> ── Epi Recipe ───────────────────────────────────────────────────────────────
#> 
#> ── Inputs
#> Number of variables by role
#> raw:        2
#> geo_value:  1
#> time_value: 1
#> 
#> ── Operations
#> 1. Lagging: death_rate by 0, 1, 7, 14
#> 2. Lagging: case_rate by 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,
#> 15,
#> 3. 16, 17, 18,...
#> 4. Leading: death_rate by 7
#> 5. • Removing rows with NA values in: all_predictors()
#> 6. • Removing rows with NA values in: all_outcomes()
```

This same general structure persists when we compare the output of a
frosting object to that of its tidy tibble. However, we no longer have
the output specific to a recipe such as the roles in the recipe output
and the trained and skip columns in tidy tibble for it. Thus, the output
of a frosting object and the tidy tibble are simplified in comparison to
those for an `epi_recipe`.

``` r

f
#> 
#> ── Frosting ─────────────────────────────────────────────────────────────────
#> 
#> ── Layers
#> 1. Creating predictions: "<calculated>"

tidy(f)
#> # A tibble: 1 × 4
#>   number operation type    id                   
#>    <int> <chr>     <chr>   <chr>                
#> 1      1 layer     predict predict_default_qZ3vE
```

[^1]: We capitalize these names to avoid possible clashes with the
    [workflows](https://github.com/tidymodels/workflows) versions of
    these functions. The lower-case versions are also available,
    however, if you load
    [workflows](https://github.com/tidymodels/workflows) after
    [epipredict](https://github.com/cmu-delphi/epipredict/), these will
    be masked and may not work as expected.
