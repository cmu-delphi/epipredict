# epipredict (development)

# epipredict 0.0.7.9000

-   add `check_enough_train_data` that will error if training data is too small
-   added `check_enough_train_data` to `arx_forecaster`

# epipredict 0.0.7

-   simplify `layer_residual_quantiles()` to avoid timesuck in `utils::methods()`

# epipredict 0.0.6

-   rename the `dist_quantiles()` to be more descriptive, breaking change)
-   removes previous `pivot_quantiles()` (now `*_wider()`, breaking change)
-   add `pivot_quantiles_wider()` for easier plotting
-   add complement `pivot_quantiles_longer()`
-   add `cdc_baseline_forecaster()` and `flusight_hub_formatter()`

# epipredict 0.0.5

-   add `smooth_quantile_reg()`
-   improved printing of various methods / internals
-   canned forecasters get a class
-   fixed quantile bug in `flatline_forecaster()`
-   add functionality to output the unfit workflow from the canned forecasters

# epipredict 0.0.4

-   add quantile_reg()
-   clean up documentation bugs
-   add smooth_quantile_reg()
-   add classifier
-   training window step debugged
-   `min_train_window` argument removed from canned forecasters

# epipredict 0.0.3

-   add forecasters
-   implement postprocessing
-   vignettes avaliable
-   arx_forecaster
-   pkgdown

# epipredict 0.0.0.9000

-   Publish public for easy navigation
-   Two simple forecasters as test beds
-   Working vignette
