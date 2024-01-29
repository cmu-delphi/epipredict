# epipredict (development)

Pre-1.0.0 numbering scheme: 0.x will indicate releases, while 0.0.x will indicate PR's.

# epipredict 0.1

-   add `check_enough_train_data` that will error if training data is too small
-   added `check_enough_train_data` to `arx_forecaster`
-   simplify `layer_residual_quantiles()` to avoid timesuck in `utils::methods()`
-   rename the `dist_quantiles()` to be more descriptive, breaking change)
-   removes previous `pivot_quantiles()` (now `*_wider()`, breaking change)
-   add `pivot_quantiles_wider()` for easier plotting
-   add complement `pivot_quantiles_longer()`
-   add `cdc_baseline_forecaster()` and `flusight_hub_formatter()`
-   add `smooth_quantile_reg()`
-   improved printing of various methods / internals
-   canned forecasters get a class
-   fixed quantile bug in `flatline_forecaster()`
-   add functionality to output the unfit workflow from the canned forecasters
-   add quantile_reg()
-   clean up documentation bugs
-   add smooth_quantile_reg()
-   add classifier
-   training window step debugged
-   `min_train_window` argument removed from canned forecasters
-   add forecasters
-   implement postprocessing
-   vignettes avaliable
-   arx_forecaster
-   pkgdown
-   Publish public for easy navigation
-   Two simple forecasters as test beds
-   Working vignette
