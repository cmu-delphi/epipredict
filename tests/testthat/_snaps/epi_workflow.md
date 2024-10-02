# model can be added/updated/removed from epi_workflow

    Code
      extract_spec_parsnip(wf)
    Condition
      Error in `extract_spec_parsnip()`:
      ! The workflow does not have a model spec.

# forecast method errors when workflow not fit

    Code
      forecast(wf)
    Condition
      Error in `forecast()`:
      ! You cannot `forecast()` a <workflow> that has not been trained.
      i Please use `fit()` before forecasting.

