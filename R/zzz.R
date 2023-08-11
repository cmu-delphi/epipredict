# ON LOAD ----

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  make_flatline_reg()
  make_quantile_reg()
  make_smooth_quantile_reg()
}
