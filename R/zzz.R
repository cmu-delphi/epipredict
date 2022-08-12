# ON LOAD ----

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

# Also create an enviro. for epi_juice that is the same as for juice (so
# can access the funs in recipes) & also make sure the funs in recipes use epi_juice
.onLoad <- function(libname, pkgname) {
  make_flatline_reg()

  environment(epi_juice) <<- asNamespace('recipes')
  utils::assignInNamespace("juice", epi_juice, ns = 'recipes')
}
