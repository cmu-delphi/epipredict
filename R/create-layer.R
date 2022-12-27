
#' Create a new layer
#'
#' This function creates the skeleton for a new `frosting` layer. When called
#' inside a package, it will create an R script in the `R/` directory,
#' fill in the name of the layer, and open the file.
#'
#' @inheritParams usethis::use_test
#'
#' @importFrom rlang `%||%`
#' @export
#'
#' @examples
#' \dontrun{
#'
#'   # Note: running this will write `layer_strawberry.R` to
#'   # the `R/` directory of your current project
#'   create_layer("strawberry")
#' }
#'
create_layer <- function(name = NULL, open = rlang::is_interactive()) {
  name <- name %||% usethis:::get_active_r_file(path = "R")
  layer_name <- name
  name <- paste0("layer_", name)
  name <- usethis:::slug(name, "R")
  usethis:::check_file_name(name)
  path <- fs::path("R", name)
  if (!fs::file_exists(path)) {
    usethis::use_template(
      "layer.R", save_as = path,
      data = list(name = layer_name), open = FALSE,
      package = "epipredict"
    )
  }
  usethis::edit_file(usethis::proj_path(path), open = open)
}
