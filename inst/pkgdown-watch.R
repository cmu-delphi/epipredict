# Run with: Rscript pkgdown-watch.R
#
# Modifying this: https://gist.github.com/gadenbuie/d22e149e65591b91419e41ea5b2e0621
# - Removed docopts cli interface and various configs/features I didn't need.
# - Sped up reference building by not running examples.
#
# Note that the `pattern` regex is case sensitive, so make sure your Rmd files
# end in `.Rmd` and not `.rmd`.
#
# Also I had issues with `pkgdown::build_reference()` not working, so I just run
# it manually when I need to.

rlang::check_installed(c("pkgdown", "servr", "devtools", "here", "cli", "fs"))
library(pkgdown)
pkg <- pkgdown::as_pkgdown(here::here())
devtools::build_readme()
pkgdown::build_articles(pkg)
pkgdown::build_site(pkg, lazy = FALSE, examples = FALSE, devel = TRUE, preview = FALSE)

servr::httw(
  dir = here::here("docs"),
  watch = here::here(),
  pattern = "[.](Rm?d|y?ml|s[ac]ss|css|js)$",
  handler = function(files) {
    devtools::load_all()

    files_rel <- fs::path_rel(files, start = getwd())
    cli::cli_inform("{cli::col_yellow('Updated')} {.val {files_rel}}")

    articles <- grep("vignettes.+Rmd$", files, value = TRUE)

    if (length(articles) == 1) {
      name <- fs::path_ext_remove(fs::path_rel(articles, fs::path(pkg$src_path, "vignettes")))
      pkgdown::build_article(name, pkg)
    } else if (length(articles) > 1) {
      pkgdown::build_articles(pkg, preview = FALSE)
    }

    refs <- grep("man.+R(m?d)?$", files, value = TRUE)
    if (length(refs)) {
      # Doesn't work for me, so I run it manually.
      # pkgdown::build_reference(pkg, preview = FALSE, examples = FALSE, lazy = FALSE) # nolint: commented_code_linter
    }

    pkgdown <- grep("pkgdown", files, value = TRUE)
    if (length(pkgdown) && !pkgdown %in% c(articles, refs)) {
      pkgdown::init_site(pkg)
    }

    pkgdown_index <- grep("index[.]Rmd$", files_rel, value = TRUE)
    if (length(pkgdown_index)) {
      devtools::build_rmd(pkgdown_index)
      pkgdown::build_home(pkg)
    }

    readme <- grep("README[.]rmd$", files, value = TRUE, ignore.case = TRUE)
    if (length(readme)) {
      devtools::build_readme()
      pkgdown::build_site(pkg, lazy = TRUE, examples = FALSE, devel = TRUE, preview = FALSE)
    }

    cli::cli_alert("Site rebuild done!")
  }
)
