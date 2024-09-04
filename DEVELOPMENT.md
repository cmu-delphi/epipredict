## Setting up the development environment

```r
install.packages(c('devtools', 'pkgdown', 'styler', 'lintr')) # install dev dependencies
devtools::install_deps(dependencies = TRUE) # install package dependencies
devtools::document() # generate package meta data and man files
devtools::build() # build package
```

## Validating the package

```r
styler::style_pkg() # format code
lintr::lint_package() # lint code

devtools::test() # test package
devtools::check() # check package for errors
```

## Developing the documentation site

The [documentation site](https://cmu-delphi.github.io/epipredict/) is built off of the `main` branch. The `dev` version of the site is available at https://cmu-delphi.github.io/epipredict/dev.

The documentation site can be previewed locally by running in R

```r
pkgdown::build_site(preview=TRUE)
```

The `main` version is available at `file:///<local path>/epidatr/epipredict/index.html` and `dev` at `file:///<local path>/epipredict/docs/dev/index.html`.

You can also build the docs manually and launch the site with python. From the terminal, this looks like

```bash
R -e 'devtools::document()'
R -e 'pkgdown::build_site()'
python -m http.server -d docs
```

## Versioning

Please follow the guidelines in the [PR template document](.github/pull_request_template.md).

## Release process

TBD
