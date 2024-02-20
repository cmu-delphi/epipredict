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
python -m http.server -d docs
```

## Versioning

Please follow the guidelines in the PR template document (reproduced here):

-   [ ] Make sure this PR is against "dev", not "main".
-   [ ] Request a review from one of the current epipredict main reviewers:
        dajmcdon.
-   [ ] Makes sure to bump the version number in `DESCRIPTION` and `NEWS.md`.
        Always increment the patch version number (the third number), unless you are
        making a release PR from dev to main, in which case increment the minor
        version number (the second number).
-   [ ] Describe changes made in NEWS.md, making sure breaking changes
        (backwards-incompatible changes to the documented interface) are noted.
        Collect the changes under the next release number (e.g. if you are on
        0.7.2, then write your changes under the 0.8 heading).

## Release process

TBD
