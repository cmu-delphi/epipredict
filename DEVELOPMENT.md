## Setting up the development environment

```r
install.packages(c('devtools', 'pkgdown', 'styler', 'lintr', 'pak')) # install dev dependencies
pak::pkg_install(".") # install package and dependencies
```

## Validating the package

```r
styler::style_pkg() # format code
lintr::lint_package() # lint code

devtools::check() # run R CMD check, which runs everything below
devtools::document() # generate package meta data and man files
devtools::test() # test package
devtools::build_vignettes() # build vignettes only
devtools::run_examples() # run doc examples
devtools::check(vignettes = FALSE) # check package without vignettes
```

## Developing the documentation site

Our CI builds two versions of the documentation:

- https://cmu-delphi.github.io/epipredict/ from the `main` branch and
- https://cmu-delphi.github.io/epipredict/dev from the `dev` branch.

Commands for developing the documentation site:

```sh
# Basic build and preview
R -e 'pkgdown::clean_site()'
R -e 'devtools::document()'
R -e 'pkgdown::build_site()'
```

Note that sometimes the caches from either `pkgdown` or `knitr` can cause
difficulties. To clear those, run `make`, with either `clean_knitr`,
`clean_site`, or `clean` (which does both).

If you work without R Studio and want to iterate on documentation, you might
find `Rscript pkgdown-watch.R` useful.
helpful. For updating references, you will need to manually call `pkgdown::build_reference()`.

## Versioning

Please follow the guidelines in the [PR template document](.github/pull_request_template.md).

## Planned CRAN release process

Open a release issue and then copy and follow this checklist in the issue (modified from the checklist generated by `usethis::use_release_issue(version = "1.0.2")`):

- [ ] `git pull` on `dev` branch.
- [ ] Make sure all changes are committed and pushed.
- [ ] Check [current CRAN check results](https://cran.rstudio.org/web/checks/check_results_epipredict.html).
- [ ] `devtools::check(".", manual = TRUE, env_vars = c(NOT_CRAN = "false"))`.
  - Aim for 10/10, no notes.
- [ ] If check works well enough, merge to main. Otherwise open a PR to fix up.
- [ ] [Polish NEWS](https://github.com/cmu-delphi/epipredict/blob/dev/NEWS.md).
  - Some [guidelines](https://style.tidyverse.org/news.html#news-release).
- [ ] `git checkout main`
- [ ] `git pull`
- [ ] `urlchecker::url_check()`.
  - This may choke on the MIT license url, and that's ok.
- [ ] `devtools::build_readme()`
- [ ] `devtools::check_win_devel()`
- [ ] Have maintainer ("cre" in description) check email for problems.
- [ ] `revdepcheck::revdep_check(num_workers = 4)`.
  - This may choke, it is very sensitive to the binary versions of packages on a given system. Either bypass or ask someone else to run it if you're concerned.
- [ ] Update `cran-comments.md`
- [ ] PR with any changes (and go through the list again) into `dev` and run through the list again.

Submit to CRAN:

- [ ] `devtools::submit_cran()`.
- [ ] Maintainer approves email.

Wait for CRAN...

- [ ] If accepted :tada:, move to next steps. If rejected, fix and resubmit.
- [ ] Open and merge a PR containing any updates made to `main` back to `dev`.
- [ ] `usethis::use_github_release(publish = FALSE)` (publish off, otherwise it won't push) will create a draft release based on the commit hash in CRAN-SUBMISSION and push a tag to the GitHub repo.
- [ ] Go to the repo, verify the release notes, and publish when ready.
