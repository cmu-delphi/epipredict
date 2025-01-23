##
# epipredict docs build
#

# knitr doesn't actually clean it's own cache properly; this just deletes any of
# the article knitr caches in vignettes or the base
clean_knitr:
	rm -r *_cache; rm -r vignettes/*_cache
clean_site:
	Rscript -e "pkgdown::clean_cache(); pkgdown::clean_site()"
# this combines 
clean: clean_knitr clean_site

# end
