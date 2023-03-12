library(epipredict)
library(ggplot2)
library(dplyr)
library(tidyr)
library(recipes)
library(parsnip)
library(workflows)
dat <- case_death_rate_subset %>% # 1 year of daily data, 56 locations
  filter(time_value >= "2021-11-01", geo_value %in% c("ca", "ny", "pa"))

dat_no_pa <- dat2 %>%
  group_by(is_pa = geo_value == "pa") %>%
  group_modify(function(gdf, gk) {
    if (gk$is_pa) {
      filter(gdf, .data$time_value <= max(.data$time_value) - 2L)
    } else {
      gdf
    }
  }) %>%
  ungroup() %>%
  select(-is_pa)

r <- epi_recipe(dat) %>%
  step_epi_lag(case_rate, lag = c(0, 1, 2, 3, 7, 14)) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 14) %>%
  recipes::step_naomit(all_predictors(), skip = FALSE) %>%
  recipes::step_naomit(all_outcomes(), skip = TRUE)

train_data_full <- r %>% prep() %>% bake(dat)
train_data_no_pa <- r %>% prep() %>% bake(dat_no_pa)

dim(train_data_full)
dim(train_data_no_pa)
sum(complete.cases(train_data_full)) # most recent data has NAs in the outcome
sum(complete.cases(train_data_no_pa))

test_full <- get_test_data(r, dat)
test_no_pa <- get_test_data(r, dat_no_pa)

# the test data that lm gets to see, eventually
(baked_test_full <- r %>% prep() %>% bake(test_full))
(baked_test_no_pa <- r %>% prep() %>% bake(test_no_pa))


range(test_full %>% filter(geo_value == "pa") %>% pull(time_value))
range(test_no_pa %>% filter(geo_value == "pa") %>% pull(time_value))

ewf <- epi_workflow(r, linear_reg())
fit_full <- ewf %>% fit(dat)
fit_no_pa <- ewf %>% fit(dat_no_pa)

mod_full <- workflows::extract_fit_engine(fit_full) # the lm object
mod_no_pa <- workflows::extract_fit_engine(fit_no_pa)

# using the lm, and predict.lm
p_full <- predict(mod_full, newdata = baked_test_full) # order is ca, ny, pa
p_no_pa <- predict(mod_no_pa, newdata = baked_test_no_pa) # order is pa, ca, ny

# using the workflow, these match the "by hand" versions above (no frosting)
predict(fit_full, new_data = test_full)
predict(fit_no_pa, new_data = test_no_pa)

tibble(
  n = names(coef(mod_full)),
  full = coef(mod_full),
  no_pa = coef(mod_no_pa)) %>%
  pivot_longer(-n) %>%
  ggplot(aes(n, value, colour = name)) + geom_point() + theme_bw() +
  coord_flip()
