library(covidcast)
library(data.table)
library(tidyverse)
library(epiprocess)

theme_set(theme_bw())
y <- covidcast_signals(c("doctor-visits", "jhu-csse"),
                       c("smoothed_adj_cli",
                         "confirmed_7dav_incidence_prop"),
                       start_day = "2020-06-01",
                       end_day = "2021-12-01",
                       issues = c("2020-06-01", "2021-12-01"),
                       geo_type = "state",
                       geo_values = c("ca", "fl"))
x <- y[[1]] %>%
  select(geo_value, time_value,
         version = issue,
         percent_cli = value) %>%
  as_epi_archive()
epix_merge(x, y[[2]] %>%
             select(geo_value, time_value,
                    version = issue,
                    case_rate = value) %>%
             as_epi_archive(),
           all = TRUE)

x_latest <- epix_as_of(x, max_version = max(x$DT$version))
fc_time_values <- seq(as.Date("2020-08-01"),
                      as.Date("2021-12-01"),
                      by = "1 month")


k_week_ahead <- function(x, ahead = 7, as_of = TRUE) {
  if (as_of) {
    x %>%
      epix_slide(fc = arx_forecaster(
        percent_cli, case_rate, geo_value, time_value,
        args = arx_args_list(ahead = ahead)),
        n = 120, ref_time_values = fc_time_values) %>%
      mutate(target_date = time_value + ahead, as_of = TRUE)
  } else {
    x_latest %>%
      epi_slide(fc = arx_forecaster(
        percent_cli, case_rate, geo_value, time_value,
        args = arx_args_list(ahead = ahead)),
        n = 120, ref_time_values = fc_time_values) %>%
      mutate(target_date = time_value + ahead, as_of = FALSE)
  }
}

# Generate the forecasts, and bind them together
fc <- bind_rows(k_week_ahead(x, ahead = 7, as_of = TRUE),
                k_week_ahead(x, ahead = 14, as_of = TRUE),
                k_week_ahead(x, ahead = 21, as_of = TRUE),
                k_week_ahead(x, ahead = 28, as_of = TRUE),
                k_week_ahead(x, ahead = 7, as_of = FALSE),
                k_week_ahead(x, ahead = 14, as_of = FALSE),
                k_week_ahead(x, ahead = 21, as_of = FALSE),
                k_week_ahead(x, ahead = 28, as_of = FALSE))


ggplot(fc, aes(x = target_date, group = time_value, fill = as_of)) +
  geom_line(data = x_latest %>%
              mutate(fc_key_vars = geo_value),
            aes(x = time_value, y = case_rate),
            inherit.aes = FALSE, color = "gray50") +
  geom_ribbon(aes(ymin = fc_q0.05, ymax = fc_q0.95), alpha = 0.4) +
  geom_line(aes(y = fc_point)) + geom_point(aes(y = fc_point), size = 0.5) +
  geom_vline(aes(xintercept = time_value), linetype = 2, alpha = 0.5) +
  facet_grid(vars(fc_key_vars), vars(as_of), scales = "free") +
  scale_x_date(minor_breaks = "month", date_labels = "%b %y") +
  labs(x = "Date", y = "Reported COVID-19 case rates") +
  theme(legend.position = "none")
