library(dplyr)
library(lubridate)
library(ggplot2)

wisc_rates <- readxl::read_xlsx(here::here("data", "Copy of Copy of General Service Current and Superseded Rates.xlsx"))

wisc_rates <- wisc_rates |>
  janitor::clean_names()

wisc_rates |> 
  group_by(docket_type) |>
  count(sort = TRUE)


wisc_rates <- wisc_rates |>
  mutate(effective_date2 = ymd(effective_date), superseded_date2 = ymd(superseded_date),
         update_time = as.numeric(superseded_date2 - effective_date2),
         time_since = if_else(status == "Current", as.numeric(ymd("2025-10-1")-effective_date2), NA_real_),
         year_of_change = year(effective_date2)
)

wisc_rates_year_data <- wisc_rates |>
  group_by(year_of_change) |>
  summarize(mean_days = mean(update_time, na.rm = TRUE), num_changes = n())


ggplot(wisc_rates_year_data) +
  geom_line(aes(x = year_of_change, y = mean_days/7)) +
  geom_line(aes(x = year_of_change, y = num_changes)) +
  theme_minimal()


## notes, this includes some negative and a bunch of zero days between rate changes
## some of the latter are because of changes in CLASS, not rates