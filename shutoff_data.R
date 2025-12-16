library(tidyverse)


wisc_shutoffs <- read_csv(here::here("data", "Water Residential Customer Data Disconnection Arrears and Tax Roll.csv"))

wisc_meters <- read_csv(here::here("data", "Meters - Totals.csv"))

wisc_shutoffs <- wisc_shutoffs |>
  janitor::clean_names() |>
  select(year, utility_id, name, class, type, ends_with("q3"), ends_with("q4"), added_to_tax_roll, amount_added_to_tax_rol)

wisc_meters <- wisc_meters |>
  janitor::clean_names() |>
  select(year, utility_id, residential)

wisc_shutoffs <- left_join(wisc_shutoffs, wisc_meters) 

wisc_shutoffs <- wisc_shutoffs |>
  mutate(notices_per_meter = disconnection_notices_end_of_q4/residential,
         disconnect_per_meter = disconnections_end_of_q4/residential,
         customers_in_arrears_per_meter = customers_with_arrears_end_of_q4/residential,
         amount_arrears_per_meter = amount_of_arrears_end_of_q4/residential,
         amount_tax_roll_per_meter = amount_added_to_tax_rol/residential,
         arrears_plus_tax_per_meter = amount_arrears_per_meter + amount_tax_roll_per_meter)

summary(wisc_shutoffs)

wisc_shutoffs |>
  filter(year == 2024) |>
  summarise(added = sum(added_to_tax_roll), amount_added = sum(amount_added_to_tax_rol),
            q3_arrearages = sum(amount_of_arrears_end_of_q3),
            q4_arrearages = sum(amount_of_arrears_end_of_q4),
            num_systems = sum(amount_added_to_tax_rol > 0))

wisc_shutoffs_summ <- wisc_shutoffs |>
  filter(year > 2020, residential > 0) |>
  group_by(year, class) |>
  summarise(num = n(),
            notices = sum(disconnection_notices_end_of_q4, na.rm = TRUE),
            disconnects = sum(disconnections_end_of_q4, na.rm = TRUE),
            customers_in_arrears = sum(customers_with_arrears_end_of_q4, na.rm = TRUE),
            amount_of_arrears = sum(amount_of_arrears_end_of_q4, na.rm = TRUE),
            tax_roll = sum(amount_added_to_tax_rol, na.rm = TRUE),
            notices_per_meter = mean(notices_per_meter, na.rm = TRUE),
            disconnect_per_meter = mean(disconnect_per_meter, na.rm = TRUE),
            customers_in_arrears_per_meter = mean(customers_in_arrears_per_meter, na.rm = TRUE),
            amount_arrears_per_meter = mean(amount_arrears_per_meter, na.rm = TRUE),
            amount_tax_roll_per_meter = mean(amount_tax_roll_per_meter, na.rm = TRUE))|>
  ungroup()


ggplot(wisc_shutoffs_summ, aes(x = year, y = notices, group = class, color = class)) +
  geom_line() +
  theme_minimal()

ggplot(wisc_shutoffs_summ, aes(x = year, y = disconnects, group = class, color = class)) +
  geom_line() +
  theme_minimal()

ggplot(wisc_shutoffs_summ, aes(x = year, y = customers_in_arrears, group = class, color = class)) +
  geom_line() +
  theme_minimal()

ggplot(wisc_shutoffs_summ, aes(x = year, y = amount_of_arrears, group = class, color = class)) +
  geom_line() +
  theme_minimal()


ggplot(wisc_shutoffs_summ, aes(x = year, y = notices_per_meter, group = class, color = class)) +
  geom_line() +
  theme_minimal()

ggplot(wisc_shutoffs_summ, aes(x = year, y = disconnect_per_meter, group = class, color = class)) +
  geom_line() +
  theme_minimal()

ggplot(wisc_shutoffs_summ, aes(x = year, y = customers_in_arrears_per_meter, group = class, color = class)) +
  geom_line() +
  theme_minimal()

ggplot(wisc_shutoffs_summ, aes(x = year, y = amount_arrears_per_meter, group = class, color = class)) +
  geom_line() +
  theme_minimal()



wisc_shutoffs_summ_system <- wisc_shutoffs |>
  filter(year > 2020, residential > 0) |>
  group_by(utility_id, name) |>
  summarise(ave_arrears_plus_tax_per_meter = mean(arrears_plus_tax_per_meter, na.rm = TRUE),
            ave_disconnects_per_meter = mean(disconnect_per_meter),
            mean_residential = mean(residential))

summary(wisc_shutoffs_summ_system)

wisc_arrears_top_20 <- wisc_shutoffs |>
  group_by(year) |>
  arrange(desc(arrears_plus_tax_per_meter)) |>
  slice_head(n = 20) |>
  ungroup() |>
  group_by(utility_id) |>
  mutate(num = n()) |>
  ungroup()


