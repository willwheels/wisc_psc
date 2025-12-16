library(dplyr)
library(ggplot2)
library(tidygeocoder)
library(tidycensus)
library(sf)

madison_2023_rolls <- readxl::read_xlsx(here::here("data", "TaxRoll2023.xlsx"), skip = 3)

## class = A is residential, B is apartments, C is manufacturing, D mostly ag, maybe vacant, PP commercial and downtown, v. confusing
## based on checking properties on https://www.cityofmadison.com/assessor/property/
madison_2023_rolls <- madison_2023_rolls |>
  janitor::clean_names() |>
  select(tax_year, status, class, prop_location, total_assessment, est_fmv_total, delq_utilities, total_due) |>
  filter(class == "A") |>
  mutate(delq_pct_total = delq_utilities/total_due)

summary(madison_2023_rolls)

# madison_2023_rolls_summ <- madison_2023_rolls |>
#   filter(total_due > 0)
  

ggplot(madison_2023_rolls, aes(x = total_due, y = delq_pct_total)) +
  geom_point() +
  theme_minimal()

madison_2023_rolls |>
  group_by(total_assessment) |>
  summarise(sum_delq = sum(delq_utilities), num_properties = n()) |>
  ungroup() |>
  ggplot(aes(x = total_assessment, y = sum_delq, size = num_properties)) +
  geom_point() +
  theme_minimal()

madison_2023_rolls_geocode <- madison_2023_rolls |>
  select(prop_location) |>
  mutate(city = "Madison", state = "WI", 
         address = paste(prop_location, "Madison,", "WI"))


# test_batch <- geocode(madison_2023_rolls_geocode[1:10, ], address = address, ## cf using street, city, state, which doesn't work nearly as well
#                 #mode = "single", #verbose = TRUE,
#                 method = "census", full_results = TRUE, 
#                 api_options = list(census_return_type = "geographies")
# )
# 
# 
# test_single <- geocode(madison_2023_rolls_geocode[1:10, ], address = address,
#                 mode = "single", #verbose = TRUE,
#                 method = "census", full_results = TRUE, 
#                 api_options = list(census_return_type = "geographies")
# )
# 

## probably should just not grab extra
## pass this to a function so I don't

# madison_all_addresses <- purrr::map(1:8, ~ geocode(madison_2023_rolls_geocode[((.x-1)*10000+1):(.x*10000), ], address = address,
#                                                    method = "census", full_results = TRUE, 
#                                                    api_options = list(census_return_type = "geographies"))
# )
# 
# madison_all_addresses <- madison_all_addresses |>
#   bind_rows()
# 
# madison_all_addresses <- madison_all_addresses[1:nrow(madison_2023_rolls_geocode), ]
# 
# save(madison_all_addresses, file = here::here("data", "madison_rolls", "madison_geocoded_addresses_census.Rda"))

load(here::here("data", "madison_rolls", "madison_geocoded_addresses_census.Rda"))

summary(madison_all_addresses)

madison_all_addresses |> 
  group_by(match_indicator) |>
  count()


madison_all_addresses |> 
  group_by(match_type) |>
  count()

madison_all_addresses <- madison_all_addresses |>
  mutate(cbg_geoid = paste0(state_fips, county_fips, census_tract, substr(census_block, 1, 1)))

acs_vars <- load_variables(year = 2023, dataset = "acs5")
acs_vars_cbg <- acs_vars |>
  filter(geography == "block group")


inc_data <- get_acs(geography = "block group", variables = "B19013_001", state = "WI")

madison_2023_rolls_joined <- madison_2023_rolls |>
  left_join(madison_all_addresses |> select(prop_location, cbg_geoid, lat, long), by = "prop_location") |>
  left_join(inc_data |> select(GEOID, NAME, estimate), by = c("cbg_geoid" = "GEOID"))


madison_2023_rolls_joined_summ <- madison_2023_rolls_joined |>
  group_by(cbg_geoid, NAME, estimate) |>
  summarise(sum_deliquent = sum(delq_utilities),
            num_deliquent = sum(delq_utilities > 0),
            num_properties = n()) |>
  ungroup() |>
  mutate(pct_deliquent = num_deliquent/num_properties,
         avg_deliquent = sum_deliquent/num_properties)

ggplot(madison_2023_rolls_joined_summ |> filter(!is.na(estimate)), aes(x = estimate, y = pct_deliquent)) +
  geom_point(aes(size = num_properties)) +
  labs(title = "Percent of Properties with Utility Deliquencies by CBG MHI", subtitle = "Madison, WI") +
  xlab("Median Household Income for CBG") + ylab("Percent of Properties with Deliquencies on Tax Rolls") +
  theme_minimal()

ggsave(here::here("pct_property_w_delinquencies.png"),
       bg = "white")

ggplot(madison_2023_rolls_joined_summ |> filter(!is.na(estimate)), aes(x = estimate, y = sum_deliquent, size = num_properties)) +
  geom_point(aes(size = num_properties)) +
  labs(title = "Total Utility Deliquencies by CBG MHI", subtitle = "Madison, WI") +
  xlab("Median Household Income for CBG") + ylab("Total Deliquencies on Tax Rolls") +
  theme_minimal()

ggplot(madison_2023_rolls_joined_summ |> filter(!is.na(estimate)), aes(x = estimate, y = avg_deliquent)) +
  geom_point(aes(size = num_properties)) +
  #geom_smooth(method = "lm") +
  labs(title = "Average Utility Deliquencies by CBG MHI", subtitle = "Madison, WI") +
  xlab("Median Household Income for CBG") + ylab("Average Deliquencies on Tax Rolls") +
  theme_minimal()

ggsave(here::here("avg_delinquencies.png"),
       bg = "white")



options(tigris_use_cache = TRUE)
wi_block_shapes <- tigris::block_groups(state = "WI", year = "2023")

wi_block_shapes <- wi_block_shapes |>
  select(GEOID, geometry)

madison_2023_rolls_joined_summ <- madison_2023_rolls_joined_summ |>
  left_join(wi_block_shapes, by = c("cbg_geoid" = "GEOID")) |>
  st_as_sf(sf_column_name = "geometry")

madison_2023_rolls_sf <- madison_2023_rolls_joined |>
  select(starts_with("delq"), lat, long) |>
  filter(!is.na(lat), !is.na(long)) |>
  filter(long < -80) |>       ## grabbed a few address in CT!
  st_as_sf(coords = c("long", "lat"), crs = 4326) |>
  st_transform(crs = st_crs(madison_2023_rolls_joined_summ))
  

ggplot() +
  geom_sf(madison_2023_rolls_joined_summ, mapping = aes(fill = estimate)) +
  geom_sf(madison_2023_rolls_sf |> filter(delq_utilities > 0), 
          mapping = aes(geometry = geometry, alpha = delq_utilities)) +
  theme_minimal()

