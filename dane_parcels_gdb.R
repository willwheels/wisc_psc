library(sf)
library(dplyr)
library(ggplot2)

if(!file.exists(here::here("data", "madison_rolls", "V1100_Wisconsin_Parcels_DANE_GDB.zip"))){
  download.file("https://web.s3.wisc.edu/parcels/v11_parcels/county/V1100_Wisconsin_Parcels_DANE_GDB.zip",
                destfile = here::here("data", "madison_rolls", "V1100_Wisconsin_Parcels_DANE_GDB.zip"))
  
  unzip(here::here("data", "madison_rolls", "V1100_Wisconsin_Parcels_DANE_GDB.zip"),
        exdir = here::here("data", "madison_rolls"))
}


st_layers(here::here("data", "madison_rolls", "V1100_Wisconsin_Parcels_DANE.gdb"))

data_county <- st_read(here::here("data", "madison_rolls", "V1100_Wisconsin_Parcels_DANE.gdb"))

summary(data_county)

# 
# ggplot(data_county |> select(Shape)) +
#   geom_sf() +
#   theme_minimal()


## this is the whole county, merge with rolls

data_county <- data_county |>
  select(STATEID, PARCELID, PSTLADRESS, SITEADRESS, LONGITUDE, LATITUDE, Shape)

tidytable::inv_gc()


madison_2023_rolls <- readxl::read_xlsx(here::here("data", "TaxRoll2023.xlsx"), skip = 3)

## class = A is residential, B is apartments, C is manufacturing, D mostly ag, maybe vacant, PP commercial and downtown, v. confusing
## based on checking properties on https://www.cityofmadison.com/assessor/property/
madison_2023_rolls <- madison_2023_rolls |>
  janitor::clean_names() |>
  select(tax_year, status, class, prop_id, prop_location, total_assessment, est_fmv_total, delq_utilities, total_due) |>
  filter(class == "A") |>
  mutate(delq_pct_total = delq_utilities/total_due)

tidytable::inv_gc()

data_county <- data_county |>
  left_join(madison_2023_rolls, by = c("PARCELID" = "prop_id"))

sum(!is.na(data_county$status)) ## 74310, same checking other variables

data_county <- data_county |>
  filter(!is.na(tax_year))

tidytable::inv_gc()

ggplot(data_county |> select(Shape, delq_utilities)) +
  geom_sf(aes(fill = delq_utilities), color = NA) +
  scale_fill_viridis_c(direction = -1) +
  theme_minimal()
