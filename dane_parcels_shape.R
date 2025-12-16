library(sf)

if(!file.exists(here::here("data", "madison_rolls", "V1100_Wisconsin_Parcels_DANE_SHP.zip"))){
  download.file("https://web.s3.wisc.edu/parcels/v11_parcels/county/V1100_Wisconsin_Parcels_DANE_SHP.zip",
               destfile = here::here("data", "madison_rolls", "V1100_Wisconsin_Parcels_DANE_SHP.zip"))
  
  unzip(here::here("data", "madison_rolls", "V1100_Wisconsin_Parcels_DANE_SHP.zip"),
        exdir = here::here("data", "madison_rolls"))
}

data_county <- st_read(here::here("data", "madison_rolls", "V1100_Wisconsin_Parcels_DANE.shp"))

summary(data_county)
