library(xml2)

if(!dir.exists(here::here("data", "milwaukee_rolls"))) {dir.create(here::here("data", "milwaukee_rolls"))}


milw_milw_county_2024 <- "https://city.milwaukee.gov/ImageLibrary/Groups/treasurerAuthors/images/2024RET402025050001---Updated-CM-3-7-25.zip"

# download.file(milw_milw_county_2024, 
#               destfile = here::here("data", "milwaukee_rolls", "2024RET402025050001---Updated-CM-3-7-25.zip"),
#               mode = "wb",headers = c(
#                 `user-agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'
#               ))
# 
# list_zips <- list.files(here::here("data", "milwaukee_rolls"), full.names = TRUE)
# 
# list_zips <- stringr::str_subset(list_zips, ".zip")
# 
# unzip(list_zips[1], exdir = here::here("data", "milwaukee_rolls"))
# 
 list_xml <- list.files(here::here("data", "milwaukee_rolls"), full.names = TRUE)
 
 list_xml <- stringr::str_subset(list_xml, ".xml")



rolls_milw_county_2024 <- read_xml(list_xml[1])

tidytable::inv_gc()

#rolls_milw_county_2024 <- as_list(rolls_milw_county_2024)

xml_structure(rolls_milw_county_2024)

items <- xml_find_all(rolls_milw_county_2024, ".//RecordNumber")
