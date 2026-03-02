library(xml2)
library(dplyr)

if(!dir.exists(here::here("data", "milwaukee_rolls"))) {dir.create(here::here("data", "milwaukee_rolls"))}


milw_milw_county_2024 <- "https://city.milwaukee.gov/ImageLibrary/Groups/treasurerAuthors/images/2024RET402025050001---Updated-CM-3-7-25.zip"

list_xml <- list.files(here::here("data", "milwaukee_rolls"), full.names = TRUE)

list_xml <- stringr::str_subset(list_xml, ".xml")



rolls_milw_county_2024 <- read_xml(list_xml[1])

tidytable::inv_gc()



all_items <- xml_find_all(rolls_milw_county_2024, ".//d1:Item")

tidytable::inv_gc()


process_nodes <- purrr::map(all_items, as_list)


tidytable::inv_gc()

save(process_nodes, file = here::here("data", "milwaukee_rolls", "lists_of_xml_files", "processed_nodes_milwaukee_county_2024.Rda"))
