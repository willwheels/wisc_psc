library(xml2)
library(dplyr)

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

#xml_structure(rolls_milw_county_2024)


xml_ns(rolls_milw_county_2024)

# # Start the clock!
# ptm <- proc.time()
# 
# xml_ns_strip(rolls_milw_county_2024) ## memory issues on laptop
# 
# # Stop the clock
# proc.time() - ptm



all_items <- xml_find_all(rolls_milw_county_2024, ".//d1:Item")

tidytable::inv_gc()

source(here::here("xml_source_functions.R"))

process_one_item <- function(item_nodeset) {
  
  #item_nodeset <- all_items[44]
  
  record_number <- xml_integer(xml_find_first(item_nodeset, ".//d1:RecordNumber"))
  
  property_info_df <- process_property_info(xml_find_first(item_nodeset, ".//d1:PropertyInfo"))
  
  owner_and_address_info_df <- process_owner_and_address_info(xml_find_first(item_nodeset, ".//d1:OwnerAndAddressInfo"))
  
  ## skipping over valuation info in XML structure
  
  jurisdiction_info_df <- process_jurisdiction_info(xml_find_first(item_nodeset, ".//d1:JurisdictionInfo"))
  
  tax_summary_df <- process_tax_summary(xml_find_first(item_nodeset, ".//d1:TaxSummary"))
  
  full_df <- bind_cols(tibble(record_number), property_info_df, owner_and_address_info_df, 
                       jurisdiction_info_df, tax_summary_df)
  
  return(full_df)
  
}

## TAX RATES AND AMOUNTS ARE IN JURISDICTION AND TAX SUMMARY, I THINK

# Start the clock!
ptm <- proc.time()

one_df <- process_one_item(all_items[1])

# Stop the clock
proc.time() - ptm

## 100 seconds for one on tiny laptop

# Start the clock!
ptm <- proc.time()

ten_dfs <- purrr::map(all_items[1:10], process_one_item) |>
  bind_rows()

# Stop the clock
proc.time() - ptm

# 780 seconds for 10 on tiny laptop
# completely impractical

## single occurence of municipality

