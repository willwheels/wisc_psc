library(xml2)
library(dplyr)

if(!dir.exists(here::here("data", "milwaukee_rolls"))) {dir.create(here::here("data", "milwaukee_rolls"))}

load(here::here("data", "milwaukee_rolls", "lists_of_xml_files", "processed_nodes_milwaukee_county_2024.Rda"))


source(here::here("property_tax_rolls", "list_source_functions.R"))

process_one_item <- function(item_list, call_how_many = "map") {
  
  
  if(call_how_many == "one") {item_list <- item_list[[1]]}
  
  
  record_number <- item_list$RecordNumber[[1]]
  
  local_id1 <- item_list$PropertyInfo$LocalID1[[1]]
  
  owner_and_address_info_df <- process_owner_and_address_info(item_list$OwnerAndAddressInfo)
  
  ## skipping over valuation info in XML structure
  
  jurisdiction_info_df <- process_jurisdiction_info(item_list$JurisdictionInfo)
  
  tax_summary_df <- process_tax_summary(item_list$TaxSummary)
  
  full_df <- bind_cols(tibble(record_number, local_id1), owner_and_address_info_df, 
                       jurisdiction_info_df, tax_summary_df)
  
  return(full_df)
  
}




## 100 seconds for one on tiny laptop xml
## 20 seconds on big new one?!? xml
## <0.1 seconds when I switch to lists!!

# # Start the clock!
# ptm <- proc.time()
# 
# process_nodes <- purrr::map(all_items[1:20], as_list)
# 
# ten_dfs <- purrr::map(process_nodes, process_one_item) |>
#   bind_rows()
# 
# # Stop the clock
# proc.time() - ptm

# 780 seconds for 10 on tiny laptop
# completely impractical

# 290 for ten on new lapton
# 10 in .3 seconds


## single occurence of municipality


# Start the clock!
ptm <- proc.time()

ten_dfs <- purrr::map(process_nodes[1:10000], process_one_item) |>
  data.table::rbindlist(fill = TRUE)

# Stop the clock
proc.time() - ptm

## new laptop is 3800 seconds, abt one hour

tidytable::inv_gc()


future::plan(future::multisession, workers = 8)



# Start the clock!
ptm <- proc.time()

ten_dfs <- furrr::future_map(process_nodes[1:10000], process_one_item) |>
  data.table::rbindlist(fill = TRUE)

# Stop the clock
proc.time() - ptm



# 10,000, new laptop, not parallel = 192 seconds
# 10,000, new laptop, parallel, 8 workers = 60 seconds

