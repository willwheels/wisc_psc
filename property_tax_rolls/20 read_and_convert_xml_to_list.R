library(xml2)
library(dplyr)

## first, switch to reading each xml file

load(here::here("files_downloaded_from_archive.Rda"))

load(here::here("files_downloaded_from_city.Rda"))

all_xml_records <- rbind(milwaukee_records_tibble, milwaukee_records_tibble_no_ftp) |>
  filter(format %in% c("zip", "xml")) |>
  filter(stringr::str_detect(read_filename, "xml"))

## are they all unzipped?

unzip_if_needed <- function(format, dest_filename, read_filename){
  
  if(format == "zip" & !file.exists(here::here("data", "milwaukee_rolls", read_filename))) {
    
    unzip(here::here("data", "milwaukee_rolls", dest_filename), exdir = here::here("data", "milwaukee_rolls"))
    
  }
  
}

purrr::walk(1:nrow(all_xml_records), ~ unzip_if_needed(all_xml_records$format[.x],
                                                       all_xml_records$dest_filename[.x],
                                                       all_xml_records$read_filename[.x]
                                                       )
)
            

process_one_xml_to_list <- function(one_row){

  one_file <- read_xml(here::here("data", "milwaukee_rolls", one_row$read_filename))
  
  header <- xml_find_all(one_file, ".//d1:FileHeader")
  
  record_count <- xml_attr(header, "recordCount")
  
  all_items <- xml_find_all(one_file, ".//d1:Item")
  
  process_nodes <- purrr::map(all_items, as_list)
  
  tidytable::inv_gc()
  
  print(one_row$read_filename[1])
  print(paste("Header count is ", record_count))
  print(paste("Number of nodes process is ", as.character(length(process_nodes))))
  print(paste("These numbers are equal", as.integer(record_count) == length(process_nodes)))
  print(" ")
  
  save_filename <- paste0("processed_nodes_", "_", one_row$county, "_county_", one_row$year, ".Rda") 
  
  
  save(process_nodes, file = here::here("data", "milwaukee_rolls", "lists_of_xml_files", save_filename))

}

purrr::walk(1:nrow(all_xml_records), ~ process_one_xml_to_list(all_xml_records[.x, ]))
