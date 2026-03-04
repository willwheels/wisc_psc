library(rvest)
library(dplyr)


milwaukee_records_url <- "https://web.archive.org/web/20260104211650/https://city.milwaukee.gov/treasurer/Tax-Rolls.htm"

milwaukee_records_page <- read_html(milwaukee_records_url)

milwaukee_records_lists <- milwaukee_records_page |>
  html_elements("ul li a")  

milwaukee_records_hrefs <- milwaukee_records_lists |>
  html_attr("href")

milwaukee_records_text <- milwaukee_records_lists |>
  html_text2()


milwaukee_records_tibble <- tibble::tibble(records_href = milwaukee_records_hrefs, 
                                           records_text = milwaukee_records_text) |>
  filter(stringr::str_detect(records_text, "^20\\d\\d")) |>
  filter(stringr::str_detect(records_text, "Real Estate")) |>
  mutate(year = stringr::str_extract(records_text,"20\\d\\d"),
         format = stringr::str_extract(records_href, "pdf|zip|txt"),
         download_url = stringr::str_remove(records_href, "/web/20260104211650/"),
         download_url = stringr::str_remove(download_url, "https://web.archive.org"),
         county = if_else(as.numeric(year) < 2021, 
                          "all",
                          stringr::str_extract(records_text, "Milwaukee|Washington|Waukesha")),
         dest_filename = stringr::str_split(download_url, stringr::fixed("/")),
         dest_filename = purrr::map(dest_filename, ~.x[length(.x)]),
         dest_filename = as.character(dest_filename),
         dest_filename = if_else(stringr::str_detect(dest_filename, "^20\\d\\d"),
                                 dest_filename, paste0(year, "_", dest_filename))
         
         ) |>
  filter(as.numeric(year) < 2022) ## uses versions from city website instead of these

milwaukee_records_tibble_no_ftp <- milwaukee_records_tibble |>
  filter_out(stringr::str_detect(download_url, "^ftp"))

user_agent_string = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'

res <- httr::GET(
  url = milwaukee_records_tibble_no_ftp$download_url[1], 
  httr::add_headers(`User-Agent` = user_agent_string),
  # Use httr::write_disk to save the response body to a file
  httr::write_disk(here::here("data", "milwaukee_rolls", milwaukee_records_tibble_no_ftp$dest_filename[1][[1]]), overwrite = TRUE) 
)


purrr::walk(1:nrow(milwaukee_records_tibble_no_ftp),
            ~ httr::GET(url = milwaukee_records_tibble_no_ftp$download_url[.x], 
                               httr::add_headers(`User-Agent` = user_agent_string),
                               # Use httr::write_disk to save the response body to a file
                               httr::write_disk(here::here("data", "milwaukee_rolls", milwaukee_records_tibble_no_ftp$dest_filename[.x][[1]]), overwrite = TRUE) 
            )
)


## retrieve filenames from zip files
## nb I tried to do this in a mutate call and failed

find_read_filename <- function(format, dest_filename) {
  
  if(format == "zip") {
    #read_filename <- unzip(here::here("data", "milwaukee_rolls", dest_filename), list = TRUE)$Name
    read_filename_long <- unzip(here::here("data", "milwaukee_rolls", dest_filename), list = TRUE)
    
    read_filename <- read_filename_long$Name
    
    }
  
  else {
    read_filename <- dest_filename
  }
  
  return(read_filename)
}


read_filenames <- purrr::map2_vec(milwaukee_records_tibble_no_ftp$format, milwaukee_records_tibble_no_ftp$dest_filename,
                                  find_read_filename)

milwaukee_records_tibble_no_ftp$read_filename <- read_filenames

save(milwaukee_records_tibble_no_ftp, file = here::here("files_downloaded_from_archive.Rda"))
