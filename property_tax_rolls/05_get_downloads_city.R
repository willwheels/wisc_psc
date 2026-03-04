library(rvest)
library(dplyr)


###
### This section is for everything from the web archive because the Milwaukee webiste changed between the January 2026
### snapshot in the Internet Archive and early March 2026

milwaukee_records_url <- "https://city.milwaukee.gov/treasurer/Tax-Rolls.htm"

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
         county = stringr::str_extract(records_text, "-Milwaukee|-Washington|-Waukesha"),
         county = stringr::str_remove(county, stringr::fixed("-")),
         dest_filename = stringr::str_split(download_url, stringr::fixed("/")),
         dest_filename = purrr::map(dest_filename, ~.x[length(.x)][[1]]),
         dest_filename = as.character(dest_filename),
         dest_filename = if_else(stringr::str_detect(dest_filename, "^20\\d\\d"),
                                 dest_filename, paste0(year, "_", dest_filename))
         )


user_agent_string = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.5005.61 Safari/537.36'

base_milw_url <- "https://city.milwaukee.gov"

## download all city files using urls taken from internet archive

purrr::walk(1:nrow(milwaukee_records_tibble),
            ~ httr::GET(url = paste0(base_milw_url, milwaukee_records_tibble$download_url[.x]), 
                               httr::add_headers(`User-Agent` = user_agent_string),
                               # Use httr::write_disk to save the response body to a file
                               httr::write_disk(here::here("data", "milwaukee_rolls", 
                                                           milwaukee_records_tibble$dest_filename[.x][[1]]), overwrite = TRUE) 
            )
)
