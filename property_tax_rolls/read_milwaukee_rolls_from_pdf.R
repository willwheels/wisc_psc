library(pdftools)
library(dplyr)


test_milw <- pdf_text(here::here("data", "milwaukee_rolls", "TAX-ROLL--MILWAUKEE-40.pdf"))


convert_one_page_to_tibble <- function(one_page) {
  
  one_page <- one_page |>
    strsplit("\n") |>
    unlist() |>
    tibble::as_tibble_col(column_name = "text_data" ) 
  
  first_line <- one_page[1, ]
  page_num <- stringr::str_extract(first_line$text_data[1], "PAGE\\s+\\d+")
  print(paste("processing", page_num))
  
  one_page <- one_page |>
    filter(!(stringr::str_detect(text_data, "REAL PROPERTY") & stringr::str_detect(text_data, "PAGE")))
  
}


all_pages <- purrr::map(test_milw, convert_one_page_to_tibble)

save(all_pages, file = here::here("data", "milwaukee_rolls", "milwaukee_property_tax_2024_from_pdf.Rda"))
