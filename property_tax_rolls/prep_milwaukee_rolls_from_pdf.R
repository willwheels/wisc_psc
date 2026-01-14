library(dplyr)
library(tidyr)


load(here::here("data", "milwaukee_rolls", "milwaukee_property_tax_2024_from_pdf.Rda"))

last_page <- all_pages[[length(all_pages)]]

all_pages <- all_pages[1:length(all_pages)-1] |>
  bind_rows()

tidytable::inv_gc()

all_pages <- all_pages |>
  mutate(start_row_plus_one = stringr::str_detect(text_data, "^\\d{10} ")*1*row_number())

## the SECOND line of each entry is the property id, which is the easiest to flag
## so calculate start rows as second minus_one
start_rows <- which(all_pages$start_row_plus_one > 0)-1

## end row is the next start row minus one, except for the last one
end_rows <- start_rows[2:length(start_rows)]-1
end_rows[length(end_rows)+1] <- nrow(all_pages)




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

last_page <- all_pages[[length(all_pages)]]

all_pages <- all_pages[1:length(all_pages)-1] |>
  bind_rows()

tidytable::inv_gc()

all_pages <- all_pages |>
  mutate(start_row_plus_one = stringr::str_detect(text_data, "^\\d{10} ")*1*row_number())

## the SECOND line of each entry is the property id, which is the easiest to flag
## so calculate start rows as second minus_one
start_rows <- which(all_pages$start_row_plus_one > 0)-1

## end row is the next start row minus one, except for the last one
end_rows <- start_rows[2:length(start_rows)]-1
end_rows[length(end_rows)+1] <- nrow(all_pages)

## work with first property entry as test




##
## start function for one entry at a time here
##


process_one_entry <- function(one_entry) {
  
## work with first block of text according to index and codes at 
## https://city.milwaukee.gov/ImageLibrary/Groups/treasurerAuthors/images/CITY-OF-MILWAUKEE-PROPERTY-TAX-ROLL-FORMAT-INDEX-AND-KEY-CODES.pdf
## then go block by block
  
  #identify end of first block by start of text in first line of next block
  end_first_block <- stringr::str_locate(one_entry$text_data[1], "\\S")[1]-1
    
  first_block <- stringr::str_sub(one_entry$text_data, 1, end_first_block)
  
  first_block
  
  property_record <- stringr::str_extract(first_block[2], "^\\d{10}") 
  
  owner_address <- paste(first_block[4:length(first_block)], collapse = "")
  owner_address <- stringr::str_replace_all(owner_address, "  ", "")
  
  
  # ditch already processed parts of entry
  stringr::str_sub(one_entry$text_data, 1, end_first_block) <- ""
  
 
  
  ## NEED TO FIX BECAUSE ASSESSMENTS CAN BE OVER $1 million!!!!!
  

  dim_second_block <- matrix(nrow = nrow(one_entry), ncol = 2)
  dim_second_block[1, ] <- stringr::str_locate(one_entry$text_data[1], "R\\s+\\d+\\.\\d+\\s+(?=CITY)") 
  dim_second_block[2, ] <- stringr::str_locate(one_entry$text_data[2], "L\\s+\\d*\\,*\\d*\\,*\\d*\\s+(?=COUNTY)")
  dim_second_block[3, ] <- stringr::str_locate(one_entry$text_data[3], "I\\s+\\d*\\,*\\d*\\,*\\d*\\s+(?=SEWER)")
  dim_second_block[4, ] <- stringr::str_locate(one_entry$text_data[4], "T\\s+\\d*\\,*\\d*\\,*\\d*\\s+(?=PUBLIC)")
  dim_second_block[5, ] <- stringr::str_locate(one_entry$text_data[5], "E\\s+\\d*\\,*\\d*\\,*\\d*\\s+(?=STATE)")
 
  
  #if(dim_second_block[1:5, ] < start_third_block[1:5, 1] )

  end_second_block <- dim_second_block[,1]-1
  end_second_block <- if_else(is.na(end_second_block), min(end_second_block, na.rm = TRUE), end_second_block)
  
  second_block <- stringr::str_sub(one_entry$text_data, 1, end_second_block)
  
  
  property_location <- paste(second_block[2:length(second_block)], collapse = "")
  property_location <- stringr::str_replace_all(property_location, "  ", "") 
  property_location <- trimws(property_location)
  
  property_address <- second_block[1]
  property_address <- trimws(property_address)
  
  property_df <- tibble(property_record, owner_address, property_location, property_address)
  

  stringr::str_sub(one_entry$text_data, 1, end_second_block) <- ""
  
  assessments_regex <- "CITY GOV|COUNTY GOV|SEWERAGE|PUBLIC SCH|STATE|TECH COLL|TOTAL"
  
  
  start_third_block <- stringr::str_locate(one_entry$text_data[1:5], assessments_regex)

  ##
  third_block <- one_entry |>
    filter(stringr::str_sub(text_data, 1, 1) %in% c("R", "L", "I", "T", "E")) 

  third_block_1 <- stringr::str_sub(third_block$text_data, 1, 1)
  
  third_block_1
  
  third_block_2 <- stringr::str_sub(third_block$text_data, 2, start_third_block[,1]-1)
  
  third_block_2

  
  tax_df <- tibble(rates_and_stuff = third_block_1, dollar_amounts = third_block_2) |>
    # filter(!((rates_and_stuff == " " | rates_and_stuff == "")   ## maybe not needed after above filter
    #          & (trimws(dollar_amounts) == "" | is.na(dollar_amounts)))) |>
    mutate(dollar_amounts = trimws(dollar_amounts),
           dollar_amounts = stringr::str_replace_all(dollar_amounts, ",", ""),
           dollar_amounts = stringr::str_replace_all(dollar_amounts, "  ", ""),
           dollar_amounts = stringr::str_replace(dollar_amounts, "^$", "0"),
           dollar_amounts = replace_na(dollar_amounts, "0"),
           dollar_amounts = as.numeric(dollar_amounts),
           rates_and_stuff = case_when(rates_and_stuff  == "R" ~ "tax_rate",
                                       rates_and_stuff  == "L" ~ "land_value",
                                       rates_and_stuff  == "I" ~ "improvement_value",
                                       rates_and_stuff  == "T" ~ "total_val",
                                       rates_and_stuff  == "E" ~ "est_fair_market_value",
           )
    ) |>
    pivot_wider(names_from = rates_and_stuff, values_from = dollar_amounts)
  
  
  stringr::str_sub(one_entry$text_data, 1, 1) <- ""
  
  one_entry$text_data <- stringr::str_replace(one_entry$text_data, paste0("^", third_block_2, collapse = "|"), "")
  

  
  # this regex should match a dollar amount
  # and this is a matrix now
  #end_fourth_block <- stringr::str_locate(one_entry$text_data, "\\d?\\,?\\d+\\.\\d{2}")  ## am gambling that the "CITY GOV" amount due is in all entries
  
  #end_fourth_block <- na.omit(end_fourth_block)
  
  #stringr::str_extract(one_entry$text_data, "\\d?\\,?\\d+\\.\\d{2}") 
  
  
  fourth_block <- one_entry |>
    filter(stringr::str_detect(text_data, assessments_regex )) 
  
  fourth_block_1 <- stringr::str_extract(fourth_block$text_data, assessments_regex)
  
  fourth_block_1
  
  fourth_block_2 <- stringr::str_extract(fourth_block$text_data, "\\d*\\,\\d\\d\\d\\.\\d{2}|\\d+.\\d{2}") 
  
  fourth_block_2
  
  assessments_df <-  tibble(assessments = fourth_block_1, dollar_amounts = fourth_block_2) |>
    filter(!(trimws(assessments) == "" & trimws(dollar_amounts) == "")) |>
    mutate(dollar_amounts = trimws(dollar_amounts),
           dollar_amounts = stringr::str_replace(dollar_amounts, ",", ""),
           dollar_amounts = replace_na(dollar_amounts, "0"),
           dollar_amounts = as.numeric(dollar_amounts),
           assessments = trimws(assessments),
           assessments = stringr::str_replace_all(assessments, " ", "_"),
           assessments = tolower(assessments)
    ) |>
    pivot_wider(names_from = assessments, values_from = dollar_amounts) |>
    rename(total_assessment = total) 
  
  one_entry$text_data <- stringr::str_replace(one_entry$text_data, paste0(fourth_block_1, collapse = "|"), "")
  one_entry$text_data <- stringr::str_replace(one_entry$text_data, paste0(fourth_block_2, collapse = "|"), "")
  
  ## going out of order here because the fifth block may not exist
  
  # okay maybe wrong way to go, it's nearly impossible to state regex from end
  # problem is S code and FIRE INS
  
  ######################################3
  
  #start_sixth_block <- stringr::str_locate(one_entry$text_data, "((T |S | H |L |N |A |P |D )\\s*\\d+\\.\\d{2})|((O |B )[:print:]+$)|(IP$|PD$)")  ## am gambling that the total tax is in all entries

  special_charges_vector <- c("FIRE INS", "DELQ SEWER", "DELQ STORM", "DELQ WATER", "DELQ SERV","BLK WASTE","GARBAGE CA", "BLDG NUISA", "BID#48",
                              "TOTAL", "SNOW REMOV")
  
  fifth_block_regex <- paste0("(",paste(special_charges_vector, collapse = "|"), ")\\s+\\d*\\,?\\d+\\.\\d{2}")

  fifth_block <- stringr::str_extract(one_entry$text_data, fifth_block_regex)
  
  fifth_block
  
  ## ugh, using different regex!
  dollar_amounts <- stringr::str_extract(fifth_block,"\\d?\\,?\\d+\\.\\d{2}")
  
  fifth_block <- stringr::str_remove(fifth_block,"\\d?\\,?\\d+\\.\\d{2}")

  special_charges_df_temp <-  tibble(special_charges = fifth_block, dollar_amounts = dollar_amounts) |>
    filter(!(is.na(dollar_amounts))) |>
    mutate(special_charges = trimws(special_charges))
  
  
  special_charges_df <-tibble(special_charges = special_charges_vector) |>
    left_join(special_charges_df_temp, by = join_by(special_charges)) |>
    mutate(dollar_amounts = trimws(dollar_amounts),
           dollar_amounts = stringr::str_replace(dollar_amounts, ",", ""),
           dollar_amounts = replace_na(dollar_amounts, "0"),
           dollar_amounts = as.numeric(dollar_amounts),
           special_charges = trimws(special_charges),
           special_charges = stringr::str_replace_all(special_charges, " ", "_"),
           special_charges = tolower(special_charges)
      ) |>
      pivot_wider(names_from = special_charges, values_from = dollar_amounts) |>
      rename(total_special_charges = total) 
    
  
  
  
  one_entry$text_data <- stringr::str_remove(one_entry$text_data, fifth_block_regex)
  
  one_entry$text_data <- trimws(one_entry$text_data)
  
  sixth_block_1 <- stringr::str_sub(one_entry$text_data, 1, 2)
  
  sixth_block_1
  
  sixth_block_2 <- stringr::str_sub(one_entry$text_data, 3, nchar(one_entry$text_data))
  
  sixth_block_2
  
  payments_and_credits_df <- tibble(names = sixth_block_1, values = sixth_block_2) |>
    filter(!(names == "" & values == "")) |>
    mutate(values = trimws(values),
           values = if_else(values == "" & names %in% c("IP", "PD"), names, values),
           names = case_when(names == "T " ~ "total_gross_tax",
                             names == "S " ~ "school_credit",
                             names == "H " ~ "first_dollar_credit",
                             names == "L " ~ "lottery_credit",
                             names == "N " ~ "net_tax",
                             names == "A " ~ "payments",
                             names == "O " ~ "date_paid",
                             names == "B " ~ "paid_by",
                             names == "P " ~ "install_balance_due",
                             names == "D " ~ "delq_balance_due",
                             names %in% c("IP", "PD") ~ "payment_status")
    ) |>
    pivot_wider(names_from = names, values_from = values) 
  
  
  
  
  return_df <- bind_cols(property_df, tax_df, assessments_df, special_charges_df, payments_and_credits_df )         
  
}
         
        

one_entry <- all_pages[start_rows[551]:end_rows[551], 1]

check <- process_one_entry(one_entry)

# # Start the clock!
# ptm <- proc.time()
# 
# test_map <- purrr::map(1:1000, ~ process_one_entry(all_pages[start_rows[.x]:end_rows[.x], 1]))
# 
# # Stop the clock
# proc.time() - ptm
# 
# ## about one minute for 250
# 
# test_map <- bind_rows(test_map)

####

future::plan(future::multisession, workers = 8)


# Start the clock!
ptm <- proc.time()

test_map <- furrr::future_map(1:2000, 
                              ~ process_one_entry(all_pages[start_rows[.x]:end_rows[.x], 1]))

# Stop the clock
proc.time() - ptm

future::plan(future::sequential)

## about 87 seconds for 1000, 4 workers
## about 158 seconds for 1000, 8 workers
## about 88 seconds for 2000, 8 workers
test_map_furrr <- bind_rows(test_map_furrr)


##############

# mirai::daemons(4)

# Start the clock!
# ptm <- proc.time()
# 
# test_map_parallel <- purrr::map(1:1000,
#                                 purrr::in_parallel(\(x) process_one_entry(all_pages[start_rows[x]:end_rows[x], 1]),
#                                                    process_one_entry = process_one_entry, 
#                                                    #all_pages = all_pages,
#                                                    start_rows = start_rows, end_rows = end_rows)
#                                 )
# 
# # Stop the clock
# proc.time() - ptm
# 
# 
# 
# test_map_parallel <- bind_rows(test_map_parallel)


#mirai::daemons(0)
## This failed due to memory error--would need to rewrite in order to move less to each daemon


# Start the clock!
ptm <- proc.time()

all_records <- purrr::map(1:length(start_rows),
                          ~ process_one_entry(all_pages[start_rows[.x]:end_rows[.x], 1]))

# Stop the clock
proc.time() - ptm


all_records <- bind_rows(all_records)


