
## script to process one entry in 2024 Milw tax rolls
## tried to parse my column but spaces in the files are not read consistently or 
## the pages in each file are not consistently aligned or something like that.


process_one_entry <- function(one_entry) {
  
  ## work with first block of text according to index and codes at 
  ## https://city.milwaukee.gov/ImageLibrary/Groups/treasurerAuthors/images/CITY-OF-MILWAUKEE-PROPERTY-TAX-ROLL-FORMAT-INDEX-AND-KEY-CODES.pdf
  ## then go block by block
  
  
  first_block <- stringr::str_sub(one_entry$text_data, 1, 30)
  
  property_record <- stringr::str_extract(first_block[2], "^\\d{10}") 
  
  owner_address <- paste(first_block[4:length(first_block)], collapse = "")
  owner_address <- stringr::str_replace_all(owner_address, "  ", "")
  
  
  second_block <- stringr::str_sub(one_entry$text_data, 31, 62)
  
  
  property_location <- paste(second_block[2:length(second_block)], collapse = "")
  property_location <- stringr::str_replace_all(property_location, "  ", "") ##
  property_location <- trimws(property_location)
  
  property_address <- second_block[1]
  property_address <- trimws(property_address)
  
  property_df <- tibble(property_record, owner_address, property_location, property_address)
  
  third_block_1 <- stringr::str_sub(one_entry$text_data, 64, 64)
  
  #third_block_1
  
  third_block_2 <- stringr::str_sub(one_entry$text_data, 65, 78)
  
  #third_block_2
  
  tax_df <- tibble(rates_and_stuff = third_block_1, dollar_amounts = third_block_2) |>
    filter(!rates_and_stuff == " " & !dollar_amounts == " ") |>
    mutate(dollar_amounts = trimws(dollar_amounts),
           dollar_amounts = stringr::str_replace(dollar_amounts, ",", ""),
           dollar_amounts = as.numeric(dollar_amounts),
           rates_and_stuff = case_when(rates_and_stuff  == "R" ~ "tax_rate",
                                       rates_and_stuff  == "L" ~ "land_value",
                                       rates_and_stuff  == "I" ~ "improvement_value",
                                       rates_and_stuff  == "T" ~ "total_val",
                                       rates_and_stuff  == "E" ~ "est_fair_market_value",
           )
    ) |>
    pivot_wider(names_from = rates_and_stuff, values_from = dollar_amounts)
  
  
  fourth_block_1 <- stringr::str_sub(one_entry$text_data, 80, 89)
  
  #fourth_block_1
  
  fourth_block_2 <- stringr::str_sub(one_entry$text_data, 90, 100)
  
  #fourth_block_2
  
  
  assessments_df <-  tibble(assessments = fourth_block_1, dollar_amounts = fourth_block_2) |>
    filter(!assessments == "          " & !dollar_amounts == "          ") |>
    mutate(dollar_amounts = trimws(dollar_amounts),
           dollar_amounts = stringr::str_replace(dollar_amounts, ",", ""),
           dollar_amounts = as.numeric(dollar_amounts),
           assessments = trimws(assessments),
           assessments = stringr::str_replace_all(assessments, " ", "_"),
           assessments = tolower(assessments)
    ) |>
    pivot_wider(names_from = assessments, values_from = dollar_amounts) |>
    rename(total_assessment = total) 
  
  
  
  fifth_block_1 <- stringr::str_sub(one_entry$text_data, 101, 109)
  
  #fifth_block_1
  
  fifth_block_2 <- stringr::str_sub(one_entry$text_data, 110, 118)
  
  #fifth_block_2
  
  special_charges_df <-  tibble(special_charges = fifth_block_1, 
                                dollar_amounts = fifth_block_2) |>
    filter(!(special_charges == "         " & dollar_amounts == "         ")) |>
    filter(!(special_charges == "" & dollar_amounts == ""))
  
  if(nrow(special_charges_df) == 0) {
    
    special_charges_df = tibble(total_special_charges = 0)
    
  } else {
    
    special_charges_df <- special_charges_df |>
      mutate(dollar_amounts = trimws(dollar_amounts),
             dollar_amounts = stringr::str_replace(dollar_amounts, ",", ""),
             dollar_amounts = as.numeric(dollar_amounts),
             special_charges = trimws(special_charges),
             special_charges = stringr::str_replace_all(special_charges, " ", "_"),
             special_charges = tolower(special_charges)
      ) |>
      pivot_wider(names_from = special_charges, values_from = dollar_amounts) |>
      rename(total_special_charges = total) 
    
  }
  
  sixth_block_1 <- stringr::str_sub(one_entry$text_data, 119, 119)
  
  #sixth_block_1
  
  sixth_block_2 <- stringr::str_sub(one_entry$text_data, 120, 134)
  
  #sixth_block_2
  
  payments_and_credits_df <- tibble(payments_credits = sixth_block_1, dollar_amounts = sixth_block_2) |>
    filter(!(payments_credits == "" & dollar_amounts == "")) |>
    mutate(dollar_amounts = trimws(dollar_amounts),
           payments_credits = case_when(payments_credits == "T" ~ "total_gross_tax",
                                        payments_credits == "S" ~ "school_credit",
                                        payments_credits == "H" ~ "first_dollar_credit",
                                        payments_credits == "L" ~ "lottery_credit",
                                        payments_credits == "N" ~ "net_tax",
                                        payments_credits == "A" ~ "payments",
                                        payments_credits == "O" ~ "date_paid",
                                        payments_credits == "B" ~ "paid_by",
                                        payments_credits == "P" ~ "install_balance_due",
                                        payments_credits == "D" ~ "delq_balance_due",
                                        dollar_amounts %in% c("IP", "PD") ~ "payment_status")
    ) |>
    pivot_wider(names_from = payments_credits, values_from = dollar_amounts) 
  
  
  return_df <- bind_cols(property_df, tax_df, assessments_df, special_charges_df, payments_and_credits_df )         
  
}
