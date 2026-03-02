



process_address_as_list <- function(address_list) {
  
  
  if(names(address_list) == "USAddress") {
    
    address_type <- "US"
    
    country <- "US"
    
    address_detail_list <- address_list$USAddress
    
  } else {
    
    address_type <- "Foreign"
    
    country <- address_list$Country
    
    address_detail_list <- address_list$ForeignAddress
    
  }
  
  
  address_df <- as_tibble(purrr::map(purrr::flatten(address_detail_list), unlist))

  
  return_df <- tibble::tibble(address_type, address_df, country)
  
  return(return_df)
  
}



process_owner_and_address_info <- function(owner_and_address_info_list) {
  
  owner_mailing_address_df <- process_address_as_list(owner_and_address_info_list$MailingAddress)
  
  colnames(owner_mailing_address_df) <- paste0("owner_mailing_", colnames(owner_mailing_address_df))
  
  
  owner_list <- owner_and_address_info_list$Owner
  
  if(names(owner_and_address_info_list$Owner) == "Individual") {
    
    owner_type <- "Individual"
    
    owner_name <- paste(owner_list$FirstName, owner_list$LastName)
    
    address_list <- owner_and_address_info_list$Owner$Individual$Address
    
    
  } else if(names(owner_and_address_info_list$Owner) ==  "CombinedName") {
    
    owner_type <- "Combined"
    
    owner_name = owner_list$CombinedName$Name[[1]]  ## not a node in XML structure (see Excel)
    
    address_list <- owner_and_address_info_list$Owner$CombinedName$Address
    
    
  } else if(names(owner_and_address_info_list$Owner) ==  "Business") {
    
    owner_type <- "Business"
    
    owner_name = owner_list$BusinessNameLine1
    
    address_list <- owner_and_address_info_list$Owner$Business$Address
    
  }
  
  owner_second_address_df <- process_address_as_list(address_list)
  
  colnames(owner_second_address_df) <- paste0("owner_second_", colnames(owner_second_address_df))
  
  
  owner_and_address_info_df <- tibble(owner_type, owner_name)
  
  
  if(!is.null(owner_and_address_info_list$SiteAddress)) {
    
    address_line_1 <- owner_and_address_info_list$SiteAddress$AddressLine1[[1]]

    site_address_df <- tibble(address_line_1 = address_line_1, no_physical_address = NA_character_)
    
  } else {
    
    site_address_df <- tibble(address_line_1 = NA_character_ ,  
                              no_physical_address = owner_and_address_info_list$NoPhysicalAddress)
    
  }
  
  colnames(site_address_df) <- paste0("site_address_", colnames(site_address_df))
  
  owner_and_address_info_df <- bind_cols(owner_and_address_info_df, owner_mailing_address_df, owner_second_address_df, site_address_df) |>
    janitor::clean_names()
  
  return(owner_and_address_info_df)
  
}


## JURISDICTION FUNCTIONS



## JURISDICTION FUNCTIONS

process_county <- function(county_list) {
  
  county_name <- county_list$CountyName[[1]]
  
  county_rate <- county_list$CountyRate[[1]]
  
  county_tax <- county_list$CountyTax[[1]]
  
  county_df <- tibble(county_name, county_rate, county_tax) 
  
  return(county_df)
  
}

process_municipality <- function(municipality_list) {
  
  muni_name <- municipality_list$MuniName[[1]]
  
  muni_number <- municipality_list$MuniNumber[[1]]
  
  municipal_rate <-municipality_list$MunicipalRate[[1]]
  
  municipal_tax <- municipality_list$MunicipalTax[[1]]
  
  municipality_df <- tibble(muni_name,  muni_number, municipal_rate, municipal_tax)
  
  return(municipality_df)
  
}

process_coderatetax <- function(code_rate_tax_list) {
  
  code <- code_rate_tax_list$Code[[1]]
  
  rate <- code_rate_tax_list$Rate[[1]]
  
  tax <- code_rate_tax_list$Tax[[1]]
  
  code_rate_tax_df <- tibble(code, rate, tax)
  
  if(nrow(code_rate_tax_df) == 0) {code_rate_tax_df = tibble(code = NA_character_, rate = NA_character_, tax = NA_character_)}
  
  return(code_rate_tax_df)
  
}

# 

process_special_district <- function(special_district_list) {
  
  special_district_code <- special_district_list$Code[[1]]
  
  special_district_rate <- special_district_list$Rate[[1]]
  
  special_district_tax <- special_district_list$Tax[[1]]
  
  special_district_df <- tibble(special_district_code,  special_district_rate,  special_district_tax)
  
  return(special_district_df)
  
}

# NOT INCLUDE TIDs FOR NOW SINCE THEY DON'T HAVE TAX RATES

process_jurisdiction_info <- function(jurisdiction_list) {
  
  county_df <- process_county(jurisdiction_list$County)
  
  municipality_df <- process_municipality(jurisdiction_list$Municipality)
  
  school_df <- process_coderatetax(jurisdiction_list$School)
  
  colnames(school_df) <- paste0("school_", colnames(school_df))
  
  union_hs_df <- process_coderatetax(jurisdiction_list$UnionHS)
  
  colnames(union_hs_df) <- paste0("union_hs_", colnames(union_hs_df))
  
  tech_df <- process_coderatetax(jurisdiction_list$Tech)
  
  colnames(tech_df) <- paste0("tech_", colnames(tech_df))
  
  special_district_df <- process_special_district(jurisdiction_list$SpecialDistrict)
  
  jurisdiction_df <- bind_cols(county_df, municipality_df,  school_df, union_hs_df, tech_df, special_district_df) |>
    mutate(across(ends_with("tax")|ends_with("rate"), as.numeric))
  
  return(jurisdiction_df)
  
}

process_tax_summary <- function(tax_summary_list) {
  
  land_tax_total  <- tax_summary_list$LandTaxableTotal[[1]]
  improvements_tax_total  <- tax_summary_list$ImprovementsTaxableTotal[[1]]
  total_tax_value  <- tax_summary_list$TotalTaxableValue[[1]]
  est_fair_market_value  <- tax_summary_list$EstimatedFairMarketValue[[1]]
  state_tax  <- tax_summary_list$StateTax[[1]]
  occupational_tax  <- tax_summary_list$OccupationalTax[[1]]
  forest_taxable  <- tax_summary_list$ForestTaxable[[1]]
  BOR_value  <- tax_summary_list$BORValue[[1]]
  private_forest_crop_tax  <- tax_summary_list$PrivateForestCropTax[[1]]
  managed_forest_tax  <- tax_summary_list$ManagedForestLawTax[[1]]
  tax_total  <- tax_summary_list$TaxTotal[[1]]
  school_credit <- tax_summary_list$SchoolCredit[[1]]
  lottery_credit  <- tax_summary_list$LotteryCredit[[1]]
  lottery_count  <- tax_summary_list$LotteryCount[[1]]
  lottery_credit_pass_fail  <- tax_summary_list$LotteryCreditPassFail[[1]]
  first_dollar_credit  <- tax_summary_list$FirstDollarCredit[[1]]
  net_tax  <- tax_summary_list$NetTax[[1]]
  payment  <- tax_summary_list$Payment[[1]]
  amount_due  <- tax_summary_list$AmountDue[[1]]
  
  
  delq_utilities <- tax_summary_list$DelinquentUtilityCharges[[1]]
  
  special_assessments <- tax_summary_list[names(tax_summary_list) == "SpecialAssessment"]
  
  num_special_assessments <- length(special_assessments)
  
  special_assessment_descs <- purrr::map(special_assessments, ~ purrr::pluck(.x, "SpecialAssessmentDesc")[[1]])
  
  special_assessment_amts <- purrr::map(special_assessments, ~ purrr::pluck(.x, "SpecialAssessmentCharge")[[1]])
  
  if(num_special_assessments > 0 ) {
    
    special_assessment_descs_cols <- as.character(special_assessment_descs)
    names(special_assessment_descs_cols) <- paste0("special_assessment_desc", paste0("_", as.character(seq(1, num_special_assessments))))
    
    special_assessment_amts_cols <-  as.numeric(special_assessment_amts)
    names(special_assessment_amts_cols) <- paste0("special_assessment_amt", paste0("_", as.character(seq(1, num_special_assessments))))
    
    
  }
 
  
  tax_summary_df <- tibble(land_tax_total, improvements_tax_total, total_tax_value, est_fair_market_value,
                           state_tax, occupational_tax, forest_taxable, BOR_value, private_forest_crop_tax, 
                           managed_forest_tax, tax_total, school_credit,
                           lottery_credit, lottery_count, lottery_credit_pass_fail,
                           first_dollar_credit, net_tax, payment, amount_due, delq_utilities) |>
    mutate(across(everything(), as.numeric))
  
  tax_summary_df <- bind_cols(tax_summary_df, 
                              tibble::as_tibble_row(special_assessment_descs_cols),
                              tibble::as_tibble_row(special_assessment_amts_cols) 
  )
  
  
}
