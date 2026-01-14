
process_property_info <- function(property_info_nodeset) {
  
  local_id1 <- xml_text(xml_find_first(property_info_nodeset, ".//d1:LocalID1"))
  
  town <-  xml_text(xml_find_first(property_info_nodeset, ".//d1:Town"))
  
  
  return_df <- tibble::tibble(local_id1, town)
  
}

process_address <- function(address_nodeset) {
  
  if(length(xml_find_first(address_nodeset, ".//d1:USAddress")) > 0) {
    
    address_type <- "US"
    
    country <- "US"
    
    address_detail_nodeset <- xml_find_first(address_nodeset, ".//d1:USAddress")
    
  } else {
    
    address_type <- "Foreign"
    
    country <- xml_text(xml_find_first(address_nodeset, ".//d1:Country"))
    
    address_detail_nodeset <- xml_find_first(address_nodeset, ".//d1:ForeignAddress")
    
  }
  
  
  address_line_1 <- xml_text(xml_find_first(address_detail_nodeset, ".//d1:AddressLine1"))
  
  address_line_2 <- xml_text(xml_find_first(address_detail_nodeset, ".//d1:AddressLine2"))
  
  city <- xml_text(xml_find_first(address_detail_nodeset, ".//d1:City"))
  
  state <- xml_text(xml_find_first(address_detail_nodeset, ".//d1:State"))
  
  zip <- xml_text(xml_find_first(address_detail_nodeset, ".//d1:ZIPCode"))
  
  return_df <- tibble::tibble(address_type, address_line_1, address_line_2, city, state, zip, country)
  
}


process_owner_name <- function(name_nodeset, owner_type) {
  
  if(owner_type == "Individual") {
    
    name = paste(xml_text(xml_find_first(name_nodeset, ".//d1:FirstName")), xml_text(xml_find_first(name_nodeset, ".//d1:LastName")))
    
  } else if(owner_type == "Combined") {
    
    name = xml_text(name_nodeset, ".//d1:Name")  ## not a node in XML structure (see Excel)
    
  } else if(owner_type == "Business") {
    
    name = xml_text(xml_find_first(name_nodeset, ".//d1:BusinessNameLine1"))
    
  }
  
  return(name)
  
}


process_site_address <- function(site_address_nodeset) {
  
  
}


process_site_address <- function(site_address_nodeset) {
  
  address_line_1 <- xml_text(xml_find_first(site_address_nodeset, "./d1:AddressLine1"))
  
  address_line_2 <- xml_text(xml_find_first(site_address_nodeset, "./d1:AddressLine2"))
  
  street_alias <- xml_text(xml_find_first(site_address_nodeset, "./d1:StreetAlias"))
  
  city <- xml_text(xml_find_first(site_address_nodeset, "./d1:City"))
  
  state <- xml_text(xml_find_first(site_address_nodeset, "./d1:State"))
  
  zip_code <- xml_text(xml_find_first(site_address_nodeset, "./d1:ZIPCode"))
  
  house_num <- xml_text(xml_find_first(site_address_nodeset, "./d1:HouseNumber"))
  
  prefix_dir <- xml_text(xml_find_first(site_address_nodeset, "./d1:PrefixDirection"))
  
  street_name <- xml_text(xml_find_first(site_address_nodeset, "./d1:StreetName"))

  street_type <- xml_text(xml_find_first(site_address_nodeset, "./d1:StreetType"))
  
  return_df <- tibble(address_line_1, address_line_2, street_alias, city, state, zip_code, house_num, prefix_dir, street_name, street_type)
  
  colnames(return_df) <- paste0("site_address_", colnames(return_df))
  
  return_df <- return_df |>
    mutate(no_physical_address = NA_character_)
  
  return(return_df)
  
}


### I'm only dealing with the first owner FFS
## that's all that's presented in PDFs anyway


process_owner_and_address_info <- function(owner_and_address_info_nodeset) {

  owner_mailing_address_df <- process_address(xml_find_first(owner_and_address_info_nodeset, ".//d1:MailingAddress"))

  colnames(owner_mailing_address_df) <- paste0("owner_mailing_", colnames(owner_mailing_address_df))
  
  
  owner_nodeset <- xml_find_first(owner_and_address_info_nodeset, ".//d1:Owner")
  
  if(length(xml_find_all(owner_nodeset, ".//d1:Individual")) > 0) {
    
    owner_type <- "Individual"
    
    owner_nodeset <- xml_find_first(owner_nodeset, ".//d1:Individual")

    
  } else if(length(xml_find_all(owner_nodeset, ".//d1:CombinedName")) > 0) {
    
    owner_type <- "Combined"
    
    owner_nodeset <- xml_find_first(owner_nodeset, ".//d1:CombinedName")  
    
  } else if(length(xml_find_all(owner_nodeset, ".//d1:Business")) > 0) {
    
    owner_type <- "Business"
    
    owner_nodeset <- xml_find_first(owner_nodeset, ".//d1:Business")
    
  }
  
  owner_second_address_df <- process_address(xml_find_first(owner_nodeset, ".//d1:Address"))
  
  colnames(owner_second_address_df) <- paste0("owner_second_", colnames(owner_second_address_df))
  
  owner_name <- process_owner_name(xml_find_first(owner_nodeset, ".//d1:Name"), owner_type)
  
  owner_and_address_info_df <- tibble(owner_type, owner_name)
  
  ### THIS LOOKS WRONG
  
  
  site_address_df <- if(length(xml_find_all(owner_and_address_info_nodeset, ".//d1:SiteAddress")) > 0) {
    
    process_site_address(xml_find_first(owner_and_address_info_nodeset, ".//d1:SiteAddress"))
    
  } else {
    
    return_df <- tibble(address_line_1 = NA_character_, address_line_2 = NA_character_, street_alias = NA_character_, city = NA_character_,
                        state = NA_character_, zip_code = NA_character_, house_num = NA_character_, prefix_dir = NA_character_, 
                        street_name = NA_character_, street_type = NA_character_)
    
    colnames(return_df) <- paste0("site_address_", colnames(return_df))
    
    return_df <- return_df |>
      mutate(no_physical_address = xml_text(owner_and_address_info_nodeset, ".//d1:NoPhysicalAddress"))
    
  }
  
  owner_and_address_info_df <- bind_cols(owner_and_address_info_df, owner_mailing_address_df, owner_second_address_df, site_address_df)
  
  return(owner_and_address_info_df)
  
}



## JURISDICTION FUNCTIONS

process_county <- function(county_nodeset) {
  
  county_name <- xml_text(xml_find_first(county_nodeset, ".//d1:CountyName"))
  
  county_rate <- xml_double(xml_find_first(county_nodeset, ".//d1:CountyRate"))
  
  county_tax <- xml_double(xml_find_first(county_nodeset, ".//d1:CountyTax"))
  
  county_df <- tibble(county_name, county_rate, county_tax)
  
  return(county_df)
  
}

process_municipality <- function(municipality_nodeset) {
  
  muni_name <- xml_text(xml_find_first(municipality_nodeset, ".//d1:MuniName"))
  
  muni_number <- xml_integer(xml_find_first(municipality_nodeset, ".//d1:MuniNumber"))
  
  municipal_rate <- xml_double(xml_find_first(municipality_nodeset, ".//d1:MunicipalRate"))
  
  municipal_tax <- xml_double(xml_find_first(municipality_nodeset, ".//d1:MunicipalTax"))
  
  municipality_df <- tibble(muni_name,  muni_number, municipal_rate, municipal_tax)
  
  return(municipality_df)
  
}

process_coderatetax <- function(code_rate_tax_nodeset) {
  
  code <- xml_text(xml_find_first(code_rate_tax_nodeset, ".//d1:Code"))
  
  rate <- xml_double(xml_find_first(code_rate_tax_nodeset, ".//d1:Rate"))
  
  tax <- xml_double(xml_find_first(code_rate_tax_nodeset, ".//d1:Tax"))
  
  code_rate_tax_df <- tibble(code, rate, tax)
  
  return(code_rate_tax_df)
  
}

# this is messy and reads all of the special districts together--not clear if any place will have more than one...

process_special_districts <- function(special_district_nodesets) {
  
  special_district_code <- xml_text(xml_find_all(special_district_nodesets, ".//d1:Code"))
  
  special_district_rate <- xml_double(xml_find_all(special_district_nodesets, ".//d1:Rate"))
  
  special_district_tax <- xml_double(xml_find_all(special_district_nodesets, ".//d1:Tax"))
  
  special_district_df <- tibble(special_district_code,  special_district_rate,  special_district_tax)
  
  return(special_district_df)
  
}

# NOT INCLUDE TIDs FOR NOW SINCE THEY DON'T HAVE TAX RATES

process_jurisdiction_info <- function(jurisdiction_nodeset) {
  
  county_df <- process_county(xml_find_first(jurisdiction_nodeset, ".//d1:County"))
  
  municipality_df <- process_municipality(xml_find_first(jurisdiction_nodeset, ".//d1:Municipality"))
  
  school_df <- process_coderatetax(xml_find_first(jurisdiction_nodeset, ".//d1:School"))
  
  colnames(school_df) <- paste0("school_", colnames(school_df))
  
  union_hs_df <- process_coderatetax(xml_find_first(jurisdiction_nodeset, ".//d1:UnionHS"))
  
  colnames(union_hs_df) <- paste0("union_hs_", colnames(union_hs_df))
  
  tech_df <- process_coderatetax(xml_find_first(jurisdiction_nodeset, ".//d1:Tech"))
  
  colnames(tech_df) <- paste0("tech_", colnames(tech_df))
  
  special_district_df <- process_special_districts(xml_find_all(jurisdiction_nodeset, ".//d1:SpecialDistrict"))
  
  jurisdiction_df <- bind_cols(county_df, municipality_df,  school_df, union_hs_df, tech_df, special_district_df)
    
  return(jurisdiction_df)
  
}

## TAX SUMMARY FUNCTIONS

process_tax_summary <- function(tax_summary_nodeset) {
  
  land_tax_total  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:LandTaxableTotal"))
  improvements_tax_total  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:ImprovementsTaxableTotal"))
  total_tax_value  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:TotalTaxableValue"))
  est_fair_market_value  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:EstimatedFairMarketValue"))
  state_tax  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:StateTax"))
  occupational_tax  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:OccupationalTax"))
  forest_taxable  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:ForestTaxable"))
  BOR_value  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:BORValue"))
  private_forest_crop_tax  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:PrivateForestCropTax"))
  managed_forest_tax  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:ManagedForestLawTax"))
  tax_total  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:TaxTotal"))
  school_credit <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:SchoolCredit"))
  lottery_credit  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:LotteryCredit"))
  lottery_count  <- xml_integer(xml_find_first(tax_summary_nodeset, ".//d1:LotteryCount"))
  lottery_credit_pass_fail  <- xml_text(xml_find_first(tax_summary_nodeset, ".//d1:LotteryCreditPassFail"))
  first_dollar_credit  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:FirstDollarCredit"))
  net_tax  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:NetTax"))
  payment  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:Payment"))
  amount_due  <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:AmountDue"))
  
  
  delq_utilities <- xml_double(xml_find_first(tax_summary_nodeset, ".//d1:DelinquentUtilityCharges"))
  
  special_assessments <- xml_find_all(tax_summary_nodeset, ".//d1:SpecialAssessment")
  
  num_special_assessments <- length(special_assessments)
  
  special_assessment_descs <- xml_text(xml_find_all(special_assessments, ".//d1:SpecialAssessmentDesc"))
  
  special_assessment_amts <- xml_double(xml_find_all(special_assessments, ".//d1:SpecialAssessmentCharge"))
  
  ## MAY NEED TO ALTER IF num_special_assessments = 0, use above code to tell if NULL 
  
  special_assessment_descs_cols <- purrr::map_vec(1:6, ~ if_else(.x <= num_special_assessments, special_assessment_descs[.x], NA_character_))
  names(special_assessment_descs_cols) <- paste0("special_assessment_desc", paste0("_", as.character(seq(1, 6))))
  
  special_assessment_amts_cols <-  purrr::map_vec(1:6, ~ if_else(.x <= num_special_assessments, special_assessment_amts[.x], NA_real_))
  names(special_assessment_amts_cols) <- paste0("special_assessment_amt", paste0("_", as.character(seq(1, 6))))
  

  tax_summary_df <- tibble(land_tax_total, improvements_tax_total, total_tax_value, est_fair_market_value,
                                  state_tax, occupational_tax, forest_taxable, BOR_value, private_forest_crop_tax, 
                                  managed_forest_tax, tax_total, school_credit,
                                  lottery_credit, lottery_count, lottery_credit_pass_fail,
                                  first_dollar_credit, net_tax, payment, amount_due, delq_utilities)
  
  tax_summary_df <- bind_cols(tax_summary_df, 
                              tibble::as_tibble_row(special_assessment_descs_cols),
                              tibble::as_tibble_row(special_assessment_amts_cols)
  )
                                                                                                                                                                                 
  
}



## tax districts here: https://www.revenue.wi.gov/DOR%20Publications/taxdist.pdf

## https://www.revenue.wi.gov/Pages/Schools/Home.aspx


