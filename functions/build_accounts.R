
# This function build IO industry and final demand sector names as AT_S1 and AT_FDS1, respectively
build_sector_names <- function(path_to_gross_energy_accounts){
  
  # Reading raw energy extension vectors
  FE_raw_dt <- data.table::fread(paste0(path_to_gross_energy_accounts, "IOT_1995_ixi/gross_energy_use.tsv"))
  FE_Y_raw_dt <- data.table::fread(paste0(path_to_gross_energy_accounts, "IOT_1995_ixi/gross_energy_use_Y.tsv"))
  
  # Two components of sector names (Country and Sector)
  numeric_sector_list <- rep(1:163, 49)
  numeric_fd_sector_list <- rep(1:7, 49)
  
  # Industry names
  industry_names_dt <- data.table::transpose(FE_raw_dt[1, c(5:7991)])[, Sector := stringr::str_c("S", numeric_sector_list)][
    , sec_names := stringr::str_c(V1, Sector, sep = "_")]
  
  # Final demand sector names
  fd_sector_names_dt <- data.table::transpose(FE_Y_raw_dt[1, c(5:347)])[, Sector := stringr::str_c("FDS", numeric_fd_sector_list)][
    , sec_names := stringr::str_c(V1, Sector, sep = "_")]
  
  # Binding
  all_sector_names <- rbind(industry_names_dt, fd_sector_names_dt)

  return(all_sector_names$sec_names)
}


# This function loads the energy mask contained in a given file
load_mask <- function(maskFile){
  
  eMask <- data.table::as.data.table(openxlsx::read.xlsx(maskFile, sep = " "))[
    , melt(.SD, id.vars = "X1", variable.name = "Flow", value.name = "MaskVal")][
      , setnames(.SD, "X1", "Product")]
  return(eMask)
}

# Loads and cleans the country concordance table
# Also add a World row for convenience purposes
load_country_concordance <- function(file){
  # Reading data and keeping only relevant columns
  country_data <- data.table::fread(file, skip = 0, header = TRUE)[, .(IEA_latest, ISO3, EXIO3)]
  
  # Adding world data
  spec_country_data <- rbind(country_data, list("World", "World", NA))
  return(spec_country_data)
}


# Reads and cleans up the gross energy accounts
read_ge_accounts <- function(path_to_gea,
                             sector_names,
                             country_concordance,
                             years){

  gea_list <-  setNames(vector("list", length(years)), as.character(years))
  
  class_gea_Z <- c(rep(NA, 4), rep("numeric", 7987))
  class_gea_Y <- c(rep(NA, 4), rep("numeric", 343))
  
  for (currYear in years){
    print(currYear)
    # Gross energy accounts Z matrix
    gea_Z_dt <- data.table::fread(paste0(path_to_gea, "IOT_", currYear, "_ixi/gross_energy_use.tsv"),
                                     skip = 2, colClasses = class_gea_Z, header = TRUE)
    # Gross energy accounts Y matrix
    gea_Y_dt <- data.table::fread(paste0(path_to_gea, "IOT_", currYear, "_ixi/gross_energy_use_Y.tsv"),
                                  skip = 2, colClasses = class_gea_Y, header = TRUE)
    # Merging both and operating changes
    gea_temp_dt <- merge(gea_Z_dt, gea_Y_dt, by = c("ISO3_country", "Flow", "IEA_product", "Unit"), all = TRUE)[
                                       , Year := currYear][
                                       , setcolorder(.SD, c("ISO3_country", "Flow", "IEA_product", "Year", "Unit"))][
                                       , setnames(.SD, c("ISO3_country", "Flow", "Product", "Year", "Unit", sector_names))][
                                         , setnafill(.SD, type = "const", fill = 0, nan = NA, cols = sector_names)][
                                           , melt(.SD, id.vars = c("ISO3_country", "Flow", "Product", "Year", "Unit"), variable.name = "Sector", value.name = "Eval")][
                                             Eval != 0, ]

    gea_list[[as.character(currYear)]] <- gea_temp_dt
    
    rm(gea_temp_dt)
    gc()
  }
  
  # Bind and return
  gea_raw_dt <- data.table::rbindlist(gea_list, use.names = TRUE)
  
  # Matches ISO3 country to IEA country
  gea_dt <- country_concordance[gea_raw_dt, on = .(ISO3 = ISO3_country)][
    , setcolorder(.SD, c("ISO3", "IEA_latest", "EXIO3", "Flow", "Product", "Year", "Unit", "Sector", "Eval"))][, Year := as.factor(Year)]
  
  # Check all flows got their IEA country match
  unMatchedRows <- gea_raw_dt[!country_concordance, on = .(ISO3_country = ISO3)]
  assertthat::assert_that(nrow(unMatchedRows) == 0, msg = "Some gross energy flows rows are not matched to an IEA country.")
  
  # Returns result
  return(gea_dt)
}


# This function applies a mask to the gross energy accounts and filters accordingly
apply_mask_to_gea <- function(gea_dt,
                              account,
                              etype,
                              e_mask){
  a <- e_mask[gea_dt, on = .(Product, Flow)][
    MaskVal == 1,][
    , MaskVal := NULL][
      , Account := account
    ][
      , eQuant := etype][, setcolorder(.SD, c("ISO3", "IEA_latest", "EXIO3", "Account", "eQuant", "Flow", "Product", "Year", "Unit"))]
  
  # Filtering out negative values for final energy accounts
  if (account %in% c("Final energy", "Final energy; Energy industry own use", "Non-energy uses", "Energy combustion")){
    a <- a[Eval >= 0,]
  }
  return(a)
}


# This function calculates the share of a given product in the total final energy consumption of a country
# This is helpful to quantify the relevance of the products that get renamed when calculating the useful stage accounts
# with calc_useful_stage_accounts() function
calc_share_products_in_tfe <- function(fe_account_dt,
                                       products){
  
  # Determining contribution of each product
  product_contribution <- fe_account_dt[Product %in% products,][
    , lapply(.SD, sum), .SDcols = "Eval", by = c("EXIO3", "Year", "Unit", "Product")
  ]
  
  # Determining total final energy
  total_fec <- fe_account_dt[ , lapply(.SD, sum), .SDcols = "Eval", by = c("EXIO3", "Year", "Unit")]
  
  # Determining and returning share
  share_products_in_tfe <- product_contribution[total_fec, on = .(EXIO3, Year, Unit)][, share := Eval / i.Eval][
    Product != "",]
  
  return(share_products_in_tfe)
}


# This function binds accounts together
bind_accounts <- function(list_accounts){
  bound_accounts <- rbindlist(list_accounts, use.names = TRUE)
  return(bound_accounts)
}


# Aggregates energy accounts using
# a character string vector argument "by" informing on groups to use
# and a character string vector "colsToAgg" informing on the name of columns to aggregate 
agg_accounts_by <- function(eAccounts,
                            colsToAgg,
                            by){
  agg_accounts <- eAccounts[, lapply(.SD, sum), .SDcol = colsToAgg, by = by]
  return(agg_accounts)
}


# This function loads the final energy to exergy multipliers
load_Ef_to_Xf_multipliers <- function(file){
  ef_to_xf_mult <- data.table::fread(file, skip = 0, header = TRUE)[IEA.country.name == "World", .(Product, `1995`)][
    , unique(.SD, by = c("Product", "1995"))
  ][, setnames(.SD, "1995", "PhiVal")]
}


# This function converts final stage energy accounts into final stage exergy accounts
# New function for which all final stage accounts are provided at once and specified in the "Account" and "eQuant" columns
convert_fea_to_X_bis <- function(fea_dt,
                                 ef_to_xf_mult,
                                 etype,
                                 eval_colnames){
  
  fea_X <- ef_to_xf_mult[fea_dt, on = .(Product)][
    , (eval_colnames) := lapply(.SD, function(x) x * PhiVal), .SDcols = (eval_colnames)
  ][
    , eQuant := etype][
      , PhiVal := NULL][, setcolorder(.SD, c("ISO3", "IEA_latest", "EXIO3", "Account", "eQuant", "Flow", "Product", "Year", "Unit", "Sector"))] # Remove "Sector" if sectors are in columns
  
  # Check that all final energy flows get their PhiVal
  a <- fea_dt[!ef_to_xf_mult, on = .(Product)]
  assertthat::assert_that(nrow(a) == 0, msg = "Some final energy flows do not get their PhiVal.")
  
  return(fea_X)
}


# This function loads final-to-useful energy and exergy multipliers
load_fu_multipliers <- function(files){

  # Initialisation
  multipliers <- vector("list", length(files))
  i <- 1
  
  # Looping through files
  for (file in files){
    # Extracting type of multiplier
    #mult_value <- stringr::str_extract(file, "exiobase_.*") |> stringr::str_remove_all(c("exiobase_|_multipliers.csv"))
    mult_value <- stringr::str_extract(file, "multipliers/.*") |> stringr::str_remove_all(c("multipliers/|_multipliers.csv"))
    
    # Loading, melting, and sorting data
    multipliers[[i]] <- data.table::fread(file, skip = 0, header = TRUE)[
      , melt(.SD, id.vars = c("IEA.country.name", "Flow", "Product"), variable.name = "Year", value.name = "mult")
    ][!is.na(mult),][
      , Flow := stringi::stri_replace_all_fixed(Flow, "Total energy supply; ", "")
    ][, Mtype := mult_value][
        , setcolorder(.SD, c("IEA.country.name", "Flow", "Product", "Year", "Mtype"))][
          , setnames(.SD, c("IEA_latest", "Flow", "Product", "Year", "Mtype", "mult"))
        ]
    
    # Incrementing
    i <- i+1
  }
  
  fu_multipliers <- rbindlist(multipliers, use.names = TRUE)
  return(fu_multipliers)
}


# This function calculates the useful stage energy accounts
calc_useful_stage_accounts <- function(fe_accounts,
                                       mults,
                                       eval_columns, # If code is run with wide format, i.e. each sector contained in a column, value should be sector_names
                                       # New account name
                                       target_account,
                                       # Arguments to filter final energy accounts
                                       account,
                                       etype,
                                       # Argument to filter multipliers
                                       mtype){
  
  # (0) Filtering the accounts received as argument to keep only relevant portion
  filtered_accounts <- fe_accounts[Account == account & eQuant == etype & !(stringr::str_detect(Flow, "bunkers")),]
  filtered_bunkers <- fe_accounts[Account == account & eQuant == etype & (stringr::str_detect(Flow, "bunkers")),]
  
  # (1) Preparing all multipliers to be used, in right order
  mult_etype_specific <- mults[Mtype == mtype,]
  multipliers_world_sec_specific <- mult_etype_specific[IEA_latest == "World",][, IEA_latest := NULL]
  multipliers_country_EW <- mult_etype_specific[Flow == "Economy-wide",][, Flow := NULL]
  multipliers_world_EW <- mult_etype_specific[IEA_latest == "World" & Flow == "Economy-wide",][, `:=` (IEA_latest = NULL, Flow = NULL)]

  # (2) Matching both Country and Sector, and picking up UnMatched final energy flows 
  FirstMatch <- mult_etype_specific[filtered_accounts, on = .(IEA_latest, Flow, Product, Year), nomatch = NULL][
    , setcolorder(.SD, c("ISO3", "IEA_latest", "EXIO3"))
  ]
  unMatched_fe1 <- filtered_accounts[!mult_etype_specific, on = .(IEA_latest, Flow, Product, Year)]
  gc()
  
  # (3) Matching with Country and Economy-wide multipliers, and picking up UnMatched final energy flows 
  SecondMatch <- multipliers_country_EW[unMatched_fe1, on = .(IEA_latest, Product, Year), nomatch = NULL]
  unMatched_fe2 <- unMatched_fe1[!multipliers_country_EW, on = .(IEA_latest, Product, Year)]
  gc()
  
  # (4) Matching with World and Economy-wide multipliers, and picking up UnMatched final energy flows 
  ThirdMatch <- multipliers_world_EW[unMatched_fe2, on = .(Product, Year), nomatch = NULL]
  remainingUnmatched <- unMatched_fe2[!multipliers_world_EW, on = .(Product, Year)]
  gc()
  
  # (5) Testing all final energy flows (excluding bunkers) get their multiplier
  if (nrow(remainingUnmatched) > 0){
    years_issues <- unique(remainingUnmatched$Year)
  }
  assertthat::assert_that(nrow(remainingUnmatched) == 0, msg = paste0("Some final energy flows are not matched any multiplier in years: ", years_issues))
  
  # (6.a) Match bunkers with their corresponding multiplier
  BunkersMatch <- multipliers_world_sec_specific[filtered_bunkers, on = .(Flow, Product, Year), nomatch = NULL]
  unMatched_bunkers <- filtered_bunkers[!multipliers_world_sec_specific, on = .(Flow, Product, Year)]
  
  # (6.b) Matching unmatched bunkers to the World and economy-wide multipliers
  BunkersMatchEW <- multipliers_world_EW[unMatched_bunkers, on = .(Product, Year), nomatch = NULL]
  unMatched_bunkers_after_EW <- unMatched_bunkers[!multipliers_world_EW, on = .(Product, Year)]
  
  # (7) Check that all bunkers get their multiplier
  if (nrow(unMatched_bunkers_after_EW) > 0){
    years_issues_bk <- unique(unMatched_bunkers_after_EW$Year)
  }
  assertthat::assert_that(nrow(unMatched_bunkers_after_EW) == 0, msg = paste0("Some final energy flows (bunkers) are not matched any multiplier in years: ", years_issues_bk))
  
  # (8) Binding and applying multipliers
  all_flows <- rbindlist(list(FirstMatch, SecondMatch, ThirdMatch, BunkersMatch, BunkersMatchEW), use.names = TRUE, fill = TRUE)
  gc()
  
  res <- all_flows[
    , (eval_columns) := lapply(.SD, function(x) x * mult), .SDcols = (eval_columns)
  ][, `:=` (Mtype = NULL, mult = NULL)][
    , Account := target_account
  ][
    # If sectors contained in different columns, "Sector" to be removed
    , setcolorder(.SD, c("ISO3", "IEA_latest", "EXIO3", "Account", "eQuant", "Flow", "Product", "Year", "Unit", "Sector")) 
  ]
  return(res)
}


# This function calculates the share of unmatched final energy for each matching step;
# and returns the share of unmatched final energy in each year, both in terms of total and per EXIO3 region
calc_share_unmatched_fec <- function(fe_accounts,
                                     mults,
                                     eval_columns,
                                     # New account name
                                     target_account,
                                     # Arguments to filter final energy accounts
                                     account,
                                     etype,
                                     # Argument to filter multipliers
                                     mtype){
  
  # (0) Filtering the accounts received as argument to keep only relevant portion
  filtered_accounts_inc_bks <- fe_accounts[Account == account & eQuant == etype,]
  filtered_accounts <- fe_accounts[Account == account & eQuant == etype & !(stringr::str_detect(Flow, "bunkers")),]
  filtered_bunkers <- fe_accounts[Account == account & eQuant == etype & (stringr::str_detect(Flow, "bunkers")),]
  
  # (1) Preparing all multipliers to be used, in right order
  mult_etype_specific <- mults[Mtype == mtype,]
  multipliers_world_sec_specific <- mult_etype_specific[IEA_latest == "World",][, IEA_latest := NULL]
  multipliers_country_EW <- mult_etype_specific[Flow == "Economy-wide",][, Flow := NULL]
  multipliers_world_EW <- mult_etype_specific[IEA_latest == "World" & Flow == "Economy-wide",][, `:=` (IEA_latest = NULL, Flow = NULL)]
  
  # (2) Matching both Country and Sector, and picking up UnMatched final energy flows 
  FirstMatch <- mult_etype_specific[filtered_accounts, on = .(IEA_latest, Flow, Product, Year), nomatch = NULL][
    , setcolorder(.SD, c("ISO3", "IEA_latest", "EXIO3"))
  ]
  unMatched_fe1 <- filtered_accounts[!mult_etype_specific, on = .(IEA_latest, Flow, Product, Year)][, UnmatchNumber := 1]
  gc()
  
  # (3) Matching with Country and Economy-wide multipliers, and picking up UnMatched final energy flows 
  SecondMatch <- multipliers_country_EW[unMatched_fe1, on = .(IEA_latest, Product, Year), nomatch = NULL]
  unMatched_fe2 <- unMatched_fe1[!multipliers_country_EW, on = .(IEA_latest, Product, Year)][, UnmatchNumber := 2]
  gc()
  
  # (4) Matching with World and Economy-wide multipliers, and picking up UnMatched final energy flows 
  ThirdMatch <- multipliers_world_EW[unMatched_fe2, on = .(Product, Year), nomatch = NULL]
  remainingUnmatched <- unMatched_fe2[!multipliers_world_EW, on = .(Product, Year)][, UnmatchNumber := 3]
  gc()
  
  # (5) Matching bunkers with World and bunkers-specific multipliers, and picking up Unmatched final energy flows
  BunkersMatch <- multipliers_world_sec_specific[filtered_bunkers, on = .(Flow, Product, Year), nomatch = NULL]
  unMatched_bunkers <- filtered_bunkers[!multipliers_world_sec_specific, on = .(Flow, Product, Year)][, UnmatchNumber := 1]
  
  # (6) Matching unmatched bunkers with World and economy-wide multipliers, and picking up Unmatched final energy flows
  BunkersMatchEW <- multipliers_world_EW[unMatched_bunkers, on = .(Product, Year), nomatch = NULL]
  remaining_unMatched_bunkers <- unMatched_bunkers[!multipliers_world_EW, on = .(Product, Year)][, UnmatchNumber := 3]
  
  # (7) Binding all umatched
  all_unmatched <- rbindlist(list(unMatched_fe1, unMatched_fe2, remainingUnmatched, unMatched_bunkers, remaining_unMatched_bunkers),
                             use.names = TRUE)
  
  # (8) Sum unmatched by year and compare with TFC
  sum_unmatched_by_year <- all_unmatched[, .(SumUnmatched = sum(Eval)), by = c("eQuant", "Year", "Unit", "UnmatchNumber")]
  sum_fec_by_year <- filtered_accounts_inc_bks[, .(SumFEC = sum(Eval)), by = c("eQuant", "Year", "Unit")]
  share_Unmatched_by_Year <- sum_fec_by_year[sum_unmatched_by_year, on = .(eQuant, Year, Unit)][, Share := SumUnmatched / SumFEC][, EXIO3 := "Total"]
  
  # (9) Sum unmatched by year and EXIO3 country and compare with TFC
  sum_unmatched_by_country_year <- all_unmatched[, .(SumUnmatched = sum(Eval)), by = c("eQuant", "EXIO3", "Year", "Unit", "UnmatchNumber")]
  sum_fec_by_country_year <- filtered_accounts_inc_bks[, .(SumFEC = sum(Eval)), by = c("eQuant", "EXIO3", "Year", "Unit")]
  share_Unmatched_by_Country_Year <- sum_fec_by_country_year[sum_unmatched_by_country_year, on = .(EXIO3, eQuant, Year, Unit)][, Share := SumUnmatched / SumFEC]
  
  # (10) Binding all shares unmatched together
  all_shares_unmatched <- data.table::rbindlist(list(share_Unmatched_by_Year, share_Unmatched_by_Country_Year), use.names = TRUE)
  
  # Returning shares unmatched final energy consumption
  return(all_shares_unmatched)
}


# Expands the energy footprints calculated to a data.table
expand_e_footprints <- function(e_footprint_mats,
                                evecs,
                                country_names,
                                years){

  # Creating empty structure for each year
  efootprint_dt_struct <- evecs[Year == min(years), .SD, .SDcols = c("Account", "eQuant")]
  
  # Initialising list for collectiong values
  list_e_footprint <-  setNames(vector("list", length(years)), as.character(years))

  # Looping and filling in energy footprit list
  for (currYear in years){
    list_e_footprint[[as.character(currYear)]] <- data.table::copy(efootprint_dt_struct)[
      , `:=` (Year = as.factor(currYear), Unit = "TJ")]
    list_e_footprint[[as.character(currYear)]][, (country_names) := as.data.table(as.matrix(e_footprint_mats[[as.character(currYear)]]))]
  }
  
  # Names of variables in basic structure
  id_footprint_str <- c("Account", "eQuant", "Year", "Unit")
  
  # Binding all list items together, and processing
  e_footprints_dt <- data.table::rbindlist(list_e_footprint, use.names = TRUE)[
    , data.table::melt(.SD, id.vars = id_footprint_str, measure.vars = country_names, variable.name = "EXIO3.Region", value.name = "Footprint_excl_fd")]
  
  return(e_footprints_dt)
}


# This function calculates the final production-based and consumption-based accounts
build_pba_cba <- function(eFootprints_dt,
                          eAccounts_dt,
                          fds_names){

  # Set up column ids for melting and aggregations
  id_cols_aggregation <- c("Account", "eQuant", "EXIO3.Region", "Year", "Unit")
  
  # Preparing final demand energy use accounts
  e_Y_dt <- eAccounts_dt[Sector %in% fds_names,][
    , EXIO3.Region := sub("_.*", "", Sector)][
      , .(e_Y = sum(Eval)), by = id_cols_aggregation]
  
  e_PBA_dt <- eAccounts_dt[, EXIO3.Region := sub("_.*", "", Sector)][
    , .(PBA = sum(Eval)), by = id_cols_aggregation]
  
  # Joining all PBA accounts (those part of final demand and part of intermediate production)
  all_ePBA_dt <- e_Y_dt[e_PBA_dt, on = (id_cols_aggregation)]
  
  # Now, joining all three accounts
  eAccounts_joined <- all_ePBA_dt[eFootprints_dt, on = (id_cols_aggregation)]
  eAccounts_joined[is.na(e_Y), e_Y := 0]
  eAccounts_joined[is.na(PBA), PBA := 0]
  eAccounts_joined[is.na(Footprint_excl_fd), Footprint_excl_fd := 0]
  
  # Calculating CBA, and removing auxiliary columns
  eAccounts_by_country <- eAccounts_joined[
    , CBA := Footprint_excl_fd + e_Y][, `:=` (Footprint_excl_fd = NULL, e_Y = NULL)]

  return(eAccounts_by_country)
}


cast_accounts_to_wide <- function(accounts_dt,
                                  account_cols){
  # Casting energy accounts to wide
  eAccountsWide <- accounts_dt[, dcast(.SD, Account + eQuant + Year + Unit ~ Sector, value.var = "Eval", fill = 0)]
  
  # Check which columns are missing and add them
  missing_cols <- setdiff(account_cols, names(eAccountsWide))
  
  if (length(missing_cols)) {
    eAccountsWide[, (missing_cols) := 0]
  }
  
  setcolorder(eAccountsWide, c("Account", "eQuant", "Year", "Unit", account_cols))
  
  # Check we have the sectors contained in the columns of accounts_efi_wide match expected sectors, in the expected order
  obtained_cols <- colnames(eAccountsWide[, ..account_cols])
  assertthat::assert_that(isTRUE(all(obtained_cols == account_cols)))
  
  return(eAccountsWide)
}


# Compares the final energy accounts from EXIOBASE to the IEA's Total final consumption
compare_fue_to_tfc <- function(fe_accounts,
                               path_to_iea_webs,
                               country_concordance){
  # Flows to include/exclude
  flows_exclude_exio <- c("Total final consumption; Transport; International aviation bunker", 
                          "Total final consumption; Transport; International marine bunkers",
                          "Total final consumption; Transport; World aviation bunkers",
                          "Total final consumption; Transport; World marine bunkers")
  
  flows_to_keep_iea <- c("Energy industry own use", "Industry", "Domestic aviation", "Domestic navigation", 
                         "Road", "Rail", "Pipeline transport", "Transport not elsewhere specified", "Residential",
                         "Commercial and public services", "Agriculture/forestry", "Fishing", "Final consumption not elsewhere specified")
  
  # Summing across each region, and filtering out RoW regions
  FEU_own_calcs_excl_bunkers <- fe_accounts[! Flow %in% flows_exclude_exio,][
    , lapply(.SD, sum), .SDcols = "Eval", by = c("EXIO3", "Year", "Unit")]
  
  # Reading the IEA's WEBs
  iea_raw_weeb <- data.table::fread(path_to_iea_webs, header = TRUE)
  
  # Keeping only relevant flows to exclude bunkers
  FEU_IEA_excl_bunkers <- iea_raw_weeb[(FLOW %in% flows_to_keep_iea) & PRODUCT == "Total", ][
    , .SD, .SDcols = c("COUNTRY", "FLOW", "PRODUCT", as.character(1990:2020))][
      , melt(.SD, id.vars = c("COUNTRY", "FLOW", "PRODUCT"), variable.name = "Year", value.name = "TFC")][
        , TFC := abs(as.numeric(TFC))][
          , TFC := fcoalesce(TFC, 0)][
            , lapply(.SD, sum), .SDcols = "TFC", by = c("COUNTRY", "Year")]
  
  # Prepare country concordance by removing RoW regions
  country_concordance_df <- country_concordance[IEA_latest != "",][! EXIO3 %in% c("WA", "WE", "WF", "WL", "WM"), ]
  
  # Bind the final energy totals from tge IEA to the exiobase region name
  FEU_IEA_excl_bunkers_wNames <- country_concordance_df[FEU_IEA_excl_bunkers, on = .(IEA_latest == COUNTRY)][! is.na(EXIO3),]
  
  # Now comparing the two accounts
  comp_EXIO_IEA <- FEU_IEA_excl_bunkers_wNames[FEU_own_calcs_excl_bunkers, on = .(EXIO3, Year)][
    , diff := abs(TFC - Eval) / TFC][
      , setorder(.SD, -diff)][
        !is.na(TFC), ]
  
  return(comp_EXIO_IEA)
}


# This function exports a file to csv
export_results_to_csv <- function(object, accounts_to_exclude, path){
  if (any(is.na(accounts_to_exclude))){
    dt_to_write <- object
  } else {
    dt_to_write <- object[!Account %in% accounts_to_exclude,]
  }
  data.table::fwrite(dt_to_write, path)
  return(path)
}

