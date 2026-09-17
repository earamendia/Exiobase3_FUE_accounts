library(targets)

source("setup.R")
source("functions/build_accounts.R")
source("functions/build_pxp.R")
source("functions/io_calcs.R")

# Specific options for workflow 
# 1) Packages we want to load
# 2) Method for errors (target will return NULL if errored)

list(
  
  # Pipeline set up ---------------------------------------------------------
  
  # Start building list of targets here
  # Path to gross energy accounts
  tar_target(
    path_to_gea,
    return(path_to_gross_energy_accounts_set)
  ),
  
  # Path to IEA WEBs
  tar_target(
    path_to_iea_webs,
    return(path_to_iea_webs_set)
  ),
  
  # Path to Exiobase
  tar_target(
    path_to_exiobase,
    return(path_to_exiobase_set)
  ),
  
  # Path to exported results
  tar_target(
    path_to_results,
    return(path_to_results_set)
  ),
  
  # Years to run pipeline for
  tar_target(
    years_pipeline,
    return(years_set)
  ),
  
  # Final energy mask file
  tar_target(
    file_final_energy_mask,
    here::here(path_to_fe_mask),
    format = "file"
  ),
  
  # Energy industry own use mask file
  tar_target(
    file_eiou_mask,
    here::here(path_to_eiou_mask),
    format = "file"
  ),
  
  # Transformation processes mask file
  tar_target(
    file_tp_mask,
    here::here(path_to_tp_mask),
    format = "file"
  ),
  
  # Energy losses mask file
  tar_target(
    file_energy_losses_mask,
    here::here(path_to_eloss_mask),
    format = "file"
  ),
  
  # Non-energy uses mask file
  tar_target(
    file_non_energy_use_mask,
    here::here(path_to_neu_mask),
    format = "file"
  ),
  
  # Energy combustion mask file
  tar_target(
    file_energy_combustion_mask,
    here::here(path_to_ecomb_mask),
    format = "file"
  ),
  
  # Building sector names
  tar_target(
    sector_names,
    build_sector_names(path_to_gea)
  ),
  
  # Building industry names
  tar_target(
    industry_names,
    return(sector_names[1:7987])
  ),
  
  # Building final demand sector names
  tar_target(
    fds_names,
    return(sector_names[7988:8330])
  ),
  
  # Extracting EXIOBASE country names
  tar_target(
    country_names,
    return(unique(sapply(strsplit(sector_names, "_"), "[", 1)))
  ),
  
  # Building product names
  tar_target(
    product_names,
    build_product_names(country_names)
  ),
  
  # Final energy to exergy multipliers
  tar_target(
    file_Ef_to_Xf_multipliers,
    here::here(path_to_ef_to_xf_multipliers),
    format = "file"
  ),
  
  # Final energy to useful energy multipliers
  tar_target(
    file_Ef_to_Eu_multipliers,
    here::here(path_to_ef_to_eu_multipliers),
    format = "file"
  ),
  
  # Final exergy to useful exergy multipliers
  tar_target(
    file_Xf_to_Xu_multipliers,
    here::here(path_to_xf_to_xu_multipliers),
    format = "file"
  ),
  
  # Final energy to energy losses multipliers
  tar_target(
    file_Ef_to_Eloss_multipliers,
    here::here(path_to_ef_to_eloss_multipliers),
    format = "file"
  ),
  
  # Final exergy to exergy losses multipliers
  tar_target(
    file_Xf_to_Xloss_multipliers,
    here::here(path_to_xf_to_xloss_multipliers),
    format = "file"
  ),
  
  # Country concordance table
  tar_target(
    file_country_concordance,
    here::here(path_to_country_concordance),
    format = "file"
  ),
  
  
  # Loading energy masks and country concordance -----------------------------------------------------
  
  # Loading in final energy mask
  tar_target(
    fe_mask,
    load_mask(maskFile = file_final_energy_mask)
  ),
  
  # Loading in energy industry own use mask
  tar_target(
    eiou_mask,
    load_mask(maskFile = file_eiou_mask)
  ),
  
  # Loading in non-energy uses mask
  tar_target(
    neu_mask,
    load_mask(maskFile = file_non_energy_use_mask)
  ),
  
  # Loading in energy losses mask
  tar_target(
    eloss_mask,
    load_mask(maskFile = file_energy_losses_mask)
  ),
  
  # Loading in energy combustion mask
  tar_target(
    ecomb_mask,
    load_mask(maskFile = file_energy_combustion_mask)
  ),
  
  # Loading in transformation processes mask
  tar_target(
    tp_mask,
    load_mask(maskFile = file_tp_mask)
  ),
  
  # Loading and cleaning country concordance
  tar_target(
    country_concordance,
    load_country_concordance(file = file_country_concordance)
  ),
  
  
  # Building final energy accounts by energy carrier, flow, and industry (IEA country) --------------------
  
  # Loading gross energy accounts (IEA country)
  tar_target(
    ge_accounts_iea_efi,
    read_ge_accounts(path_to_gea = path_to_gea,
                     sector_names = sector_names,
                     country_concordance = country_concordance,
                     years = years_pipeline)
  ),
  
  # Final energy accounts (energy carrier, flow, industry)
  tar_target(
    fe_accounts_iea_efi,
    apply_mask_to_gea(gea_dt = ge_accounts_iea_efi,
                      account = "Final energy",
                      etype = "E",
                      e_mask = fe_mask)
  ),
  
  # Energy industry own use (energy carrier, flow, industry)
  tar_target(
    eiou_accounts_iea_efi,
    apply_mask_to_gea(gea_dt = ge_accounts_iea_efi,
                      account = "Final energy; Energy industry own use",
                      etype = "E",
                      e_mask = eiou_mask)
  ),
  
  # Non-energy uses accounts (energy carrier, flow, industry)
  tar_target(
    neu_accounts_iea_efi,
    apply_mask_to_gea(gea_dt = ge_accounts_iea_efi,
                      account = "Non-energy uses",
                      etype = "E",
                      e_mask = neu_mask)
  ),

  # Energy losses accounts
  tar_target(
    eloss_accounts_iea_efi,
    apply_mask_to_gea(gea_dt = ge_accounts_iea_efi,
                      account = "T&D losses",
                      etype = "E",
                      e_mask = eloss_mask)
  ),

  # Energy combustion accounts
  tar_target(
    ecomb_accounts_iea_efi,
    apply_mask_to_gea(gea_dt = ge_accounts_iea_efi,
                      account = "Energy combustion",
                      etype = "E",
                      e_mask = ecomb_mask)
  ),
  
  # Binding all final energy accounts accounts
  tar_target(
    feAccounts_iea_efi,
    bind_accounts(list_accounts = list(fe_accounts_iea_efi, 
                                  eiou_accounts_iea_efi,
                                  neu_accounts_iea_efi,
                                  eloss_accounts_iea_efi,
                                  ecomb_accounts_iea_efi))
  ),
  
  # Determining the share of a given product in the total final energy accounts by country
  # This is helpful to quantify the relevance of a given product, for instance here "Other hydrocarbons"
  tar_target(
    share_products_in_tfe,
    calc_share_products_in_tfe(fe_accounts_iea_efi,
                              products = c("Other hydrocarbons"))
  ),
  
  # Building final exergy accounts ----------------------------------------------------------------------
  
  # Loading final energy to exergy multipliers
  tar_target(
    Ef_to_Xf_multipliers,
    load_Ef_to_Xf_multipliers(file = file_Ef_to_Xf_multipliers)
  ),
  
  # Building all exergy accounts at the final energy stage
  tar_target(
    fxAccounts_iea_efi,
    convert_fea_to_X_bis(fea_dt = feAccounts_iea_efi,
                         ef_to_xf_mult = Ef_to_Xf_multipliers,
                         etype = "X",
                         eval_colnames = "Eval")
  ),
  
  
  # Building useful energy accounts ---------------------------------------------------------------------
  
  # Loading multipliers: 
  tar_target(
    final_to_useful_multipliers,
    load_fu_multipliers(files = c(file_Ef_to_Eu_multipliers, file_Xf_to_Xu_multipliers,
                                  file_Ef_to_Eloss_multipliers, file_Xf_to_Xloss_multipliers))
  ),

  # Building useful energy accounts
  tar_target(
    ue_accounts_iea_efi,
    calc_useful_stage_accounts(fe_accounts = feAccounts_iea_efi,
                               mults = final_to_useful_multipliers,
                               eval_columns = "Eval",
                               # New account name
                               target_account = "Useful energy",
                               # Arguments to filter final energy accounts
                               account = "Final energy",
                               etype = "E",
                               # Argument to filter multipliers
                               mtype = "Ef_to_Eu")
  ),
  
  # Building useful energy for energy industry own use accounts
  tar_target(
    ue_eiou_accounts_iea_efi,
    calc_useful_stage_accounts(fe_accounts = feAccounts_iea_efi,
                               mults = final_to_useful_multipliers,
                               eval_columns = "Eval",
                               # New account name
                               target_account = "Useful energy; Energy industry own use",
                               # Arguments to filter final energy accounts
                               account = "Final energy; Energy industry own use",
                               etype = "E",
                               # Argument to filter multipliers
                               mtype = "Ef_to_Eu")
  ),
  
  # Building final-to-useful energy losses accounts
  tar_target(
    fu_eloss_accounts_iea_efi,
    calc_useful_stage_accounts(fe_accounts = feAccounts_iea_efi,
                               mults = final_to_useful_multipliers,
                               eval_columns = "Eval",
                               # New account name
                               target_account = "Final-to-useful energy losses",
                               # Arguments to filter final energy accounts
                               account = "Final energy",
                               etype = "E",
                               # Argument to filter multipliers
                               mtype = "Ef_to_Eloss")
  ),
  
  # Building useful exergy accounts
  tar_target(
    ux_accounts_iea_efi,
    calc_useful_stage_accounts(fe_accounts = fxAccounts_iea_efi,
                               mults = final_to_useful_multipliers,
                               eval_columns = "Eval",
                               # New account name
                               target_account = "Useful energy",
                               # Arguments to filter final energy accounts
                               account = "Final energy",
                               etype = "X",
                               # Argument to filter multipliers
                               mtype = "Xf_to_Xu")
  ),
  
  # Building useful exergy for energy industry own use accounts
  tar_target(
    ux_eiou_accounts_iea_efi,
    calc_useful_stage_accounts(fe_accounts = fxAccounts_iea_efi,
                               mults = final_to_useful_multipliers,
                               eval_columns = "Eval",
                               # New account name
                               target_account = "Useful energy; Energy industry own use",
                               # Arguments to filter final energy accounts
                               account = "Final energy; Energy industry own use",
                               etype = "X",
                               # Argument to filter multipliers
                               mtype = "Xf_to_Xu")
  ),
  
  # Building final-to-useful exergy losses accounts
  tar_target(
    fu_xloss_accounts_iea_efi,
    calc_useful_stage_accounts(fe_accounts = fxAccounts_iea_efi,
                               mults = final_to_useful_multipliers,
                               eval_columns = "Eval",
                               # New account name
                               target_account = "Final-to-useful energy losses",
                               # Arguments to filter final energy accounts
                               account = "Final energy",
                               etype = "X",
                               # Argument to filter multipliers
                               mtype = "Xf_to_Xloss")
  ),
  
  # Binding all energy accounts (energy vector, flow, industry)
  tar_target(
    eAccounts_iea_efi,
    bind_accounts(list_accounts = list(feAccounts_iea_efi,
                                       fxAccounts_iea_efi,
                                       ue_accounts_iea_efi,
                                       ue_eiou_accounts_iea_efi,
                                       fu_eloss_accounts_iea_efi,
                                       ux_accounts_iea_efi,
                                       ux_eiou_accounts_iea_efi,
                                       fu_xloss_accounts_iea_efi))
  ),
  
  # Calculating the share of final energy unmatched
  tar_target(
    share_unmatched_fec,
    calc_share_unmatched_fec(fe_accounts = feAccounts_iea_efi,
                             mults = final_to_useful_multipliers,
                             eval_columns = "Eval",
                             # New account name
                             target_account = "Useful energy",
                             # Arguments to filter final energy accounts
                             account = "Final energy",
                             etype = "E",
                             # Argument to filter multipliers
                             mtype = "Ef_to_Eu")
  ),
  

  # Determining all energy accounts by product (energy vector, flow, industry) -----------------------------------------------
  
  # Building list of product-shares matrices
  tar_target(
    Cmats,
    calc_C_mats(path = path_to_exiobase,
                country_list = country_names,
                industry_names = industry_names,
                product_names = product_names,
                years = years_pipeline)
  ),
  
  # Calculating final energy accounts by product
  tar_target(
    eAccounts_iea_efp,
    calc_accounts_efp(accounts_efi = eAccounts_iea_efi,
                      Cmats = Cmats,
                      sector_names = sector_names,
                      industry_names = industry_names,
                      product_names = product_names,
                      fds_names = fds_names,
                      years = years_pipeline)
  ),

 
  # Aggregating all accounts to EXIOBASE regions --------------------------------------------------------
  # In this section the different aggregations of the energy accounts are conducted
  
  # (1) Starting with energy accounts by industry
  # (1.i) Energy accounts by energy carrier and industry (IEA countries)
  tar_target(
    eAccounts_iea_ei,
    agg_accounts_by(eAccounts = eAccounts_iea_efi,
                    colsToAgg = "Eval",
                    by = c("ISO3", "IEA_latest", "EXIO3", "Account", "eQuant", "Product", "Year", "Unit", "Sector"))
  ),
  
  # (1.ii) Energy accounts by industry (IEA countries)
  tar_target(
    eAccounts_iea_i,
    agg_accounts_by(eAccounts = eAccounts_iea_efi,
                    colsToAgg = "Eval",
                    by = c("ISO3", "IEA_latest", "EXIO3", "Account", "eQuant", "Year", "Unit", "Sector"))
  ),
  
  # (1.iii) Energy accounts by energy carrier, flow, and industry (EXIOBASE countries)
  tar_target(
    eAccounts_exio_efi,
    agg_accounts_by(eAccounts_iea_efi,
                    colsToAgg = "Eval",
                    by = c("Account", "eQuant", "Flow", "Product", "Year", "Unit", "Sector"))
  ),
  
  # (1.iv) Energy accounts by energy carrier and industry (EXIOBASE countries)
  tar_target(
    eAccounts_exio_ei,
    agg_accounts_by(eAccounts_iea_efi,
                    colsToAgg = "Eval",
                    by = c("Account", "eQuant", "Product", "Year", "Unit", "Sector"))
  ),
  
  # (1.v) Energy accounts by industry (EXIOBASE countries)
  tar_target(
    eAccounts_exio_i,
    agg_accounts_by(eAccounts_iea_efi,
                    colsToAgg = "Eval",
                    by = c("Account", "eQuant", "Year", "Unit", "Sector"))
  ),
  
  # (1.vi) Cast energy accounts by industry (EXIOBASE countries) to wide format
  tar_target(
    eAccounts_exio_i_wide,
    cast_accounts_to_wide(eAccounts_exio_i,
                          account_cols = sector_names)
  ),
  
  # (2) Energy accounts by product
  # (2.i) Energy accounts by energy carrier and product (IEA countries)
  tar_target(
    eAccounts_iea_ep,
    agg_accounts_by(eAccounts = eAccounts_iea_efp,
                    colsToAgg = "Eval", #c(product_names, fds_names), # used to be before moving to long format
                    by = c("ISO3", "IEA_latest", "EXIO3", "Account", "eQuant", "Product", "Year", "Unit", "Sector"))
  ),

  # (2.ii) Energy accounts by industry (IEA countries)
  tar_target(
    eAccounts_iea_p,
    agg_accounts_by(eAccounts = eAccounts_iea_efp,
                    colsToAgg = "Eval",#c(product_names, fds_names),
                    by = c("ISO3", "IEA_latest", "EXIO3", "Account", "eQuant", "Year", "Unit", "Sector"))
  ),


  # (2.iii) Energy accounts by energy carrier, flow, and product (EXIOBASE countries)
  tar_target(
    eAccounts_exio_efp,
    agg_accounts_by(eAccounts_iea_efp,
                    colsToAgg = "Eval",#c(product_names, fds_names),
                    by = c("Account", "eQuant", "Flow", "Product", "Year", "Unit", "Sector"))
  ),


  # (2.iv) Energy accounts by energy carrier and product (EXIOBASE countries)
  tar_target(
    eAccounts_exio_ep,
    agg_accounts_by(eAccounts_iea_efp,
                    colsToAgg = "Eval",#c(product_names, fds_names),
                    by = c("Account", "eQuant", "Product", "Year", "Unit", "Sector"))
  ),

  # (2.v) Energy accounts by product (EXIOBASE countries)
  tar_target(
    eAccounts_exio_p,
    agg_accounts_by(eAccounts_iea_efp,
                    colsToAgg = "Eval",# c(product_names, fds_names),
                    by = c("Account", "eQuant", "Year", "Unit", "Sector"))
  ),
  
  # (2.vi) Cast energy accounts by product (EXIOBASE countries) to wide format
  tar_target(
    eAccounts_exio_p_wide,
    cast_accounts_to_wide(eAccounts_exio_p,
                          account_cols = c(product_names, fds_names))
  ),
  
  
  # Calculating energy extension vector and energy footprints -------------------------------------------
  
  # Building ixi IO matrices
  tar_target(
    IO_matrices_ixi,
    build_io_matrices(path_to_exiobase = path_to_exiobase,
                      aggregate_Y_mat = TRUE,
                      matdims = "ixi",
                      ind_prod_names = industry_names,
                      fds_names = fds_names,
                      years = years_pipeline)
  ),
  
  # Building industry-level energy extension vectors
  tar_target(
    eVecs_exio_i,
    calc_eVecs(eAccounts = eAccounts_exio_i,
               io_mats = IO_matrices_ixi,
               ind_prod_names = industry_names,
               fds_names = fds_names,
               years = years_pipeline)
  ),
  
  # Building pxp IO matrices
  tar_target(
    IO_matrices_pxp,
    build_io_matrices(path_to_exiobase = path_to_exiobase,
                      aggregate_Y_mat = TRUE,
                      matdims = "pxp",
                      ind_prod_names = product_names,
                      fds_names = fds_names,
                      years = years_pipeline)
  ),
  
  # Building product-level energy extension vectors
  tar_target(
    eVecs_exio_p,
    calc_eVecs(eAccounts = eAccounts_exio_p,
               io_mats = IO_matrices_pxp,
               ind_prod_names = product_names,
               fds_names = fds_names,
               years = years_pipeline)
  ),
  
  
  # Calculating all the energy footprints ---------------------------------------------------------------

  # Calculating the energy footprints by final demand sector
  tar_target(
    eFootprints_mats,
    calc_e_footprints(eVecs_exio_i,
                      io_mats = IO_matrices_ixi,
                      ind_prod_names = industry_names,
                      fds_names = fds_names,
                      years = years_pipeline)
  ),
  
  # Expanding the energy footprint to a data.table
  tar_target(
    eFootprints_dt,
    expand_e_footprints(eFootprints_mats,
                        evecs = eVecs_exio_i,
                        country_names = country_names,
                        years = years_pipeline)
  ),
  
  # Building data.table of PBA and CBA energy accounts by country
  tar_target(
    eAccounts_pcba,
    build_pba_cba(eFootprints_dt = eFootprints_dt,
                  eAccounts_dt = eAccounts_exio_i,
                  fds_names = fds_names)
  ),
  
  # Comparing final energy accounts from EXIOBASE to Total final consumption from the IEA's WEBs
  tar_target(
    comp_FUE_to_IEA,
    compare_fue_to_tfc(fe_accounts = fe_accounts_iea_efi,
                       country_concordance = country_concordance,
                       path_to_iea_webs = path_to_iea_webs)
  ),
  

  # Exporting all results ---------------------------------------------------
  
  # Energy accounts ixi
  tar_target(
    export_eAccounts_ixi,
    export_results_to_csv(object = eAccounts_exio_i_wide,
                          accounts_to_exclude = c("Energy combustion", "T&D losses"),
                          path = paste0(path_to_results, "energy_accounts_ixi.csv")),
    format = "file"
  ),
  
  # Energy accounts pxp
  tar_target(
    export_eAccounts_pxp,
    export_results_to_csv(object = eAccounts_exio_p_wide,
                          accounts_to_exclude = c("Energy combustion", "T&D losses"),
                          path = paste0(path_to_results, "energy_accounts_pxp.csv")),
    format = "file"
  ),
  
  # Energy extension vectors ixi
  tar_target(
    export_eVecs_ixi,
    export_results_to_csv(object = eVecs_exio_i,
                          accounts_to_exclude = c("Energy combustion", "T&D losses"),
                          path = paste0(path_to_results, "energy_extensions_ixi.csv")),
    format = "file"
  ),
  
  # Energy extension vectors pxp
  tar_target(
    export_eVecs_pxp,
    export_results_to_csv(object = eVecs_exio_p,
                          accounts_to_exclude = c("Energy combustion", "T&D losses"),
                          path = paste0(path_to_results, "energy_extensions_pxp.csv")),
    format = "file"
  ),
  
  # Production- and consumption-based accounts
  tar_target(
    export_PBA_CBA,
    export_results_to_csv(object = eAccounts_pcba,
                          accounts_to_exclude = c("Energy combustion", "T&D losses"),
                          path = paste0(path_to_results, "energy_pba_cba_accounts.csv")),
    format = "file"
  ),
  
  # Share of unmatched final energy
  tar_target(
    export_share_unmatched_fec,
    export_results_to_csv(share_unmatched_fec,
                          accounts_to_exclude = NA,
                          path = paste0(path_to_results, "test_share_unmatched_fec.csv"))
  ),
  
  # Share of reallocated energy products in final energy consumption
  tar_target(
    export_share_products_in_tfe,
    export_results_to_csv(share_products_in_tfe,
                          accounts_to_exclude = NA,
                          path = paste0(path_to_results, "test_share_products_in_tfe.csv"))
  ),
  
  # Comparison of EXIOBASE final energy accounts to the IEA's final energy consumption
  tar_target(
    export_comp_FUE_to_IEA,
    export_results_to_csv(comp_FUE_to_IEA,
                          accounts_to_exclude = NA,
                          path = paste0(path_to_results, "test_comp_FUE_to_IEA.csv"))
  )
)
