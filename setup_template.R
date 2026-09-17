
# Options for run
tar_option_set(format = "qs",
               packages = c("magrittr", "data.table", "Matrix"),
               memory = "transient",
               garbage_collection = TRUE)

# Path to data
path_to_exiobase_set <- "PATH_TO_EXIOBASE3/"
path_to_gross_energy_accounts_set <- "inputs/dummy_gross_energy_accounts/"
path_to_iea_webs_set <- "input/dummy_webs/dummy_webs.csv"

# Path to results
path_to_results_set <- "outputs/"

# Path to masks
path_to_fe_mask <- "inputs/masks/final_energy_mask.xlsx"
path_to_eiou_mask <- "inputs/masks/eiou_mask.xlsx"
path_to_eloss_mask <- "inputs/masks/losses_mask.xlsx"
path_to_neu_mask <- "inputs/masks/non_energy_uses_mask.xlsx"
path_to_ecomb_mask <- "inputs/masks/energy_combustion_mask.xlsx"
path_to_tp_mask <- "inputs/masks/tp_mask.xlsx"

# Path to multipliers
path_to_ef_to_xf_multipliers <- "inputs/multipliers/Ef_to_Xf_multipliers.csv"
path_to_ef_to_eu_multipliers <- "inputs/multipliers/Ef_to_Eu_multipliers.csv"
path_to_xf_to_xu_multipliers <- "inputs/multipliers/Xf_to_Xu_multipliers.csv"
path_to_ef_to_eloss_multipliers <- "inputs/multipliers/Ef_to_Eloss_multipliers.csv"
path_to_xf_to_xloss_multipliers <- "inputs/multipliers/Xf_to_Xloss_multipliers.csv"

# Path to country concordance
path_to_country_concordance <- "inputs/country_data.tsv"

# Years
years_set <- c(1995, 2020)
