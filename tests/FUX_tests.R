
# This script performs tests on the final-to-useful exergy efficiencies,
# and on the final and useful stage exergy accounts and vectors

library(targets)
library(dplyr)

# TEST 1: EXERGY EFFICIENCIES <1 IN MULTIPLIERS ---------------------------
Ef_to_Eloss_multipliers <- readr::read_csv("inputs/multipliers/Ef_to_Eloss_multipliers.csv")
Ef_to_Eu_multipliers <- readr::read_csv("inputs/multipliers/Ef_to_Eu_multipliers.csv")
Ef_to_Xf_multipliers <- readr::read_csv("inputs/multipliers/Ef_to_Xf_multipliers.csv")
Xf_to_Xloss_multipliers <- readr::read_csv("inputs/multipliers/Xf_to_Xloss_multipliers.csv")
Xf_to_Xu_multipliers <- readr::read_csv("inputs/multipliers/Xf_to_Xu_multipliers.csv")

a <- Ef_to_Eu_multipliers |> 
  tidyr::pivot_longer(cols = -c("IEA.country.name", "Flow", "Product"), values_to = "eta", names_to = "Year") |> 
  dplyr::filter(eta > 1) |> 
  tidyr::expand(Product) |> 
  print()

# Should be empty
b <- Xf_to_Xu_multipliers |> 
  tidyr::pivot_longer(cols = -c("IEA.country.name", "Flow", "Product"), values_to = "eta", names_to = "Year") |> 
  dplyr::filter(eta > 1) |> 
  print()

Xf_to_Xu_multipliers |> filter(Product == "Solar thermal") |> print()


# TEST 2: USEFUL EXERGY < FINAL EXERGY ALL ACCOUNTS ------------------------
energy_accounts_ixi <- readr::read_csv("outputs/energy_accounts_ixi.csv")
energy_accounts_pxp <- readr::read_csv("outputs/energy_accounts_pxp.csv")

energy_accounts_ixi <- readr::read_csv("outputs/energy_accounts_ixi.csv")
energy_accounts_pxp <- readr::read_csv("outputs/energy_accounts_pxp.csv")



# Industry-by-industry test
tidy_ixi <- tidyr::pivot_longer(energy_accounts_ixi, cols = -c("Account", "eQuant", "Year", "Unit"), names_to = "Sector", values_to = "Eval")

# Should return empty df
uX_vs_fX_ixi <- tidy_ixi |> 
  dplyr::filter(eQuant == "X") |> 
  filter(Account %in% c("Final energy", "Useful energy")) |> 
  tidyr::pivot_wider(names_from = Account, values_from = Eval) |> 
  filter(`Final energy` < `Useful energy`) |> 
  print()

# Checking no negative value - should return empty df
tidy_ixi |> filter(Eval < 0) |> print()


# Product-by-product test
tidy_pxp <- tidyr::pivot_longer(energy_accounts_pxp, cols = -c("Account", "eQuant", "Year", "Unit"), names_to = "Sector", values_to = "Eval")

# Should return empty df
uX_vs_fX_ixi <- tidy_pxp |> 
  dplyr::filter(eQuant == "X") |> 
  filter(Account %in% c("Final energy", "Useful energy")) |> 
  tidyr::pivot_wider(names_from = Account, values_from = Eval) |> 
  filter(`Final energy` < `Useful energy`) |> 
  print()

# Checking no negative value - should return empty df
tidy_pxp |> filter(Eval < 0) |> print()


# TEST 3: USEFUL EXERGY + EXERGY LOSSES = FINAL EXERGY ---------------------

# Industry-by-industry test
uX_Xloss_ixi <- tidy_ixi |> 
  dplyr::filter(eQuant == "X") |> 
  filter(Account %in% c("Final energy", "Useful energy", "Final-to-useful energy losses")) |> 
  tidyr::pivot_wider(names_from = Account, values_from = Eval) |> 
  mutate(FE2 = `Final-to-useful energy losses` + `Useful energy`) |> 
  mutate(diff = abs(FE2 -  `Final energy`) / `Final-to-useful energy losses`) |> 
  filter(diff > 1e-4) |> 
  print()


# Product-by-product test
uX_Xloss_pxp <- tidy_pxp |> 
  dplyr::filter(eQuant == "X") |> 
  filter(Account %in% c("Final energy", "Useful energy", "Final-to-useful energy losses")) |> 
  tidyr::pivot_wider(names_from = Account, values_from = Eval) |> 
  mutate(FE2 = `Final-to-useful energy losses` + `Useful energy`) |> 
  mutate(diff = abs(FE2 -  `Final energy`) / `Final-to-useful energy losses`) |> 
  filter(diff > 1e-4) |> 
  print()


# Try in consumption-based accounts
energy_pba_cba_accounts <- readr::read_csv("outputs/energy_pba_cba_accounts.csv")

energy_pba_cba_accounts <- readr::read_csv("outputs/energy_pba_cba_accounts.csv")


# Check useful exergy always smaller than final exergy, should return empty data frame
UX_vs_FX_cba <- energy_pba_cba_accounts |> 
  dplyr::select(-PBA) |> 
  dplyr::filter(eQuant == "X") |> 
  dplyr::filter(Account %in% c("Final energy", "Useful energy")) |> 
  tidyr::pivot_wider(names_from = Account, values_from = CBA) |> 
  filter(`Final energy` < `Useful energy`) |> 
  print()

# Check Useful exergy + exergy losses = final exergy, should return empty data frame
UX_Xloss_cba <- energy_pba_cba_accounts |> 
  dplyr::select(-PBA) |> 
  dplyr::filter(eQuant == "X") |> 
  filter(Account %in% c("Final energy", "Useful energy", "Final-to-useful energy losses")) |> 
  tidyr::pivot_wider(names_from = Account, values_from = CBA) |> 
  mutate(FE2 = `Final-to-useful energy losses` + `Useful energy`) |> 
  mutate(diff = abs(FE2 -  `Final energy`) / `Final-to-useful energy losses`) |> 
  filter(diff > 1e-4) |> 
  print()
