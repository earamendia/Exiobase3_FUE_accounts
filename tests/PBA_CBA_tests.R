
library(dplyr)

# TEST 1: Production and consumption based accounts should be equal global level ----------------------------------------------------------

energy_pba_cba_accounts <- readr::read_csv("outputs/energy_pba_cba_accounts.csv")

energy_pba_cba_accounts <- readr::read_csv("outputs/energy_pba_cba_accounts.csv")


# Actually to do this we can check directly the PBA and CBA, at the global level they should be equal.
test_global_pba_cba <- energy_pba_cba_accounts |> 
  dplyr::group_by(Account, eQuant, Year, Unit) |> 
  summarise(PBA_global = sum(PBA),
            CBA_global = sum(CBA)) |> 
  mutate(diff = abs(CBA_global - PBA_global) / PBA_global) |> 
  arrange(desc(diff)) |> 
  print()


# TEST 2: ENERGY ACCOUNTS AGGREGATED SHOULD BE EQUAL PRODUCT AND INDUSTRY LEVEL

energy_accounts_ixi <- readr::read_csv("outputs/energy_accounts_ixi.csv")
energy_accounts_pxp <- readr::read_csv("outputs/energy_accounts_pxp.csv")

# Check at the country level
tot_energy_ixi <- energy_accounts_ixi |>
  tidyr::pivot_longer(cols = -c("Account", "eQuant", "Year", "Unit"), names_to = "Sector", values_to = "Eval") |> 
  tidyr::separate(Sector, sep = "_", into = c("Country", "Sector")) |> 
  group_by(Country, Account, eQuant, Year, Unit) |> 
  summarise(Eixi = sum(Eval)) |> 
  print()

tot_energy_pxp <- energy_accounts_pxp |> 
  tidyr::pivot_longer(cols = -c("Account", "eQuant", "Year", "Unit"), names_to = "Sector", values_to = "Eval") |> 
  tidyr::separate(Sector, sep = "_", into = c("Country", "Product")) |> 
  group_by(Country, Account, eQuant, Year, Unit) |> 
  summarise(Epxp = sum(Eval)) |> 
  print()

# Check they match
# Less than 0.2% difference, that's fine
a <- dplyr::inner_join(tot_energy_ixi, tot_energy_pxp, by = c("Country", "Account", "eQuant", "Year", "Unit")) |> 
  mutate(diff = abs(Epxp - Eixi) / Eixi) |> 
  arrange(desc(diff)) |> 
  print()

# Try at the global level now
tot_energy_ixi_wrld <- energy_accounts_ixi |>
  tidyr::pivot_longer(cols = -c("Account", "eQuant", "Year", "Unit"), names_to = "Sector", values_to = "Eval") |> 
  tidyr::separate(Sector, sep = "_", into = c("Country", "Sector")) |> 
  group_by(Account, eQuant, Year, Unit) |> 
  summarise(Eixi = sum(Eval)) |> 
  print()

tot_energy_pxp_wrld <- energy_accounts_pxp |> 
  tidyr::pivot_longer(cols = -c("Account", "eQuant", "Year", "Unit"), names_to = "Sector", values_to = "Eval") |> 
  tidyr::separate(Sector, sep = "_", into = c("Country", "Product")) |> 
  group_by(Account, eQuant, Year, Unit) |> 
  summarise(Epxp = sum(Eval)) |> 
  print()

# Very small difference at the global level, good
b <- dplyr::inner_join(tot_energy_ixi_wrld, tot_energy_pxp_wrld, by = c("Account", "eQuant", "Year", "Unit")) |> 
  mutate(diff = abs(Epxp - Eixi) / Eixi) |> 
  arrange(desc(diff)) |> 
  print()
