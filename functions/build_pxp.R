
# This function builds product names following the EXIOBASE (Country, Product) structure
build_product_names <- function(countryNames){
  
  # Building data.table with (Country, Product)
  a <- CJ(countryNames, seq(1:200), sorted = FALSE)[, product_names := stringr::str_c(countryNames, "_P", V2)]
  
  # Selecting and returning product names
  return(a$product_names)
}


# This function calculates a list of product-share matrices (C) based on the supply matrices V for every year
# C = V g(hat-1)
calc_C_mats <- function(path,
                        country_list,
                        industry_names,
                        product_names,
                        years){
  
  # Path to Supply Use Tables
  path_to_SUTs <- paste0(path, "sut/current/")
  
  # Initialising list of C matrices
  list_C_matrices <- setNames(vector("list", length(years)), as.character(years))

  for (currYear in years){
    print(currYear)
    
    for (currCountry in country_list){
      
      # Filepath to supply matrix
      country_year_supply_file <- paste0(path_to_SUTs, currCountry, "_", currYear, "_sup.csv")
      
      # Select current country industry and product names
      currCountry_indNames <- industry_names[grepl(currCountry, industry_names)]
      currCountry_prodNames <- product_names[grepl(currCountry, product_names)]
      
      # Reading supply matix as sparse matrix
      V_dt <- data.table::fread(country_year_supply_file)[, c("Row", "m01") := NULL]
      
      # Check that there is no value beyond safe limit for storing as numeric
      bad <- V_dt[, lapply(.SD, function(x) inherits(x, "integer64") && any(abs(x) > 2^53))]
      assertthat::assert_that(any(bad) == FALSE)
      gc()
      
      # Converting to numeric and then to matrix
      V_dt[, names(V_dt) := lapply(.SD, as.numeric)]
      V_mat_currCountry <- Matrix::Matrix(as.matrix(V_dt))
      
      # Replacing industry and product names
      colnames(V_mat_currCountry) <- currCountry_indNames
      rownames(V_mat_currCountry) <- currCountry_prodNames
      
      # If first country, initialising multi-regional supply matrix V_mr; else, binding current country matrix to V_mr
      if (currCountry == country_list[1]){
        V_mr <- V_mat_currCountry
      } else {
        V_mr <- Matrix::bdiag(V_mr, V_mat_currCountry)
      }
      gc()
    }
    
    # Filling in names for multi-regional supply matrix V
    dimnames(V_mr) <- list(product_names, industry_names)
    
    # Calculating total output by industry vector g
    g <- Matrix::Matrix(Matrix::colSums(V_mr), ncol = 1, sparse = TRUE,
                        dimnames = list(colnames(V_mr)))
    
    # Testing that there are no stored 0s in the g vector
    assertthat::assert_that(any(g@x==0) == FALSE)
    
    # Creating g_inv accordingly
    g_inv <- g
    g_inv@x <- 1/g@x
    
    # Calculating product shares matrix C
    C_mat <- sweep(V_mr, 2, g_inv, `*`)
    
    # Filling in industry and product names
    colnames(C_mat) <- industry_names
    rownames(C_mat) <- product_names
    
    # Testing that all values are either ones or zeros
    test <- Matrix::colSums(C_mat)
    assertthat::assert_that(all(abs(test - 1) <= 1e-4 | abs(test - 0) <= 1e-4))
    
    # Last, test that entries are zero where rowCountry != colCountry
    # Finding i and j where there are non-zero values
    nz <- Matrix::summary(C_mat)

    # Finding corresponding rownames and colnames
    rn <- rownames(C_mat)[nz$i]
    cn <- colnames(C_mat)[nz$j]
    r1 <- sub("_.*", "", rn)
    c1 <- sub("_.*", "", cn)

    # Actual test
    assertthat::assert_that(all(r1 == c1))
    
    # If faulty, to check which coefficients are faulty use the following code:
    # bad <- r1 != c1
    # faulty <- data.frame(
    #   row   = rn[bad],
    #   col   = cn[bad],
    #   value = nz$x[bad]
    # )
    # View(faulty)
    
    # Replacing in appropriate place the supply matrix
    list_C_matrices[[as.character(currYear)]] <- C_mat
    gc()
  }
  return(list_C_matrices)
}


# This function calculates the detailed pxp accounts using the ixi accounts an the product-shares matrices C
# Noting Ei the energy accounts matrix (exi)m then Ep = E.C(transpose)
calc_accounts_efp <- function(accounts_efi,
                              Cmats,
                              sector_names,
                              industry_names,
                              product_names,
                              fds_names,
                              years){

  # Initialising list efp accounts
  list_efp_accounts <-  setNames(vector("list", length(years)), as.character(years))
  
  # List of energy accounts
  list_energy_accounts <- unique(accounts_efi$Account)
  
  # Looping through years
  for (currYear in years){
    print(currYear)
    
    # Initialising list of accounts for current year - artifact to split processing in for loop of energy accounts
    list_currYear_accounts <- setNames(vector("list", length(list_energy_accounts)), list_energy_accounts)
    gc() # Clear memory from previous year iteration
    
    for (account in list_energy_accounts){
      print(account)
      
      ## (1) Preparing a wide data.table of final stage energy accounts
      # Filtering year and casting to wide
      accounts_efi_wide <- accounts_efi[Year == currYear & Account == account,][
        , dcast(.SD, 
                ISO3 + IEA_latest + EXIO3 + Account + eQuant + Flow + Product + Year + Unit ~ Sector, value.var = "Eval", fill = 0)]
      
      # Check which columns are missing and add them
      missing_cols <- setdiff(sector_names, names(accounts_efi_wide))
      
      if (length(missing_cols)) {
        accounts_efi_wide[, (missing_cols) := 0]
      }
      
      setcolorder(accounts_efi_wide, c("ISO3", "IEA_latest", "EXIO3", "Account", "eQuant", "Flow", "Product", "Year", "Unit", sector_names))
      
      # Check we have the sectors contained in the columns of accounts_efi_wide match expected sectors, in the expected order
      obtained_cols <- colnames(accounts_efi_wide[, ..sector_names])
      assertthat::assert_that(isTRUE(all(obtained_cols == sector_names)))
      
      ## (2) Calculating accounts by EXIOBASE product
      # Filtering and transposing C_mat for current year
      C_transp_currYear <- Matrix::t(Matrix::drop0(Cmats[[as.character(currYear)]]))
      
      # Filtering energy accounts of industries (intermediary demand) and of final demand
      E_ind <- accounts_efi_wide[, ..industry_names]
      E_fds <- accounts_efi_wide[, ..fds_names]
      
      # Checking all values are numeric
      assertthat::assert_that(all(sapply(E_ind, is.numeric)))
      assertthat::assert_that(all(sapply(E_fds, is.numeric)))
      
      # Building a sparse matrix of detailed industrial energy accounts
      E_mat <- Matrix::drop0(Matrix::Matrix(as.matrix(E_ind), sparse = TRUE))
      
      # Calculating product-level detailed industrial energy accounts
      res <- E_mat %*% C_transp_currYear
      rm(E_mat, C_transp_currYear)
      gc()
      
      # Temporary dt to fill in the list
      temp <- accounts_efi_wide[, .SD, .SDcols = !sector_names][, (product_names) := as.data.table(as.matrix(res))][
        , (fds_names) := E_fds]
      
      # Freeing up memory
      rm(accounts_efi_wide)
      gc()
      
      # Casting temp and filling in the element of the list corresponding to the current account
      list_currYear_accounts[[account]] <- temp[
        , melt(.SD, 
               id.vars = c("ISO3", "IEA_latest", "EXIO3", "Account", "eQuant", "Flow", "Product", "Year", "Unit"), 
               variable.name = "Sector", 
               value.name = "Eval")][
          Eval != 0,]
      
      # Freeing up memory
      rm(temp)
      gc()
    }
    
    # Bind the obtained efp accounts for all types of energy accounts with rbindlist
    list_efp_accounts[[as.character(currYear)]] <- data.table::rbindlist(list_currYear_accounts)
    
    # Freeing up memory
    rm(list_currYear_accounts)
    gc()
    
  }
  accounts_efp <- rbindlist(list_efp_accounts, use.names = TRUE)
  return(accounts_efp)
}
