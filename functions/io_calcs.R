
# Builds input output matrices
build_io_matrices <- function(path_to_exiobase,
                              aggregate_Y_mat,
                              matdims,
                              ind_prod_names,
                              fds_names,
                              years){
  
  # Setting up empty list for IO matrices
  list_io_matrices <- setNames(vector("list", length(years)), as.character(years))
  
  for (i in years){
    
    # Year index
    year_index <- i - min(years)+1
    
    # Print i
    print(i)
    
    # Unzipping file for year i
    zipF <- paste0(path_to_exiobase, "IOT_", i, "_", matdims, ".zip")
    outDir <- paste0(path_to_exiobase, "IOT_", i, "_", matdims, "/")
    unzip(zipF, exdir = outDir)
    
    # Figuring column types to speed up data loading process
    if (i == min(years)){
      # Read only the header to determine the number of columns
      header_Z <- data.table::fread(paste0(path_to_exiobase, "IOT_", i, "_", matdims, "/Z.txt"), sep = "\t", nrows = 0)
      header_Y <- data.table::fread(paste0(path_to_exiobase, "IOT_", i, "_", matdims, "/Y.txt"), sep = "\t", nrows = 0)
      
      # Dynamically create colClasses: first two as "character", rest as "numeric"
      col_classes_Z <- c(rep("character", 2), rep("numeric", length(header_Z) - 2))
      col_classes_Y <- c(rep("character", 2), rep("numeric", length(header_Y) - 2))
    }
    
    # Loading Z and Y matrices
    Z_raw <- data.table::fread(paste0(path_to_exiobase, "IOT_", i, "_", matdims, "/Z.txt"), sep = "\t", 
                               skip = 3, 
                               colClasses = col_classes_Z)
    
    Y_raw <- data.table::fread(paste0(path_to_exiobase, "IOT_", i, "_", matdims, "/Y.txt"), sep = "\t", 
                               skip = 3, 
                               colClasses = col_classes_Y)
    
    # Check that there is no value beyond safe limit for storing as numeric
    bad_Z <- Z_raw[, lapply(.SD, function(x) inherits(x, "integer64") && any(abs(x) > 2^53))]
    assertthat::assert_that(any(bad_Z) == FALSE)
  
    bad_Y <- Y_raw[, lapply(.SD, function(x) inherits(x, "integer64") && any(abs(x) > 2^53))]
    assertthat::assert_that(any(bad_Z) == FALSE)
    
    # Converting to numeric and then to Matrix (sparsematrix) both the Z and Y matrices
    Z_raw[, names(Z_raw)[3:ncol(Z_raw)] := lapply(.SD, as.numeric), .SDcols = 3:ncol(Z_raw)]
    Y_raw[, names(Y_raw)[3:ncol(Y_raw)] := lapply(.SD, as.numeric), .SDcols = 3:ncol(Y_raw)]
    
    Z <- Matrix::Matrix(as.matrix(Z_raw[1:nrow(Z_raw), 3:ncol(Z_raw)]))
    Y <- Matrix::Matrix(as.matrix(Y_raw[1:nrow(Y_raw), 3:ncol(Y_raw)]))
    
    # Removing unzipped file
    unlink(outDir, recursive = TRUE)
    
    # Setting columns and row names
    rownames(Z) <- ind_prod_names
    colnames(Z) <- ind_prod_names
    rownames(Y) <- ind_prod_names
    colnames(Y) <- fds_names
    
    # If isTRUE, then aggregate Y_mat
    if (isTRUE(aggregate_Y_mat)){
      
      # Preparing country and final demand list
      country_list <- unique(Z_raw[, 1])
      fds_list <- as.numeric(1:7)
      
      # Detailed columns in Y, and group id by country
      cols_Y <- data.table::CJ(Country = country_list$V1, Sector = fds_list)[, group_id := .GRP, by = .(Country)]
      
      # Creating column aggregation matrix
      C_col_agg <- Matrix::sparseMatrix(i = seq_len(ncol(Y)), j = cols_Y$group_id, x = 1)
      rownames(C_col_agg) <- stringr::str_c(cols_Y$Country, "_", cols_Y$Sector)
      colnames(C_col_agg) <- country_list$V1
      
      # Calculating aggregated Y
      Y_agg <- Y %*% C_col_agg
    }
    
    # 8. Creating x, y, y_fds
    x <- Matrix::Matrix(Matrix::rowSums(Y_agg) + Matrix::rowSums(Z), ncol = 1, sparse = TRUE,
                        dimnames = list(rownames(Y_agg)))
    y <- Matrix::Matrix(Matrix::rowSums(Y_agg), ncol = 1, sparse = TRUE,
                        dimnames = list(rownames(Y_agg)))
    y_fds <- Matrix::Matrix(Matrix::colSums(Y), ncol = 1, sparse = TRUE,
                            dimnames = list(colnames(Y)))
    
    # 9. Creating a matrix inv(diag(x)) and inv(diag(y))
    # 9.a Testing that there are no stored 0s in the x vector
    x <- Matrix::drop0(x)
    assertthat::assert_that(any(x@x==0) == FALSE)
    
    # 9.b Creating x_inv accordingly
    x_inv <- x
    x_inv@x <- 1/x@x
    
    # 9.c Testing that there are no stored 0s in the y and y_fds vector
    y <- Matrix::drop0(y)
    y_fds <- Matrix::drop0(y_fds)
    assertthat::assert_that(any(y@x==0) == FALSE)
    assertthat::assert_that(any(y_fds@x==0) == FALSE)
    
    # 9.d Creating y_inv and y_fds_inv accordingly
    y_inv <- y
    y_inv@x <- 1/y@x
    
    y_fds_inv <- y_fds
    y_fds_inv@x <- 1/y_fds@x
    
    # 10. Calculating A
    A <- sweep(Z, 2, x_inv, `*`)
    colnames(A) <- ind_prod_names
    
    # 11. Calculating L
    L <- Matrix::solve(Matrix::Diagonal(n = nrow(A)) - A)
    
    # Append matrices and vectors to lists
    list_io_matrices[[as.character(i)]] <- list(Year = i,
                                                Y = Y,
                                                Y_agg = Y_agg,
                                                Z = Z,
                                                A = A,
                                                L = Matrix::Matrix(L, sparse = TRUE),
                                                x = x,
                                                x_inv = x_inv,
                                                y = y,
                                                y_inv = y_inv,
                                                y_fds = y_fds,
                                                y_fds_inv = y_fds_inv)
  }
  return(list_io_matrices)
}


# This function calculates the energy extension vectors
# Can we make it general to be used for pxp too?
calc_eVecs <- function(eAccounts,
                       io_mats,
                       ind_prod_names,
                       fds_names,
                       years){
  # List initialisation
  eVecs_list <-  setNames(vector("list", length(years)), as.character(years))
  
  # Casting energy accounts to wide
  eAccountsWide <- eAccounts[, dcast(.SD, Account + eQuant + Year + Unit ~ Sector, value.var = "Eval", fill = 0)]
  
  # Check which columns are missing and add them
  sector_names <- c(ind_prod_names, fds_names)
  missing_cols <- setdiff(sector_names, names(eAccountsWide))
  
  if (length(missing_cols)) {
    eAccountsWide[, (missing_cols) := 0]
  }
  
  setcolorder(eAccountsWide, c("Account", "eQuant", "Year", "Unit", sector_names))
  
  # Check we have the sectors contained in the columns of accounts_efi_wide match expected sectors, in the expected order
  obtained_cols <- colnames(eAccountsWide[, ..sector_names])
  assertthat::assert_that(isTRUE(all(obtained_cols == sector_names)))
  
  for (currYear in years){
    # Print currYear
    print(currYear)
    
    # Year index
    year_index <- currYear - min(years) + 1
    
    # Filtering relevant part of the accounts
    eAccounts_currYear <- eAccountsWide[Year == currYear,]
    
    # Loading relevant vectors
    x_inv <- io_mats[[as.character(currYear)]]$x_inv
    y_fds_inv <- io_mats[[as.character(currYear)]]$y_fds_inv # TO REPLACE WITH ACTUAL y_inv!
    
    # Calculating energy extension vectors
    e_Vecs_curr_year <- eAccounts_currYear
    
    # Energy extension vectors for energy intensities by industry output
    E_mat_x <- Matrix::Matrix(as.matrix(e_Vecs_curr_year[, ..ind_prod_names]))
    e_vecs_mat_x <- sweep(E_mat_x, 2, x_inv, `*`)
    
    # Energy extension vectors for energy intensities by final demand
    E_mat_y <- Matrix::Matrix(as.matrix(e_Vecs_curr_year[, ..fds_names]))
    e_vecs_mat_y <- sweep(E_mat_y, 2, y_fds_inv, `*`)
    
    # Replace in the data.table and add to list
    e_Vecs_curr_year[, (ind_prod_names) := data.table::as.data.table(as.matrix(e_vecs_mat_x))]
    e_Vecs_curr_year[, (fds_names) := data.table::as.data.table(as.matrix(e_vecs_mat_y))]
    
    eVecs_list[[year_index]] <- e_Vecs_curr_year
  }
  # Bind and return
  eVecs <- data.table::rbindlist(eVecs_list, use.names = TRUE)
  return(eVecs)
}


# This function calculates all the energy footprints
calc_e_footprints <- function(evecs,
                              io_mats,
                              ind_prod_names,
                              fds_names,
                              years){
  
  # Initialisation list of efootprints
  e_footprint_mats <-  setNames(vector("list", length(years)), as.character(years))
  
  # For loop through years
  for (currYear in years){
    print(currYear)
    
    # Loading energy vectors
    e_vecs_mat <- Matrix::Matrix(as.matrix(evecs[Year == currYear, ..ind_prod_names]), sparse = TRUE)
    
    # Loading IO matrices
    Y_agg <- io_mats[[as.character(currYear)]]$Y_agg
    L <- io_mats[[as.character(currYear)]]$L

    # Calculating footprint matrix by final demand sector
    e_footprint_mats[[as.character(currYear)]] <- e_vecs_mat %*% L %*% Y_agg
  }
  return(e_footprint_mats)
}
