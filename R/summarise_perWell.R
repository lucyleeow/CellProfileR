#' Summarise Per well
#' 
#' Calculate the sum (for 'Count' features) or median (for features starting 
#' with 'Median' ) value of the Cell Profiler columns (features) for each well. 
#' If only df_full is given, the sum and median of ALL images (rows) will be 
#' output. If df_filtered is also provided, the sum of ALL images (i.e. using 
#' df_full) will be output for 'Count' features but the median of only the 
#' images remaining after filtering (i.e. using df_filtered) will be output for
#' features starting with 'Median'.  
#' 
#' @return A dataframe or data.table (if filtered = TRUE) with one value per
#'     well.
#' 
#' @param df_full Dataframe of full raw (per image) Cell Profiler data, with no
#'     images (rows) removed.
#' @param df_filtered Optional argument. Dataframe of filtered Cell Profiler 
#'     data, with poor quality images removed. If given, this dataframe will be
#'     used to calculate the median value of features that start with 'Median'.
#' @param num_images The number of images taken per well.
#' 
#' 
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @import data.table
#' 
#' @export
summarise_perWell <- function(df_full, df_filtered, num_images) {
  
  # check inputs
  assert_that(is.numeric(num_images), length(num_images) == 1, 
              msg = "Check 'num_images' is single number")
  
  assert_that(dim(df_full)[1] %% num_images == 0, 
              msg = "Check that there are the same number of images for each 
              well in 'df_full'")
  
  
  # obtain only count columns
  df_count <- df_full %>% 
    dplyr::select(dplyr::starts_with("Meta"), dplyr::starts_with("Count")) 
  
  # take sum of every 'num_images' rows to get per well data
  mat_sum <- apply(df_count[,3:ncol(df_count)], 2, 
                   function(x) colSums(matrix(x, nrow = num_images)))
  
  
  # if data has not been filtered, the number of images (and thus 
  # rows) as 'num_images' for each well/plate grouping
  
  if (missing(df_filtered)){
    
    # obtain only median columns
    df_median <- df_full %>% 
      dplyr::select(dplyr::starts_with("Meta"), dplyr::starts_with("Median")) 
    
    # take median of every 'num_images' rows to get per well data
    mat_median <- apply(df_median[,3:ncol(df_median)], 2, 
                        function(x) robustbase::colMedians(
                          matrix(x, nrow = num_images)))
    
    
    # get metadata rows, 1 for each well
    df_meta <- df_full[,c("Metadata_Barcode","Metadata_WellID")]
    
    df_meta <- unique(df_meta)
    
    
    # join summarised matrices
    full_mat <- cbind(mat_sum, mat_median)
    
    df_final <- cbind(df_meta, as.data.frame(full_mat))
    
  } else {
    
    # when number of rows are not consistent for each plate/well
    # grouping, use data.table to aggregate and summarise
    # This take MUCH longer than above function 
    
    # vector of grouping column names
    grouping_cols <- c("Metadata_Barcode", "Metadata_WellID")
    
    # obtain only median columns
    df_median <- df_filtered %>% 
      dplyr::select(dplyr::starts_with("Meta"), dplyr::starts_with("Median"))
    
    df_median <- as.data.table(df_median)
    
    # convert all data columns to double type
    numcol <- ncol(df_median)
    
    df_median <- df_median[ ,colnames(df_median)[3:numcol] := lapply(.SD, 
                                                                     as.double),
                            .SDcols = 3:numcol]
    
    # get median of median columns
    df_median <- df_median[ , lapply(.SD, function(x) median(x, na.rm = TRUE)),
                          by = grouping_cols]
    
    
    # as order is preserved, cbind columns together
    df_final <- cbind(df_median[ , 1:2], mat_sum, 
                      df_median[ , 3:ncol(df_median)])
    
  }
  
  return(df_final)
  
}


