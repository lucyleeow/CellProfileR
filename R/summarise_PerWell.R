#' Summarise Per well
#' 
#' Calculate the sum (for 'Count' features) or median (for 'Median' features)
#' value of the Cell Profiler columns (features) for each well. 
#' 
#' @return A dataframe or data.table (if filtered = TRUE) with one value per
#'     well.
#' 
#' @param df Dataframe of raw (per image) Cell Profiler data.
#' @param num_images The number of images taken per well.
#' @param filtered Single logical indicated whether poor quality images have 
#'     been filtered (thus the number of images per well is variable).
#' 
#' 
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @import data.table
#' 

#' @export
summarise_PerWell <- function(df, num_images, filtered = FALSE) {
  
  # check inputs
  assert_that(is.logical(filtered), length(filtered) == 1, 
              msg = "Check that 'filtered' is a single logical")
  
  assert_that(is.numeric(num_images), length(num_images) == 1, 
              msg = "Check 'num_images' is single number")
  
  
  # obtain only count columns
  df_count <- df %>% 
    dplyr::select(dplyr::starts_with("Meta"), dplyr::starts_with("Count")) 
  
  # obtain only median columns
  df_median <- df %>% 
    dplyr::select(dplyr::starts_with("Meta"), dplyr::starts_with("Median")) 
  
  # if data has not been filtered, the number of images (and thus 
  # rows) as 'num_images' for each well/plate grouping
  
  if (! filtered){
    
    assert_that(dim(df)[1] %% num_images == 0, 
                msg = "Check that there are the same number of images for each well")
    
    # take sum of every 'num_images' rows to get per well data
    mat_sum <- apply(df_count[,3:ncol(df_count)], 2, 
                     function(x) colSums(matrix(x, nrow = num_images)))
    
    
    # take median of every 'num_images' rows to get per well data
    mat_median <- apply(df_median[,3:ncol(df_median)], 2, 
                        function(x) robustbase::colMedians(
                          matrix(x, nrow = num_images)))
    
    
    # get metadata rows, 1 for each well
    df_meta <- df %>%
      dplyr::select(Metadata_Barcode, Metadata_WellID)
    
    nrow <- dim(df)[1]
    repeats <- nrow/num_images
    
    df_meta <- df_meta[ rep(c(TRUE, rep(FALSE, num_images - 1)), 
                            repeats), ]
    
    
    # join summarised matrices
    full_mat <- cbind(mat_sum, mat_median)
    
    df_final <- cbind(df_meta, as.data.frame(full_mat))
    
  } else {
    
    # when number of rows are not consistent for each plate/well
    # grouping, use data.table to aggregate and summarise
    # This take MUCH longer than above function 
    
    # vector of grouping column names
    grouping_cols <- c("Metadata_Barcode", "Metadata_WellID")
    
    # join 2 dfs in list
    df_list <- list(df_count,df_median)
    # turn both dfs into data.table
    df_list <- lapply(df_list, as.data.table)
    
    
    # get sum of count columns
    df_count <- df_list[[1]][ , lapply(.SD,
                                       function(x) sum(x, na.rm = TRUE)),
                              by = grouping_cols]
    
    # get median of median columns
    df_median <- df_list[[2]][ , lapply(.SD, 
                                        function(x) median(x, na.rm = TRUE)),
                               by = grouping_cols]
    
    
    # as order is preserved, cbind columns together
    df_final <- cbind(df_count, df_median[ , 3:ncol(df_median)])
    
  }
  
  return(df_final)
  
}


