#############################################
# Functions to clean/manipulate raw data from 
# Cell Profiler
#
# Author: Lucy Liu
#############################################

library(data.table)
library(tidyverse)

# This function removes unnecessary columns from the
# full csv output from cell profiler
#############################################

clean_df <- function(
  df     # full df from Cell Profiler to clean. 
){
  
  df_clean <- df %>% 
    select(Metadata_PlateID, 
           Metadata_WellID,
           Count_Cells,
           Count_Cells_unfiltered,
           starts_with("Median"),
           starts_with("ImageQuality_PowerLogLogSlope_Orig"))
  
  return(df_clean)
  
}


# This function summarises features per well instead of
# per image.
#
# Sum is given for Count features
# Median is given for all other features
#############################################

summarise_PerWell <- function(
  df,               # the df of raw (per image) data
  no_images,        # the number of images taken per well
  filtered = FALSE  # logical indicating whether poor quality images have been filtered
){
  
  # check function inputs
  assertthat::assert_that(is.logical(filtered), 
                          length(filtered) == 1,
                          msg = "Check that 'filtered' is a single logical")
  
  assertthat::assert_that(is.numeric(no_images),
                          length(no_images) == 1,
                          msg = "Check 'no_images' is single number")
  
  
  # check if df is a data.table, if not, turn into data.table
  if (sum (class(df) %in% "data.table") < 1){
    
    df <- as.data.table(df)
    
  }
  
  # obtain only count columns
  df_count <- df %>% 
    select(starts_with("Meta"), starts_with("Count")) 
  
  # obtain only median columns
  df_median <- df %>% 
    select(starts_with("Meta"), starts_with("Median")) 
  
  
  # vector of grouping column names
  grouping_cols <- c("Metadata_PlateID", "Metadata_WellID")
  
  
  # if data has not been filtered, the number of images (and thus 
  # rows) as 'no_images' for each well/plate grouping
  
  if (! filtered){
    
    assertthat::assert_that(dim(df)[1] %% no_images == 0,
                            msg = "Check that the number of rows in 'df' is a multiple of 'no_images'")
    
    # take sum of every 'no_images' rows to get per well data
    mat_sum <- apply(df_count[3:ncol(df_count),], 2, 
                     function(x) colSums(matrix(x, nrow = no_images)))
    
    
    # take median of every 'no_images' rows to get per well data
    mat_median <- apply(df_median[3:ncol(df_median),], 2, 
                        function(x) colMedians(matrix(x, nrow = no_images)))
    
    
    # get metadata rows, 1 for each well
    df_meta <- df %>%
      select(Metadata_PlateID, Metadata_WellID)
    
    nrow <- dim(df)[1]
    repeats <- nrow/no_images
    
    df_meta <- df_meta[ rep(c(TRUE, rep(FALSE, no_images - 1)), 
                            repeats), ]
    
    
    # join summarised matrices
    full_mat <- cbind(mat_sum, mat_median)
    
    df_final <- cbind(df_meta, as.data.frame(full_mat))
    
  }else{
    
    # when number of rows are not consistent for each plate/well
    # grouping, use data.table to aggregate and summarise
    # This take MUCH longer than above function 
    
    
    # get sum of count columns
    df_count <- df_count[ , lapply(.SD, 
                                   function(x) sum(x, na.rm = TRUE)),
                          by = grouping_cols]
    
    # get median of median columns
    df_final <- df[ , lapply(.SD, function(x) median(x, na.rm = TRUE)),
                   by = grouping_cols]
  
  }
  
  return(df_final)
  
  
}


