#############################################
# Functions to clean/manipulate raw data from 
# Cell Profiler
#
# Author: Lucy Liu
#############################################


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
  df,           # the df of raw (per image) data
  no_images     # the number of images taken per well
){
  
  # check function inputs
  
  assertthat::assert_that(dim(df)[1] %% no_images == 0,
                          msg = "Check that the number of rows in 'df' is a multiple of 'no_images'")
  
  
  assertthat::assert_that(is.numeric(no_images),
                          length(no_images) == 1,
                          msg = "Check 'no_images' is single number")
  
  
  # obtain only count columns
  df_count <- df %>% 
    select(starts_with("Count")) 
  
  # take sum of every 'no_images' rows to get per well data
  
  mat_sum <- apply(df_count, 2, 
             function(x) colSums(matrix(x, nrow = no_images)))
  
  # obtain only median columns
  df_median <- df %>% 
    select(starts_with("Median")) 
  
  # take median of every 'no_images' rows to get per well data
  
  mat_median <- apply(df_median, 2, 
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
  
  df_full <- cbind(df_meta, as.data.frame(full_mat))
  
  return(df_full)
  
}




