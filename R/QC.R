##############################################################
# Functions for performing QC steps on data from Cell Profiler
#
# Author: Lucy Liu
##############################################################

library(data.table)
library(tidyverse)

# Plot density plots of the 'ImageQuality_PowerLogLogSlope - '
# values (for a selected channel) for each plate
##############################################################

plot_plls <- function(
  df,       # df containing 'ImageQuality_PowerLogLogSlope' column
  channel   # channel of interest, as written in cell profiler 
  # output, as string
){
  
  columnName <- paste("ImageQuality_PowerLogLogSlope_Orig",
                      channel, sep = "")
  
  # check inputs
  assertthat::assert_that(columnName %in% colnames(df),
                          msg = "Make sure channel exists and is spelt correctly")
  
  # make plots
  plots <- df %>%
    group_by(Metadata_PlateID) %>%
    do(plots = ggplot(data = ., 
                      aes_string(x = columnName)) + 
         geom_density() +
         labs(title = .$Metadata_PlateID[1])
    )
  
  return(plots$plots)
  
}


# Remove images that have low PowerLogLogSlope value
######################################################

filterImages <- function(
  df,         # raw df (each row is 1 image, NOT 1 well)
  no_IQR      # number of IQR's below the 25% quantile to place cutoff threshold
){
  
  # check no_IQR argument
  assertthat::assert_that(is.numeric(no_IQR),
                          length(no_IQR) == 1, 
                          msg = "Check 'no_IQR' is a single number")
  
  # get column names of the powerloglog of all channels
  plls_columns <- colnames(
    df %>%
      select(starts_with("ImageQuality_PowerLogLogSlope_Orig"))
  )
  
  # check powerloglog column(s) exist 
  assertthat::assert_that(length(plls_columns) > 0,
                          msg = "Check df has a 'ImageQuality_PowerLogLogSlope_Orig' column")
  
  
  threshold_fun <- function(group_df){
    
    # obtain only columns of interest
    reduced_df <- group_df[,plls_columns]
    
    # create empty threshold vector
    threshold <- vector(mode = "numeric",
                        length = length(plls_columns))
    
    # fill vector with threshold for each powerloglog column
    
    for (i in 1:length(plls_columns)){
      
      threshold[i] <- 
        quantile(reduced_df[,i][[1]], 0.25) - 
        ( no_IQR * IQR(reduced_df[,i][[1]]) )
      
    }
    
    # for each row in grouped df, check all elements are greater
    # than threshold.
    # Result is vector of TRUEs and FALSEs the length of nrow.
    
    filter_rows <- apply(reduced_df, 1, function(x){
      sum(x > threshold) == length(plls_columns)}
    )
    
    # subset using vector above
    filtered_df <- group_df[filter_rows,]
    
    return(filtered_df)
    
  }
  
  # per form function on grouped df
  
  return(
    df %>%
      group_by(Metadata_PlateID) %>%
      do(threshold_fun(.))
  )
  
}


# Plot the number of images filtered 
############################################


plot_filtered <- function(
  df,            # the filtered df
  no_images,     # the number of images taken per well
  wells = 384    # the number of wells per plate
){
  
  # check inputs
  assertthat::assert_that(is.numeric(no_images),
                          length(no_images) == 1,
                          msg = "Check 'no_images' is single number")
  
  assertthat::assert_that(is.numeric(wells),
                          length(wells) == 1,
                          msg = "Check 'wells' is single number")
  
  # make plot
  df %>%
    group_by(Metadata_PlateID) %>%
    do(no_images * wells  - count(.)) %>%
    ggplot(aes(y=n, x=Metadata_PlateID)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of images filtered", y = "Number of images",
         x = "Plate") + 
    coord_flip()
  
}


# Plot the difference between count unfiltered and count
############################################################

plot_countfilt <- function(
  df      # df of raw data
){
  
  df %>%
    mutate(count_diff = Count_Cells_unfiltered - Count_Cells) %>%
    ggplot(aes(x = count_diff)) +
    geom_density() +
    labs(title = "Difference between unfiltered & filtered count",
         x = "Difference")
  
}


table_countfilt <- function(
  df,      # df of raw data
  n = 10   # number of rows to show
){
  
  df %>%
    select(starts_with("Meta"), 
           Count_Cells_unfiltered, 
           Count_Cells) %>%
    mutate(count_diff = Count_Cells_unfiltered - Count_Cells) %>%
    arrange(desc(count_diff)) %>%
    head(n=n)
  
}


plot_countImg <- function(
  df
)






