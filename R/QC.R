##############################################################
# Functions for performing QC steps on data from Cell Profiler
#
# Author: Lucy Liu
##############################################################


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
  assertthat::assert_that(is.numeric(no_images),
                          length(no_images) == 1, 
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
                        length = length(columns))
    
    # fill vector with threshold for each powerloglog column
    
    for (i in 1:length(columns)){
      
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


