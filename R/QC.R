##############################################################
# Functions for performing QC steps on data from Cell Profiler
#
# Author: Lucy Liu
##############################################################

library(platetools)
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
  
  # perform function on grouped df
  
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


# Plot the difference between 'count unfiltered' and 'count'
# &
# Produce df containing the n largest differences between 
# count unfiltered and count
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
           ImageNumber,
           Count_Cells_unfiltered, 
           Count_Cells) %>%
    mutate(count_diff = Count_Cells_unfiltered - Count_Cells) %>%
    arrange(desc(count_diff)) %>%
    head(n=n)
  
}


# Produce df containing per well 'count' & 'count_unfiltered' 
# coloumns, of full raw data and image QC filtered data
############################################################

df_ImageQCcounts <- function(
  df_full,       # df of raw unfiltered data
  df_filtered    # df filtered of poor quality images (rows)
){
  
  # check inputs
  assertthat::assert_that(nrow(df_full)>nrow(df_filtered),
                          msg = "Filtered df should be smaller than full df. Check arguments are in correct order")
  
  # create list of the 2 input dfs
  df_list <- list(df_full, df_filtered)
  
  # convert both inputs to data.tables
  df_list <- lapply(df_list, as.data.table)
  
  
  # subset only count columns
  
  subset_fun <- function(df){
    return(
      df %>%
        select(starts_with("Meta"), starts_with("Count"))
    )
  }
  
  ## apply function to list of inputs
  subsetted_dfs <- lapply(df_list, subset_fun)

  
  # group by plate & well and sum counts
  
  ## vector of grouping column names
  grouping_cols <- c("Metadata_PlateID", "Metadata_WellID")
  
  grouping_fun <- function(df){
    
    return(
      df[ , lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
          by = grouping_cols]
    )
    
  }
  
  ## apply function to list of inputs
  summed_dfs <- lapply(subsetted_dfs, grouping_fun)
  
  # Add column indicating whether count came from filtered or 
  # unfiltered df
  
  summed_dfs[[1]]$Type <- "Fullimages"
  
  summed_dfs[[2]]$Type <- "Filteredimages"
  
  # join dfs together
  bound_df <- rbind(summed_dfs[[1]], summed_dfs[[2]])
  
  # reshape into wide form
  reshaped_df <- dcast(bound_df, 
                    Metadata_PlateID + Metadata_WellID ~ Type,
                    value.var = c("Count_Cells", 
                                  "Count_Cells_unfiltered"))
  # 
  # final_df <- reshaped_df %>%
  #   mutate(diff_filtered = )
  
  return(reshaped_df)
  
  # Author seems to really like lapply() today....
}


# Plot difference between count before and after removing poor
# quality images
############################################################

plot_countImageQC <- function(
  df_full,                  # df of raw unfiltered data
  df_filtered,              # df filtered of poor quality images (rows)
  CP_unfiltered = FALSE     # single logical indicating whether count or
                            # count_unfiltered column (from CP filtering) 
                            # is to be compared
){
  
  # check inputs
  assertthat::assert_that(is.logical(CP_unfiltered),
                          length(CP_unfiltered) == 1,
                          msg = "Check 'CP_unfiltered' is single logical")
  
  
  # Use helper function to obtain df of count values
  count_df <- df_ImageQCcounts(df_full,df_filtered)
  
  # columns to compare
  cols_CPfiltered <- colnames(count_df)[grep("Count_Cells_F",
                                         colnames(count_df))]
  cols_CPunfiltered <- colnames(count_df)[grep("unfiltered",
                                           colnames(a))]
  
  if (CP_unfiltered){
    
    diff_df <- count_df %>%
      mutate(diff_filt = !!as.name(cols_CPunfiltered[2]) - 
               !!as.name(cols_CPunfiltered[1]))
    
  }else{
    
    diff_df <- count_df %>%
      mutate(diff_filt = !!as.name(cols_CPfiltered[2]) - 
               !!as.name(cols_CPfiltered[1]))
    
  }
  
  # make plots
  plots <- diff_df %>%
    group_by(Metadata_PlateID) %>%
    do(plots = ggplot(data = .,
                      aes(x = diff_filt)) +
         geom_density() + 
         labs(title = .$Metadata_PlateID[1], y = "Density",
              x = "Difference in count before and after image QC")
    )
  
  
  return(plots$plots)
  
}


# Plot a feature (column) as plate heatmap for each plate.
# Scale is uniform across all plates
############################################################

plot_plateheat <- function(
  df,          # df of per well data
  column,      # approximate column name of feature of interest as string 
               # matching will be done with grep
  plate = 384  # number of wells in plate
){
  
  # check inputs
  assertthat::assert_that(is.character(column),
                          length(column) == 1,
                          msg = "Check that 'column' is single character")
  
  
  # grep for column name
  grep_pattern <- paste(column, "$", sep = "")
  column_full <- colnames(df)[grep(grep_pattern, colnames(df))]
  
  assertthat::assert_that(length(column_full) == 1,
                          msg = "Did not find single column when matching with 'column'. Try different 'column' string")
  
  
  # find med, max and min to set common gradient scale
  feature_vect <- df[ , get(column_full)]
  
  med <- median(feature_vect)
  max <- max(feature_vect)
  min <- min(feature_vect)
  
  
  # make plots
  plots <- df %>%
    group_by(Metadata_PlateID) %>%
    do(plots = raw_map(data = .[,column_full],
                       well = .$Metadata_WellID,
                       plate = plate) + 
         ggtitle(.$Metadata_PlateID[1]) +
         scale_fill_gradient2(low = "blue", mid = "white",
                              high = "red", midpoint = med,
                              limits = c(floor(min), ceiling(max)))
       )
  
  return(plots$plots)
  
}



