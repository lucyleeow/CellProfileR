#' Count differences
#' 
#' Determine the differences in count values between images (rows) before and
#' after Cell Profiler filtering or manual filtering of poor quality and 
#' present as dataframe or plot in the current graphics device.
#' 
#' 
#' @param df_raw Dataframe of raw Cell Profiler data with no images (rows)
#'     removed. Each row should be 1 image NOT 1 well.
#' @param df_filtered Dataframe of Cell Profiler data after filtering for poor
#'     quality images (rows). Again, each row should be 1 image NOT 1 well.
#' @param n Number of rows (images), with the largest differences, to include
#'     in the output dataframe.
#' @param CP_unfiltered Single logical indicating whether the differences 
#'     (before and after image filtering) should be calculted and shown for the
#'     CP filtered column 'Count_Cells' or CP unfiltered column 
#'     'Count_Cells_unfiltered'. 
#'

#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @import ggplot2
#' @import data.table
#' 

#' @describeIn plot_countFilt Calculates the difference between 
#'     'Count_Cells_unfiltered' and 'Count_Cells' (Cell Profiler filtering)
#'     for each row (image) and creates a density plot of the differences for 
#'     ALL plates.
#' @export
plot_countFilt <- function(df_raw) {
  
  df_raw %>%
    dplyr::mutate(count_diff = Count_Cells_unfiltered - Count_Cells) %>%
    ggplot(aes(x = count_diff)) +
    geom_density() +
    labs(title = "Difference between unfiltered & filtered count",
         x = "Count difference")
  
}

#' @describeIn plot_countFilt Calculates the difference between 
#'     'Count_Cells_unfiltered' and 'Count_Cells' (Cell Profiler filtering) 
#'     for each row (image) and outputs a dataframe containing the 'n' rows 
#'     (images) that have the largest differences. Plate, well and image number 
#'     also shown in dataframe.
#' @export
df_countFilt <- function(df_raw, n = 10) {
  
  df_raw %>%
    dplyr::select(dplyr::starts_with("Meta"),
           ImageNumber,
           Count_Cells_unfiltered, 
           Count_Cells) %>%
    dplyr::mutate(count_diff = Count_Cells_unfiltered - Count_Cells) %>%
    dplyr::arrange(dplyr::desc(count_diff)) %>%
    head(n=n)
  
}


#' @describeIn plot_countFilt Calculates the per well total count before and 
#'     after filtering for poor quality images, for BOTH the 
#'     'Count_Cells_unfiltered' and 'Count_Cells' column (CP unfiltered and
#'     filtered). Mostly for internal use but made available as well.
#' @export
calc_countsImageQC <- function(df_raw, df_filtered) {
  
  # check inputs
  assert_that(nrow(df_raw) > nrow(df_filtered),
              msg = "Filtered df should be smaller than full df. Check arguments
              are in correct order")
  
  
  # create list of the 2 input dfs
  df_list <- list(df_raw, df_filtered)
  
  # convert both inputs to data.tables
  df_list <- lapply(df_list, as.data.table)
  
  
  # subset only count and Meta columns
  subset_fun <- function(df){
    return(
      df %>%
        dplyr::select(dplyr::starts_with("Meta"), dplyr::starts_with("Count"))
    )
  }
  
  ## apply function to list of inputs
  subsetted_dfs <- lapply(df_list, subset_fun)
  
  
  # group by plate & well and sum counts
  
  ## vector of grouping column names
  grouping_cols <- c("Metadata_Barcode", "Metadata_WellID")
  
  grouping_fun <- function(df){
    
    return(
      df[ , lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
          by = grouping_cols]
    )
    
  }
  
  ## apply function to list of inputs
  summed_dfs <- lapply(subsetted_dfs, grouping_fun)
  
  # Add column indicating whether count came from raw or image quality filtered
  # df
  
  summed_dfs[[1]]$Type <- "Fullimages"
  
  summed_dfs[[2]]$Type <- "Filteredimages"
  
  # join dfs together
  bound_df <- rbind(summed_dfs[[1]], summed_dfs[[2]])
  
  # reshape into wide form
  reshaped_df <- data.table::dcast(bound_df, 
                                   Metadata_Barcode + Metadata_WellID ~ Type,
                                   value.var = c("Count_Cells", 
                                                 "Count_Cells_unfiltered"))

  return(reshaped_df)

}


#' @describeIn plot_countFilt Calculates the difference in total count per well
#'     before and after filtering for poor quality images (for either the 
#'     'Count_Cells_unfiltered' or 'Count_Cells' column) then plots density 
#'     plot of the differences for EACH plate.
#' @export
plot_countImageQC <- function(df_raw, df_filtered, CP_unfiltered = FALSE) {
  
  # check inputs
  assert_that(is.logical(CP_unfiltered), length(CP_unfiltered) == 1,
              msg = "Check 'CP_unfiltered' is single logical")
  
  
  # Use helper function to obtain df of count values
  count_df <- calc_countsImageQC(df_raw, df_filtered)
  
  # columns to compare. Output cols - 1: filtered, 2: full
  cols_CPfiltered <- colnames(count_df)[grep("Count_Cells_F", 
                                             colnames(count_df))]
  cols_CPunfiltered <- colnames(count_df)[grep("unfiltered", 
                                               colnames(count_df))]
  
  
  if (CP_unfiltered) {
    
    diff_df <- count_df %>%
      dplyr::mutate(diff = !!as.name(cols_CPunfiltered[2]) -
                      !!as.name(cols_CPunfiltered[1]))
    
  }else{
    
    diff_df <- count_df %>%
      dplyr::mutate(diff = !!as.name(cols_CPfiltered[2]) -
                      !!as.name(cols_CPfiltered[1]))
    
  }
  
  # make plots
  plots <- diff_df %>%
    dplyr::group_by(Metadata_Barcode) %>%
    dplyr::do(plots = ggplot(data = ., aes(x = diff)) +
         geom_density() +
         labs(title = .$Metadata_Barcode[1], y = "Density",
              x = "Difference in count before and after image QC")
    )
  
  for (i in 1:length(unique(diff_df$Metadata_Barcode))){
    
    print(plots$plots[[i]])
    
  }
  
}


#' @describeIn plot_countFilt Calculates the difference in total count per well
#'     before and after filtering for poor quality images (for either the 
#'     'Count_Cells_unfiltered' or 'Count_Cells' column) then produces a 
#'     dataframe containing the 'n' rows (wells) with the largest differences.
#' @export
df_countImageQC <- function(df_raw, df_filtered, n = 10, 
                            CP_unfiltered = FALSE) {
  
  # check inputs
  assert_that(is.logical(CP_unfiltered), length(CP_unfiltered) == 1,
              msg = "Check 'CP_unfiltered' is single logical")
  
  assert_that(is.numeric(n), length(n) == 1, 
              msg = "Check 'n' is single number")
  
  
  # Use helper function to obtain df of count values
  count_df <- calc_countsImageQC(df_raw, df_filtered)
  
  # columns to compare. Output cols - 1: filtered, 2: full
  cols_CPfiltered <- colnames(count_df)[grep("Count_Cells_F", 
                                             colnames(count_df))]
  cols_CPunfiltered <- colnames(count_df)[grep("unfiltered", 
                                               colnames(count_df))]
  
  
  if (CP_unfiltered){
    
    diff_df <- count_df %>%
      dplyr::mutate(diff = !!as.name(cols_CPunfiltered[2]) - 
               !!as.name(cols_CPunfiltered[1])) %>%
      dplyr::select(- !!(as.name(cols_CPfiltered[1]))) %>%
      dplyr::select(- !!(as.name(cols_CPfiltered[2])))
    
  }else{
    
    diff_df <- count_df %>%
      dplyr::mutate(diff = !!as.name(cols_CPfiltered[2]) - 
               !!as.name(cols_CPfiltered[1])) %>%
      dplyr::select(- !!(as.name(cols_CPunfiltered[1]))) %>%
      dplyr::select(- !!(as.name(cols_CPunfiltered[2])))
    
  }
  
  return(
    diff_df %>%
      dplyr::group_by(Metadata_Barcode) %>%
      dplyr::arrange(dplyr::desc(diff)) %>%
      head(n=n)
  )
  
}


