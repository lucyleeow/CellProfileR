#' Plot plate heatmaps
#' 
#' Plot a feature (column) as a plate heatmap for each plate, in the current
#' graphics device. Scale is uniform across all plates, such that the max 
#' value = red, min value = blue and median value = white.
#' 
#' @param df_PW Dataframe of per well Cell Profiler data.
#' @param column Name of the column (feature) to plot, as string. Matching
#'     for column name will be doing using grep but enough of the column name
#'     must be given to identify a single column in the dataframe.
#' @param plate Number of wells in a plate.
#' 
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @import ggplot2
#' 

#' @export
plot_plateHeat <- function(df_PW, column, plate = 384) {
  
  # check inputs
  assert_that(is.character(column),length(column) == 1, 
              msg = "Check that 'column' is single string")
  
  
  # grep for column name
  grep_pattern <- paste(column, "$", sep = "")
  column_name <- colnames(df_PW)[grep(grep_pattern, colnames(df_PW))]
  
  assert_that(! length(column_name) == 0,
              msg = "Did not find a column matching your given 'column' string")
  
  assert_that(! length(column_name) > 1,
              msg = "Found >1 columns matching your given 'column' string")
  
  
  # make sure df_PW is a data.table
  if (! "data.table" %in% class(df_PW)) {
    df_PW <- data.table::as.data.table(df_PW)
  }
  
  # find med, max and min to set common gradient scale
  feature_vect <- df_PW[ , get(column_name)]
  
  med <- median(feature_vect)
  max <- max(feature_vect)
  min <- min(feature_vect)
  
  
  # make plots
  plots <- df_PW %>%
    dplyr::group_by(Metadata_Barcode) %>%
    dplyr::do(plots = platetools::raw_map(data = .[,column_name],
                                          well = .$Metadata_WellID,
                                          plate = plate) + 
                ggtitle(.$Metadata_Barcode[1]) +
                scale_fill_gradient2(low = "blue", mid = "white",
                                     high = "red", midpoint = med,
                                     limits = c(floor(min), ceiling(max)))
    )
  
  for (i in 1:length(unique(df_PW$Metadata_Barcode))){
    
    print(plots$plots[[i]])
    
  }
  
}