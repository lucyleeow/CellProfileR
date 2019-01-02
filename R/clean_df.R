#' Clean raw data
#' 
#' Remove redundant columns from the raw Cell Profiler data.
#' 
#' @return A dataframe with only the necessary plate, well, count, median and
#'     image quality columns.
#'     
#' @param df Dataframe of raw Cell Profiler data.
#'
#' @importFrom magrittr %>%   
#'   
#' @export
clean_df <- function(df) {
  
  df_clean <- df %>% 
    dplyr::select(Metadata_Barcode, 
           Metadata_WellID,
           ImageNumber,
           Count_Cells,
           Count_Cells_unfiltered,
           starts_with("Median"),
           starts_with("ImageQuality_PowerLogLogSlope_Orig"))
  
  return(df_clean)
  
}