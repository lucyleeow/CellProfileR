#' Clean raw data
#' 
#' Remove redundant columns from the raw Cell Profiler data.
#' 
#' @return A dataframe with only the necessary plate, well, count, median and
#'     image quality columns.
#'     
#' @param df Dataframe of raw Cell Profiler data.
#' @param meta_col Name of the column that identifies the plate each 
#'     observation belongs to, as string.
#' 
#'
#' @importFrom magrittr %>%   
#'   
#' @export
clean_df <- function(df, meta_col = "Metadata_Barcode") {
  
  # check input
  assert_that(is.character(meta_col), length(meta_col) == 1,
              msg = "Check 'meta_col' is a single string")
  
  
  df_clean <- df %>% 
    dplyr::select(!! as.name(meta_col), 
           Metadata_WellID,
           ImageNumber,
           Count_Cells,
           Count_Cells_unfiltered,
           dplyr::starts_with("Median"),
           dplyr::starts_with("ImageQuality_PowerLogLogSlope_Orig"))
  
  return(df_clean)
  
}