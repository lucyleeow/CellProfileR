#' Plot distribution of negative control wells
#' 
#' Create boxplot of the negative control wells for each plate for a given 
#' feature.
#' 
#' @return Creates a plot in the current graphics device.
#' 
#' @param df Dataframe of Cell Profiler data including well annotations.
#' @param feature Name of the feature column to plot as string.
#' @param neg_ctrl The term labelling the negative control wells in the 
#'     annotation (e.g. "DMSO"), as string.
#' @param annot Name of the column containing the annotation data as string.
#'     
#' 
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @import ggplot2
#' 
#' @export
plot_negCtrl <- function(df, feature, annot, neg_ctrl){
  
  # check inputs
  assert_that(is.character(feature), length(feature) == 1,
              msg = "Check that 'feature' is a single string")
  
  assert_that(is.character(annot), length(annot) == 1,
              msg = "Check that 'annot' is a single string")
  
  assert_that(is.character(neg_ctrl), length(neg_ctrl) == 1,
              msg = "Check that 'neg_ctrl' is a single string")
  
  assert_that(sum(feature %in% colnames(df)) == 1,
              msg = "Check that your 'feature' is a column that exists in 'df'")
  
  assert_that(sum("Metadata_PlateID" %in% colnames(df)) == 1,
              msg = "Check that there is a column called 'Metadata_PlateID' in your 'df'")
  
  assert_that(sum(annot %in% colnames(df)) == 1,
              msg = "Check that 'annot' is a column that exists in 'df'")
  
  assert_that(sum(df[[annot]] == neg_ctrl) >= 1,
              msg = "Check taht 'neg_ctrl' is correct")
  
  
  # make plot
  title <- paste("Distribution of ", feature, " in negative controls", 
                 sep = "")
  
  return(
  df[df[,annot] == neg_ctrl,] %>%
    ggplot(aes_string(y = feature, x = "Metadata_PlateID")) + 
    geom_boxplot() + 
    labs(title = title) +
    coord_flip()
  )
    
}