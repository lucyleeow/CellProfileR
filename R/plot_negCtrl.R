#' Assess negative control wells
#' 
#' \itemize{
#'     \item \code{plot_negCtrl} Create boxplot of the negative control wells 
#'     for each plate for a given feature, in the current graphics device.
#'     \item \code{calc_negCtrl} will calculate the mean, standard deviation
#'     and coefficient of variation for the negative control wells for each 
#'     plate given a feature.
#' }
#' 
#' @param df Dataframe of Cell Profiler data including well annotations.
#' @param feature Name of the feature column to plot as string.
#' @param neg_ctrl The term labelling the negative control wells in the 
#'     annotation (e.g. "DMSO"), as string.
#' @param annot_col Name of the column containing the negative control terms
#'     as string.
#'     
#' 
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @importFrom rlang !!
#' @import ggplot2
#' 
#' @describeIn plot_negCtrl
#' @export
plot_negCtrl <- function(df, feature, annot_col, neg_ctrl){
  
  # check inputs
  ## character inputs
  character_args <- list(feature = feature, annot_col = annot_col,
                         neg_ctrl = neg_ctrl)
  
  for (i in 1:length(character_args)){
    
    msg <- paste("Check that '", names(character_args)[i], 
                 "' is a single string",
                 sep = "")
    
    assert_that(is.character(character_args[[i]]), 
                length(character_args[[i]]) == 1,
                msg = msg)
    
  }
  
  assert_that(sum(colnames(df) %in% annot_col) == 1,
              msg = "Check that your 'feature' is a column that exists in 'df'")
  
  assert_that(sum(colnames(df) %in% "Metadata_Barcode") == 1,
              msg = "Check that there is a column called 'Metadata_Barcode' in your 'df'")
  
  assert_that(sum(colnames(df) %in% annot_col) == 1,
              msg = "Check that 'annot_col' is a column that exists in 'df'")
  
  assert_that(sum(df[[annot_col]] == neg_ctrl) >= 1,
              msg = "Check that 'neg_ctrl' is correct")
  
  
  # make plot
  title <- paste("Distribution of ", feature, " in negative controls", 
                 sep = "")
  
  return(
  df[df[,annot_col] == neg_ctrl,] %>%
    ggplot(aes_string(y = feature, x = "Metadata_Barcode")) + 
    geom_boxplot() + 
    labs(title = title) +
    coord_flip()
  )
    
}



#' @describeIn plot_negCtrl
#' @export
calc_negCtrl <- function(df, feature, annot_col, neg_ctrl){
  
  # check inputs
  ## character inputs
  character_args <- list(feature = feature, annot_col = annot_col,
                         neg_ctrl = neg_ctrl)
  
  for (i in 1:length(character_args)){
    
    msg <- paste("Check that '", names(character_args)[i], 
                 "' is a single string",
                 sep = "")
    
    assert_that(is.character(character_args[[i]]), 
                length(character_args[[i]]) == 1,
                msg = msg)
    
  }
  
  assert_that(sum(colnames(df) %in% annot_col) == 1,
              msg = "Check that your 'feature' is a column that exists in 'df'")
  
  assert_that(sum(colnames(df) %in% "Metadata_Barcode") == 1,
              msg = "Check that there is a column called 'Metadata_Barcode' in your 'df'")
  
  assert_that(sum(colnames(df) %in% annot_col) == 1,
              msg = "Check that 'annot_col' is a column that exists in 'df'")
  
  assert_that(sum(df[[annot_col]] == neg_ctrl) >= 1,
              msg = "Check that 'neg_ctrl' is correct")
  
  
  df %>% 
    filter(!! as.name(annot_col) == "DMSO") %>%
    group_by(Metadata_Barcode) %>%
    summarise(Mean = mean(!! as.name(feature)),
              SD = sd(!! as.name(feature)),
              CV = SD/Mean * 100) %>%
    arrange(desc(CV))
  
}

