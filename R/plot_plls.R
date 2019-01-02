#' Plot powerloglog slope
#' 
#' Plot the density of the image quality metric, powerloglog slope for a 
#' specified channel, for each plate, in the current graphics device.
#' 
#' @param df Dataframe containing a 'ImageQuality_PowerLogLogSlope' column
#' @param channel Name of the channel to be plotted, exactly as written in
#'     Cell Profiler output, as string.
#'     
#' @importFrom assertthat assert_that
#' @import ggplot2
#' @importFrom magrittr %>% 
#' 
#' @export
plot_plls <- function(df, channel) {
  
  columnName <- paste("ImageQuality_PowerLogLogSlope_Orig",
                      channel, sep = "")
  
  # check inputs
  assert_that(columnName %in% colnames(df),
              msg = "Make sure channel exists and is spelt correctly")
  
  # make plots
  plots <- df %>%
    dplyr::group_by(Metadata_Barcode) %>%
    dplyr::do(plots = ggplot(data = ., 
                      aes_string(x = columnName)) + 
         geom_density() +
         labs(title = .$Metadata_Barcode[1])
    )
  
  for (i in 1:length(unique(df$Metadata_Barcode))){
    
    print(plots$plots[[i]])
    
  }
  
}