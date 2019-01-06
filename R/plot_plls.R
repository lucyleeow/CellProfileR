#' Plot powerloglog slope
#' 
#' Plot the density of the image quality metric, powerloglog slope for a 
#' specified channel, for each plate, in the current graphics device.
#' 
#' @param df Dataframe containing a 'ImageQuality_PowerLogLogSlope' column
#' @param channel Name of the channel to be plotted, exactly as written in
#'     Cell Profiler output, as string.
#' @param scale Single logical. If TRUE the scale of all plots will be uniform.
#'     The x-axis limits will be set to the min and max of the powerloglog slope 
#'     for all plates.
#'     
#' @importFrom assertthat assert_that
#' @import ggplot2
#' @importFrom magrittr %>% 
#' 
#' @export
plot_plls <- function(df, channel, scale) {
  
  columnName <- paste("ImageQuality_PowerLogLogSlope_Orig",
                      channel, sep = "")
  
  # check inputs
  assert_that(columnName %in% colnames(df),
              msg = "Make sure channel exists and is spelt correctly")
  
  assert_that(is.logical(scale), length(scale) == 1,
              msg = "Check 'scale' is a single logical")
  
  # make plots
  if (scale) {
    
    min_plls <- min(data_raw[[columnName]])
    max_plls <- max(data_raw[[columnName]])
    
    plots <- df %>%
      dplyr::group_by(Metadata_Barcode) %>%
      dplyr::do(plots = ggplot(data = ., aes_string(x = columnName)) + 
                  geom_density(fill = "#7570b3", alpha = 0.5) +
                  coord_cartesian(xlim = c(min_plls,max_plls)) +
                  labs(title = .$Metadata_Barcode[1]) +
                  theme_minimal()
      )
    
  } else {
    
    plots <- df %>%
      dplyr::group_by(Metadata_Barcode) %>%
      dplyr::do(plots = ggplot(data = ., aes_string(x = columnName)) + 
                  geom_density(fill = "#7570b3", alpha = 0.5) +
                  labs(title = .$Metadata_Barcode[1]) +
                  theme_minimal()
      )
    
  }
  
  # print plots
  for (i in 1:length(unique(df$Metadata_Barcode))){
    
    print(plots$plots[[i]])
    
  }
  
}