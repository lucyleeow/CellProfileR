#' Plot MP values
#' 
#' Creates density plots of the MP value or Mahalanobis distance.
#' 
#' @param mp_df Dataframe containing the MP values.
#' @param mp_col Name of the column containing the MP values, as string.
#' @param Mdist_col Name of the column containing the Mahalanobis distances,
#'     as string.
#' 
#' 
#' @importFrom assertthat assert_that
#' @import ggplot2
#' 
#' @describeIn plot_mp Creates a density plot of the MP values, in the 
#'     current graphics device.
#' @export
plot_mp <- function(mp_df, mp_col = "mp.value") {
  
  # check inputs
  assert_that(is.character(mp_col), length(mp_col) == 1,
              msg = "Check 'mp_col' is a single string")
  
  assert_that(sum(colnames(mp_df) == mp_col) == 1,
              msg = "Check there is one 'mp_col' column in 'mp_df'")
  
  
  ggplot(mp_df, aes_string(x = mp_col)) + 
    geom_density(fill = "#7570b3", alpha = 0.5) +
    labs(title = "MP value distribution for all comparisons", y = "Density",
         x = "MP value") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
}


#' @describeIn plot_mp Creates a density plot of the Mahalanobis distances,
#'     in the current graphics device.
#' @export
plot_Mdist <- function(mp_df, Mdist_col = "Mahalanobis") {
  
  # check inputs
  assert_that(is.character(Mdist_col), length(Mdist_col) == 1,
              msg = "Check 'mp_col' is a single string")
  
  assert_that(sum(colnames(mp_df) == Mdist_col) == 1,
              msg = "Check there is one 'Mdist_col' column in 'mp_df'")
  
  
  ggplot(mp_df, aes_string(x = Mdist_col)) + 
    geom_density(fill = "#7570b3", alpha = 0.5) +
    labs(title = "Mahalanobis distance distribution for all comparisons", 
         y = "Density", x = "Mahalanobis distance") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
}

