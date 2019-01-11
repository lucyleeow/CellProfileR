#' Filter images
#' 
#' Filter poor quality images (using the powerloglog slope feature). 
#' \code{plotFiltered} plots the number of images filtered for each plate.
#' 
#' @return 
#' \itemize{
#'     \item \code{filterImages} returns the dataframe with poor quality images
#'     (rows) removed.
#'     \item \code{plotFiltered} creates a bar plot in the current graphics
#'     device.
#' }
#' 
#' @param df Dataframe of raw CellProfiler data, where each row is 1 image
#'     NOT one well.
#' @param num_IQR Number of IQR's below the 25% quantile to place cutoff 
#'     threshold.
#' @param filtered_df The plate ID column of the dataframe of CellProfiler 
#'     data that has been filtered for poor quality data, as a vector. E.g.
#'     if the plate ID column from the filtered dataframe \code{filtered_df}
#'     is called 'Metadata_Barcode', \code{filtered_df[,'Metadata_Barcode']} 
#'     should be used.
#' @param num_images The number of images taken per well.
#' @param annot The plate ID column from the annotation data, as a vector. E.g.
#'     if the plate ID column from the \code{annot} dataframe is called 
#'     'VCFG_Compound_Plate_ID', \code{annot[,'VCFG_Compound_Plate_ID']} should
#'     be used.
#' 
#' 
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @import ggplot2
#' 

#' @describeIn filterImages Filter images (rows) with a powerloglog slope 
#'     value below the threshold ('num_IQR' * IQR below the 25th percentile) in 
#'     ANY channel.
#' @export
filterImages <- function(df, num_IQR) {
  
  # check inputs
  assert_that(is.numeric(num_IQR), length(num_IQR) == 1, 
              msg = "Check 'num_IQR' is a single number")
  
  ## get column names of the powerloglog of all channels
  plls_columns <- colnames(
    df %>%
      dplyr::select(dplyr::starts_with("ImageQuality_PowerLogLogSlope_Orig"))
  )
  
  ## check powerloglog column(s) exist 
  assert_that(length(plls_columns) > 0,
              msg = "Check that there is at least one 'ImageQuality_PowerLogLogSlope_Orig' 
              column in 'df'")
  
  
  # thresholding function
  threshold_fun <- function(group_df){
    
    # obtain only columns of interest
    reduced_df <- group_df[,plls_columns]
    
    # create empty threshold vector
    threshold <- vector(mode = "numeric", length = length(plls_columns))
    
    
    # fill vector with threshold for each channel (i.e. powerloglog column)
    for (i in 1:length(plls_columns)){
      
      threshold[i] <- 
        quantile(reduced_df[,i][[1]], 0.25) - 
        ( num_IQR * IQR(reduced_df[,i][[1]]) )
      
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
      dplyr::group_by(Metadata_Barcode) %>%
      dplyr::do(threshold_fun(.))
  )
  
}


#' @describeIn filterImages Creates bar graph of the number of images filtered 
#'     for each plate.
#' @export
plotFiltered <- function(filtered_df, num_images, annot) {
  
  # check inputs
  assert_that(is.numeric(num_images), length(num_images) == 1,
              msg = "Check 'num_images' is single number")
  
  assert_that(is.numeric(wells), length(wells) == 1,
              msg = "Check 'wells' is single number")
  
  
  # calculate number of used wells per plate
  wells <- table(annot)
  
  # calculate the number of rows per plate in the filtered data
  images_perPlate <- table(filtered_df)
  
  # calculate the number of filtered images
  num_filtered <- data.frame((wells * num_images) - images_perPlate)
  
  # make plot
  num_filtered %>%
    dplyr::group_by(Var1) %>%
    ggplot(aes(y=Freq, x=Var1)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of images filtered", y = "Number of images",
         x = "Plate") + 
    coord_flip() +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
}





