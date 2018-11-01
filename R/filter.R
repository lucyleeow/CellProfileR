#' Filter features
#' 
#' Filters features (columns) of Cell Profiler data to exclude any columns 
#' with >1 NA/NaN or low variance.
#' 
#' @return A dataframe in the same format as the input df but with columns
#'     filtered.
#'
#' @param df Dataframe of Cell Profiler data.
#' @param NA_cutoff 
#' 
#' @importFrom assertthat assert_that
#'
#' @describeIn filter_NA
#' @export
filter_NA <- function(df, NA_cutoff){

  # check inputs
  assert_that(is.numeric(NA_cutoff), length(NA_cutoff) == 1,
              msg = "Check that 'NA_cutoff' is a single number")
  
  
  # calculate total NAs for each column
  col_NAs <- apply(df, 2, function(x) sum(is.na(x)))
  
  if (sum(col_NAs > 0)) {
    
    print("There were no columns with >0 NA's")
    
  } 
  
  if (missing(NA_cutoff)){
    
    print(paste("There were ", sum(col_NAs > 0), " columns with >0 NA's and all will be removed"))
    
    # filter columns
    df <- df[,col_NAs == 0]
    
  } else {
    
  filter_num <- sum(col_NAs < NA_cutoff)

  print(paste("There were ", filter_num, " columns with >", NA_cutoff, " NAs and these will be removed", sep = ""))
  
  # filter columns
  df <- df[,col_NAs > NA_cutoff]
    
  }
  
  return(df)
  
}


#' @describeIn filter_NA
#' @export
filter_lowVar <- function(df){
  
  
}