#' Filter features
#' 
#' Filters features (columns) of Cell Profiler data to exclude any columns 
#' with >1 NA/NaN or low variance.
#' 
#' @return A dataframe in the same format as the input df but with columns
#'     filtered.
#'
#' @param df Dataframe of Cell Profiler data.
#' 
#'
#' @describeIn filter_NA
#' @export
filter_NA <- function(df){
  
  # calculate total NAs for each column
  col_NAs <- apply(df, 2, function(x) sum(is.na(x)))
  
  if (sum(col_NAs > 0)) {
    
    print("There were no columns with >0 NA's")
    
  } else {
    
    print(paste("There were ", sum(col_NAs > 0), " columns with >0 NA's and will all be removed"))
    
    # filter columns
    df <- df[,col_NAs == 0]
    
  }
  
  return(df)
  
}


#' @describeIn filter_NA
#' @export
filter_lowVar <- function(df){
  
  
}