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
  if (! missing(NA_cutoff)) {
  assert_that(is.numeric(NA_cutoff), length(NA_cutoff) == 1,
              msg = "Check that 'NA_cutoff' is a single number")
  }
  
  # calculate total NAs for each column
  col_NAs <- apply(df, 2, function(x) sum(is.na(x)))
  
  if (sum(col_NAs >= 1) == 0) {
    
    print("There were no columns with >0 NA's")
    
  } else if (missing(NA_cutoff)){
    
    filter_cols <- colnames(df)[col_NAs >= 1]
    cat("The columns:\n", filter_cols, "were removed")
    
    # keep columns with 0 NA values only
    df <- df[,col_NAs == 0]

  } else {
    
  filter_cols <- colnames(df)[col_NAs > NA_cutoff]
  cat("The columns:\n", filter_cols, "were removed")
  
  
  # filter columns
  df <- df[,col_NAs <= NA_cutoff]
    
  }
  
  return(df)
  
}

#' @describeIn filter_NA
#' @export
filter_lowVar <- function(df, freqCut = 95/5, uniqueCut = 10){
  
  to_filter <- caret::nearZeroVar(df, freqCut, uniqueCut)
  
  if (length(to_filter) == 0) {
    
    cat("There were no columns with near zero variance")
    
  } else {
    
    filter_cols <- colnames(df)[to_filter]
    cat("The columns:\n", filter_cols, "were removed")
    
    # filter df
    df <- df[,-filter_cols]
    
  }
  
  return(df)
  
}




