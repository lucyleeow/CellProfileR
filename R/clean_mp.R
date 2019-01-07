#' Clean MP data
#' 
#' If each unique treatment and concentration was treated as a separate 
#' treatment in \code{calc_mp}, the "tx" column of the output would contain the 
#' treatment and concentration concatenated together. This function takes the 
#' dataframe output by \code{calc_mp} and outputs the dataframe with two extra 
#' columns; 'conc' which contains just the concentration and 'compound', which 
#' contains just the compound name.
#' 
#' Note that this function assumes that each concatenated string has the 
#' treatment first and then the concentration. It also assumes that the 
#' concentration is numerical (does not contain any non-numerical characters) 
#' and does not contain the 'sep' symbol.
#'  
#' 
#' @param mp_df Dataframe output from \code{calc_mp}, containing mp-values and
#'     treatment and batch details.
#' @param sep Symbol separating the treatment and concentration in the 
#'     concatenated column, as string.
#' @param tx_col Name of the column containing the concatenated treatment and
#'     concentration data.
#'     
#' @importFrom assertthat assert_that
#' 
#' 
#' @export
clean_mp <- function(mp_df, sep, tx_col = "tx") {
  
  # check inputs
  assert_that(is.character(sep), length(sep) == 1,
              msg = "Check that 'sep' is a single string")
  
  assert_that(is.character(tx_col), length(tx_col) == 1,
              msg = "Check that 'tx_col' is a single string")
  
  assert_that(sum(colnames(mp_df) == tx_col) == 1,
              msg = "Check there is one 'tx_col' in 'mp_df'")
  
  
  # extract concentration
  conc <- sapply(strsplit(mp_df[[tx_col]], sep), function(x) x[length(x)])
  
  mp_df$conc <- as.numeric(conc)
  
  # extract compound
  compound <- sapply(strsplit(mp_df[[tx_col]], sep), 
                     function(x) paste(x[1:(length(x) - 1)], collapse = sep))
  
  mp_df$compound <- as.character(compound)
  
  # re-order columns
  mp_ncols <- ncol(mp_df)
  
  mp_df <- mp_df[ ,c(1,2, mp_ncols, (mp_ncols-1), 3:(mp_ncols-2))]
  
  return(mp_df)
  
}


