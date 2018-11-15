#' Read in & combine
#' 
#' Read in all data output by Cell Profiler (e.g. for one cell line/experiment) 
#' and combine (bind rows) together into on data.table.
#' 
#' @param paths Character vector of folder paths the Cell Profiler data is in.
#'     Function will use \code{list.files} on each folder path and read in
#'     all files in each folder. Paths should NOT have a trailing "/".
#' @param sep The field separator character. Values on each line of the file 
#'     are separated by this character.
#' 
#' @importFrom assertthat assert_that
#' 

#' @export
readALL <- function(paths, sep = ",") {
  
  # check input
  assert_that(is.character(paths), msg = "Check that paths is a string")
  
  assert_that(is.character(sep), length(sep) == 1,
              msg = "Check that 'sep' is a single string")
  
  
  # list all files in paths
  all_files <- unlist(sapply(paths, function(x) list.files(x, 
                                                           full.names = TRUE)))

  # read and combine
  data_raw <- data.table::rbindlist(lapply(all_files, 
                                           function(x) 
                                             data.table::fread(x, sep = ",", 
                                                               header = TRUE)), 
                                    fill = TRUE, use.names = TRUE)
  # note fill = TRUE fills in missing columns with NAs and use.names checks
  # column names when binding.
  
  return(data_raw)
  
}

