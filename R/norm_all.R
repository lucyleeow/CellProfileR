#' Normalise data
#' 
#' Takes in CellProfiler morphology data and uses \code{reshape::rescaler} to 
#' normalise all data columns. Also calculates count value normalised to the
#' negative control, for each plate, and retains the raw count value.
#' 
#' @return A dataframe with the following columns, (in order): 
#' \itemize{
#'     \item Annotation columns (all non-data columns).
#'     \item Cell count columns: raw cell count, cell count normalised to 
#'     negative control, z score normalised cell count (using 
#'     \code{reshape::rescaler}).
#'     \item All remaining data columns z score normalised (using 
#'     \code{reshape::rescaler}). All of these columns names will be the same 
#'     as the input dataframe.
#' }
#'
#' @param df Dataframe containing the CellProfiler data to normalise.
#' @param data_cols Numeric vector. Indicies of the data columns of df. All of 
#'     these columns should be of numerice type. All remaining (non 'data_cols')
#'     should NOT be of numeric type.
#' @param count_col Name of the column containing the raw cell counts, as 
#'     string.
#' @param compound_col Name of the column in 'df' containing the compound 
#'     details.
#' @param negCtrl Name of the negative control compound.
#' @param rescale_type Type of rescaling to use, as string. Will be passed to 
#'     \code{reshape::rescaler}.
#' @param plate_col Name of the column containing the plate IDs.
#'
#'
#'
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' 
#' @export
norm_all <- function(df, data_cols, count_col = "Count_Cells", compound_col, 
                     negCtrl, rescale_type = "robust", 
                     plate_col = "Metadata_Barcode") {
  
  # check inputs
  assert_that(is.numeric(data_cols), 
              msg = "Check 'data_cols' is of numeric type")
  
  
  ## character inputs
  character_args <- list(count_col = count_col, compound_col = compound_col, 
                         negCtrl = negCtrl, rescale_type = rescale_type)
  
  for (i in 1:length(character_args)){
    
    msg <- paste("Check that '", names(character_args)[i], 
                 "' is a single string",
                 sep = "")
    
    assert_that(is.character(character_args[[i]]), 
                length(character_args[[i]]) == 1,
                msg = msg)
    
  }
  
  ## check column data type
  check_numCols <- apply(df[,data_cols], 2, is.numeric)
  
  assert_that(sum(check_numCols) == length(check_numCols),
              msg = "Check all your 'data_cols' are of numeric type")
  
  check_non_numCols <- apply(df[,-data_cols], 2, is.numeric)
  
  assert_that(sum(check_non_numCols) == 0,
              msg = "Check all your NON 'data_cols' are NOT of numeric type")


  
  # z score normalisation
  norm_z <- df %>%
    dplyr::group_by(Metadata_Barcode) %>%
    dplyr::do(reshape::rescaler(., type = rescale_type)) %>%
    dplyr::ungroup()
  
  print("norz_z done")
  
  ## rename count column
  colnames(norm_z)[colnames(norm_z) == count_col] <- "CP_Zscore_Count_Cells"

  
  # norm to DMSO
  CP_NormToNeg_Count_Cells <- df %>%
    dplyr::group_by(!! as.name(plate_col)) %>%
    dplyr::do(mutate(., CP_NormToNeg_Count_Cells = .[[count_col]] /
             mean(.[[count_col]][.[[compound_col]] == negCtrl], 
                  na.rm = TRUE))) %>%
    dplyr::select(CP_NormToNeg_Count_Cells)
  
  print("norm to neg done")
  
  
  # Raw count data
  CP_RAW_count <- df[,count_col]
  
  # combine with annotation columns first, then count columns, then 'Median'
  # columns
  count_zscore_col <- which(colnames(norm_z) == "CP_Zscore_Count_Cells")
  med_cols <- data_cols[! data_cols == count_zscore_col]
  
  final_df <- cbind(norm_z[,-data_cols], CP_RAW_count, 
                    CP_NormToNeg_Count_Cells, norm_z[,"CP_Zscore_Count_Cells"],
                    norm_z[,med_cols])
  
  return(final_df)
  
}



