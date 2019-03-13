#' Raw CellProfiler data
#' 
#' Raw CellProfiler data from imaging cells in a 384 (24 by 16) well plate. 
#' Cells in each well were exposed to a different compound, left to grow and
#' the imaged using three stains; CMFDA, DAPI and Phalloidin. CellProfiler was 
#' then used to extract phenotype data from each image.
#' 
#' @format A data.table object with 1600 rows (800 rows per plate) and 371
#' variables. Each row provides metatdata and information extracted by 
#' CellProfiler from one image taken of cells grown in one well. There are
#' four rows (images) per well. There are two 384 well plates. Only 200 wells 
#' from each 384 well plate are included to minimise the size of the data set.
#' A number of columns output by CellProfiler were also removed (including 
#' 'Mean' and 'StDev' columns) to minimise the size of the data set. Details
#' of the columns are described below: 
#' \describe{
#'     \item{Metadata_Barcode}{Plate ID.}
#'     \item{Metadata_WellID}{Well ID. The plate is numbered such that the 
#'       columns go from 1-24 and the rows from A-P.}
#'     \item{Compound_ID}{The compound ID of the compound the well was exposed
#'       to. "DMSO" was the negative control compound.}
#'     \item{Concentration}{The concentration of the compound the well was
#'       exposed to. Several concentrations of the same compound was used for
#'       some compounds.}
#' }
#' 
#' The remaining columns were output by CellProfiler. Phenotype columns 
#' from CellProfiler have the general structure: 
#' `MeasurementType_Location_Category_SpecificFeatureName` 
#' \describe{
#'     \item{MeasurementType}{This tells you the type of data contained in the
#'       measurement. Most columns are 'Median', which means that the median
#'       value of that measurement, for all cells identified in that image, is
#'       provided.}
#'     \item{Location}{The location of the cell this measurement is from. This
#'       is often 'Nuclei', 'Cytoplasm' or 'Actin' (which captures  
#'       cell cytoskeleton details.)}
#'     \item{Category}{Description of the type of measurement taken. For 
#'       example 'AreaShape' provides information about the area or shape of
#'       a cell location and 'Texture' provides texture information about
#'       a cell location.}
#'     \item{SpecificFeatureName}{Details about the measurement made.}
#' }
#' 
#' More details about how each column (variable) is named can be found 
#' \href{http://cellprofiler-manual.s3.amazonaws.com/CellProfiler-3.1.5/help/output_measurements.html}{here}
#' and details about how each variable is calculated can be found 
#' \href{http://cellprofiler-manual.s3.amazonaws.com/CellProfiler-3.1.5/modules/measurement.html?highlight=available%20measurements}{here}.
#' 
#' 
"CP_data"