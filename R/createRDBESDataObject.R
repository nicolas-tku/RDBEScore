#' Create an RDBES Raw Object
#'
#' Create an R object from a folder containing .csv data files downloaded from
#' the RDBES upload/download page.
#'
#' @param rdbesExtractPath (Optional) The path to the csv files produced as an
#' extract by the ICES RDBES.  If no path is suppled then an empty
#' RDBESDataObject will be returned.
#' @param listOfFileNames (Optional) A named list of file names - the list names
#' shoudl be the two-letter code for the relevent table e.g.
#' list("DE" = "DE.csv",... ).  If the parameter is not supplied then the
#' default file names used by the RDBES data download will be used e.g.
#' "Design.csv" etc.
#' @param castToCorrectDataTypes (Optional) If TRUE then the function
#' will attempt to cast the required columns to the correct data type.  If
#' FALSE then the column data types will be determined by how the csv files
#' are read in.  The default is TRUE
#'
#' @return A RDBESDataObject.  If a path to RDBES extract files is provided then
#' it will contain the data from those files.  If no path is supplied then
#' an empty RDBESDataObject will be returned.
#' @export
#'
#' @examples
#' myEmptyRDBESObject <- importRDBESDataCSV()

createRDBESDataObject <- function(x = NULL) {
  #listOfFileNames = NA,
  #castToCorrectDataTypes = TRUE)

  #  x <- "tests/testthat/h1_v_1_19_18/2023_10_16_104555.zip"

  # if input is string and zip file
  if(is.character(x) && grepl(".zip", x)) import.type <- "zip" else
    # if input is string and folder assume it contains only csv files
    if(is.character(x) && file_test("-d", x)) import.type <- "folder" else
      # if input is string and folder assume it contains only csv files
      if(is.list(x)) import.type <- "list" else
        stop("x input type not recognised. Should be a RDBES zip file, folder of csv files, or list of data frames.")


      # -------------------------------------------------------------------------

      if(import.type == "zip") importRDBESDataZIP()


      if(import.type == "folder") importRDBESDataCSV()


      if(import.type == "list") importRDBESDataCSV()


}
