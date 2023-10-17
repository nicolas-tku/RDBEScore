#' Create an RDBES Data Object
#'
#' Create a RDBES Data object from either:
#'  - a `zip` file downloaded from RDBES (or multiple zip files if you want to include CL and CE data)
#'  - a folder containing `csv` files downloaded from RDBES (e.g. the unzipped archive file)
#'  - a `list` of data frames in the current environment representing different tables in the hierarchy
#'
#' @param x The path to the zip file extracted from RDBES, or path to a folder
#'   of `csv` files, or a list object in the current environment containing data frames of each table.
#'
#' @param listOfFileNames (Optional) A named list of file names - the list names
#'   shoudl be the two-letter code for the relevent table e.g. list("DE" =
#'   "DE.csv",... ).  If the parameter is not supplied then the default file
#'   names used by the RDBES data download will be used e.g. "Design.csv" etc.
#' @param castToCorrectDataTypes (Optional) If TRUE then the function will
#'   attempt to cast the required columns to the correct data type.  If FALSE
#'   then the column data types will be determined by how the csv files are read
#'   in.  The default is TRUE
#'
#' @return A RDBESDataObject.  If a path to RDBES extract files is provided then
#'   it will contain the data from those files.  If no path is supplied then an
#'   empty RDBESDataObject will be returned.
#' @export
#' @md
#'
#' @examples
#' myEmptyRDBESObject <- importRDBESDataCSV()

createRDBESDataObject <- function(x = NULL) {

  # x <- c("zip1", "zip2")
  # x <- c("csv1", "csv2")
  # x <- c("csv1", "zip2")
  #
  # if(!is.list(x))
  #   if(!(all(grepl("zip", x))) || !(all(grepl("csv", x)))) stop("Do not mix csv and zip inputs")
  #
  # #listOfFileNames = NA,
  # #castToCorrectDataTypes = TRUE)
  #
  # #  x <- "tests/testthat/h1_v_1_19_18/2023_10_16_104555.zip"
  #
  # # if input is string and zip file
  # if(is.character(x) && any(grepl(".zip", x))) {
  #   if((!(all(grepl("zip", x)))) stop("Cannot mix csv and zip inputs")
  #      import.type <- "zip"
  #      # if input is string and folder assume it contains only csv files
  #      } else if(is.character(x) && file_test("-d", x)) {
  #        import.type <- "folder"
  #        # if input is string and folder assume it contains only csv files
  #      } else if(is.list(x)) {
  #        import.type <- "list"
  #      } else {
  #        stop("x input type not recognised. Should be a RDBES zip file, folder of csv files, or list of data frames.")
  #      }
  # }

  #     # -------------------------------------------------------------------------
  #
  #     if(import.type == "zip") importRDBESDataZIP()
  #
  #
  #     if(import.type == "folder") importRDBESDataCSV()
  #
  #
  #     if(import.type == "list") {
  #       warning("NOTE: creating RDBES objects from list of data frames bypasses the ICES data checks. Make sure you know what you are doing.")
  #       importRDBESDataDFS()s
  #     }
  #
  #     # check for duplicate tables
  }
