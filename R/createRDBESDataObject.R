#' Create an RDBES Data Object
#'
#' This function lets you create an RDBES Data object in your current R
#' environment.
#'
#' The `input` should be either:
#'  - A `zip` file downloaded from RDBES (or multiple zip files if you want to include or overwrite tables, for example CL and CE data)
#'  - A folder containing `csv` files downloaded from RDBES (e.g. the unzipped file), or any set of csv files of the RDBES tables.
#'  - A `list` of data frames in the current environment representing different tables in the hierarchy.
#'  - A `NULL` input will return and emopty RDBES data object
#'
#' @details
#'
#' ***ZIP file inputs***
#' This input should be a path to a zip file downloaded from RDBES. Multiple zip
#' files can be entered if you want to include additional tables, for example CL
#' and CE. The main hierarchy input should be the first input. E.g. `input =
#' c("path/to/H1.zip", "path/to/CL.zip"). If any tables in the first input will
#' be overwritten by other inputs a warning is given.
#'
#' ***CSV file inputs***
#'
#' ***List of data frames inputs***
#'
#' ***NULL inputs***
#'
#'
#'
#' @param input Strings or `list` object. The path to the zip file extracted
#'   from RDBES (or multiple zip files - see details), or path to a folder of
#'   `csv` files, or a list object in the current environment containing data
#'   frames of each table. If `NULL` an emoty `RDBESDataObject` is created.
#' @param listOfFileNames `list` of Strings, Optional. For csv inputs only, and
#'   only required if the csv file names are *not* the default file names used
#'   by RDBES when downloading data (for instance if you created them yourself).
#'   The names should be a `list` of the two-letter code for the relevant table
#'   e.g. `list("DE" = "DE.csv", "SD" = "SD.csv", etc.)`.  If not supplied then
#'   it is assumed the files have the default file names used by the RDBES data
#'   download ("Design.csv" etc).
#' @param castToCorrectDataTypes Logical. If `TRUE` then the function will
#'   attempt to cast the required columns to the correct data type.  If `FALSE`
#'   then the column data types will be determined by how the csv files are read
#'   in. Default is `TRUE`
#'
#' @return A RDBESDataObject
#' @export
#' @md
#'
#' @examples
#' myEmptyRDBESObject <- importRDBESDataCSV(input = NULL)

createRDBESDataObject <- function(input = NULL,
                                  listOfFileNames = NA,
                                  castToCorrectDataTypes = TRUE) {

  # x <- c("1.zip", "2.zip")
  # x <- c("tests/testthat/h5_v_1_19_18/2023_10_16_112208.zip")
  # x <- c("tests/testthat/h5_v_1_19_18")
  # x <- c("1.csv", "2.zip")
  # x <- c("Something else")

  #  x <- "tests/testthat/h1_v_1_19_18/2023_10_16_104555.zip"

  # if input is string and zip file
  if(any(is.character(input)) && any(grepl(".zip", input))) {
    if(!(all(grepl(".zip", input)))) stop("You cannot import a mix of 'csv' and 'zip' inputs. To import multiple tables unzip all files and import as a folder of 'csv' files.")
    import.type <- "zip"
    # if input is string and folder assume it contains only csv files
  } else if(length(input) == 1 && is.character(input) && file_test("-d", input)) {
    import.type <- "csv"
    # if input is string and folder assume it contains only csv files
  } else if(is.list(input)) {
    import.type <- "list.of.dfs"
  } else if(is.null(input)) {
    import.type <- "null"
  } else {
    stop("Input type not recognised. Should be a RDBES zip file, folder of csv files, or list of data frames.")
  }


  # -------------------------------------------------------------------------

  if(import.type == "zip") output <- importRDBESDataZIP(filenames = input,
                                                        castToCorrectDataTypes = castToCorrectDataTypes)

  if(import.type == "folder") output <- importRDBESDataCSV(filenames = input,
                                                           listOfFileNames = listOfFileNames,
                                                           castToCorrectDataTypes = castToCorrectDataTypes)


  if(import.type == "list.of.dfs") {
    warning("NOTE: creating RDBES objects from list of data frames bypasses the ICES data checks. Make sure you know what you are doing.")
    importRDBESDataDFS()
  }

  if(import.type == "null") {
    warning("NOTE: 'NULL' input specified. Creating an EMPTY RDBES Data Object")
    output <- newRDBESDataObject()
  }

  # check for duplicate tables?

  return(output)
}
