#' Create an RDBES Data Object
#'
#' Create a RDBES Data object from either:
#'  - a `zip` file downloaded from RDBES (or multiple zip files if you want to include CL and CE data)
#'  - a folder containing `csv` files downloaded from RDBES (e.g. the unzipped archive file)
#'  - a `list` of data frames in the current environment representing different tables in the hierarchy
#'
#' @param input The path to the zip file extracted from RDBES (or multiple zip
#'   files), or path to a folder of `csv` files, or a list object in the current
#'   environment containing data frames of each table.
#' @param listOfFileNames (Optional) For csv input only. A named list of file names - the list names
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
    if(!(all(grepl(".zip", input)))) stop("Cannot mix csv and zip inputs")
    import.type <- "zip"
    # if input is string and folder assume it contains only csv files
  } else if(length(input) == 1 && is.character(input) && file_test("-d", input)) {
    import.type <- "csv"
    # if input is string and folder assume it contains only csv files
  } else if(is.list(input)) {
    import.type <- "list.of.dfs"
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

    # check for duplicate tables

    return(output)
}
