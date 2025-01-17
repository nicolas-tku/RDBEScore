#' Import RDBES Downloaded Data
#'
#' Read the .zip files and/or .csv files downloadable from the ICES RDBES
#' web page. The function accepts a list of paths to csv and zip files. Unzips
#' an then uses \code{\link{createRDBESDataObject}}
#'
#' @param filenames - vector of paths pointing to files that should be imported
#' @param castToCorrectDataTypes (Optional) If TRUE then the function
#' will attempt to cast the required columns to the correct data type.  If
#' FALSE then the column data types will be determined by how the csv files
#' are read in.  The default is TRUE.
#'
#' @return a list of all the RDBES data tables
#' The table that are not in input data are NULL
#' @export
#'
#' @seealso \code{\link{createRDBESDataObject}}
#'
#' @examples
#' files <- c("./tests/testthat/h1_v_1_19/H1_2021_000_example.zip")
#' obj <- importRDBESDownloadData(files)
importRDBESDownloadData <- function(filenames,
                                    castToCorrectDataTypes = TRUE) {
  randInt <- paste0(sample(1:100, 3), collapse = "")
  tmp <- paste0(tempdir(), "/downloadImport", randInt)
  dir.create(tmp)
  all_unzipped <- c()
  unzipFile <- function(x, tmp) {
    if (!file.exists(x)) {
      return()
    }
    if (is.zip(x)) {
      unzipped <- utils::unzip(x, exdir= tmp)
      unzipped <- basename(unzipped)
      unzipped <- unzipped[grep("*.csv", unzipped)]
      intersected <- intersect(unzipped, all_unzipped)
      if(length(intersected) != 0) {
        warning(paste0("Duplicate unzipped files detected in ", x,":\n", paste0("\t", unzipped, collapse="\n")))
      }
      all_unzipped <<- c(all_unzipped, unzipped)
      return(unzipped)
    }
    if (fileExt(x) == "csv") {
      newName <- paste0(tmp, "/", basename(x))
      if (file.exists(newName)) {
        warning(paste0(
          "Overwriting file: ", basename(x),
          ", this might be intended!\n"
        ), call. = FALSE)
      }
      file.copy(x, tmp)
      return(newName)
    }
  }

  fileExt <- function(x) {
    ext <- strsplit(x, ".", fixed = TRUE)[[1]]
    ext[length(ext)]
  }

  is.zip <- function(x) {
    # let's assume that the file extension is .zip to be on the safe side
    ext <- fileExt(x)
    # the zip starts with a local file header signature 50 4b 03 04
    # see: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
    b <- readBin(x, "raw", 4)
    if (setequal(as.character(b), c("50", "4b", "03", "04")) &
      ext == "zip") {
      return(TRUE)
    }
    FALSE
  }

  # the files are not used currently but can be if we want to
  files <- unique(unlist(sapply(filenames, unzipFile, tmp)))
  res <- RDBEScore::createRDBESDataObject(tmp,
                              castToCorrectDataTypes = castToCorrectDataTypes)
  unlink(tmp, recursive = T)
  res
}
