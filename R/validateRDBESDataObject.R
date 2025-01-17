#' Check Whether an `RDBESDataObject` is in a Valid Format
#'
#' Perform basic checks on a object.
#'
#' Checks if 'objectToCheck' parameter is valid. Returns the parameter if it is
#' valid and otherwise stops on error.
#' It checks the `RDBESDataObject` if:
#' * Is this an object of class RDBESDataObject
#' * Tables don't have column names that aren't allowed
#' * Tables have all the required  column names
#'
#' It does not check if the data is valid.
#' The RDBES upload system performs an extensive set of checks on the
#' uploaded data.
#'
#' @param objectToCheck RDBESDataObject i.e. a list of data.tables
#' @param checkDataTypes (Optional) Set to TRUE if you want to check that
#' the data types of the required columns are correct, or FALSE if you don't
#' care. Default value is FALSE.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#'
#' @return Returns objectToCheck
#' @md
#' @export
#' @aliases checkRDBESDataObject
#' @examples
#' \dontrun{
#' myH1RawObject <-
#' createRDBESDataObject(rdbesExtractPath = "tests/testthat/h1_v_1_19")
#' validateRDBESDataObject(myH1RawObject)}
#'
validateRDBESDataObject <- function(objectToCheck,
                                checkDataTypes = FALSE,
                                verbose = FALSE) {
  #allowedNamesInList <- unique(RDBEScore::mapColNamesFieldR$Table.Prefix)
  allowedNamesInList <- unique(mapColNamesFieldR$Table.Prefix)

  # CHECK 1 Have we just been passed NA?
  if (length(is.na(objectToCheck)) == 1) {
    if (is.na(objectToCheck)) {
      stop("objectToCheck is NA")
    }
  # CHECK 2 Is this an object of class RDBESDataObject?  It should be!
  } else if (! 'RDBESDataObject' %in% class(objectToCheck)) {
    stop("objectToCheck is not of the class RDBESDataObject")
  # CHECK 3 Is this a list?  It should be!
  } else if (!(is.list(objectToCheck) & inherits(objectToCheck, "list"))) {
    stop("objectToCheck does not inherit from list")
    # CHECK 4 Does this list have any names that aren't allowed?
  } else if (!all(names(objectToCheck) %in% allowedNamesInList)) {
    stop(paste("objectToCheck is a list but has extra names ",
      paste(names(objectToCheck), collapse = ","),
      sep = ""
    ))
    # CHECK 5 Does the list have an entry for all the required names?
  } else if (!all(allowedNamesInList %in% names(objectToCheck))) {
    print(paste(names(objectToCheck), collapse = ","))
    stop(paste("objectToCheck is a list but does not contain ",
      "all the required names ",
      paste(names(objectToCheck), collapse = ","),
      sep = ""
    ))
  } else { #1

    # Get any objectToCheck entries which aren't null or data tables
    badEntries <- objectToCheck[!
    sapply(
      objectToCheck,
      function(x) {
        returnValue <- FALSE
        if (length(x) == 0 & is.null(x)) {
          returnValue <- TRUE
        } else if ("data.table" %in% class(x)) {
          returnValue <- TRUE
        }
        returnValue
      }
      )]
    # CHECK 6 Are there any entries which aren't NULL or data tables?
    if (length(badEntries) > 0) {
      stop(paste("objectToCheck is a list but contains some entries which are ",
        "not NULL or data tables",
        paste(names(badEntries), collapse = ","),
        sep = ""
      ))
    } else { #2

      # Print out null entries for information
      nullEntries <- objectToCheck[sapply(objectToCheck, is.null)]
      emptyTables <- unlist(sapply(objectToCheck,
                                          function(x){nrow(x) == 0}))
      if (length(nullEntries)>0){
        if (verbose){
          print(paste("Note that ",names(nullEntries)
                     ," is NULL but this is allowed in an RDBESDataObject"
                  , sep = ""))
        }
      }

      if (any(emptyTables)){
        if (verbose){
          print(paste("Note that ",names(emptyTables[emptyTables])
                      ," has 0 rows but this is allowed in an RDBESDataObject"
                      , sep = ""))
        }
      }

      # Just check non-NULL entries
      nonNullEntries <- objectToCheck[sapply(objectToCheck, Negate(is.null))]

      # The next checks are only relevent if we don't have an empty object
      if (length(nonNullEntries) > 0) { #3
        #TODO: implement content, type checks using stop and fix this mess

        # Call a function to check whether the required field names
        # are present and that there aren't duplicates
        myReturnValue <- validateRDBESDataObjectContent(nonNullEntries)
        warningText <- myReturnValue[["warningText"]]
        validRDBESDataObject <- myReturnValue[["validRDBESDataObject"]]

        # If we also want to check the data types of the columns
        # then go ahead and call the function to do that
        if (checkDataTypes){
          myDiffs <- validateRDBESDataObjectDataTypes(nonNullEntries)
          numberOfDifferences <- nrow(myDiffs)
          if (numberOfDifferences >0 ){
            validRDBESDataObject <- FALSE
            if(is.na(warningText)){
              warningText <- ""
            } else {
              warningText <- paste0(warningText,". ")
            }
            warningText <- paste0(warningText,
              "objectToCheck has the following fields ",
              "with incorrect data types: ",
              paste(myDiffs[,"R.Name"], collapse = ","),
              sep = ""
              )
          }
        }
        if(!validRDBESDataObject) {
          stop(warningText)
        }
      } #3
    } #2
  } #1

  # Return the validation result
  return(invisible(objectToCheck));
}

#' @rdname validateRDBESDataObject
#' @export
checkRDBESDataObject <- validateRDBESDataObject
