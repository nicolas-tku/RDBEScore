#' Filter an RDBESRawObject
#'
#' The returned object will include all rows which either: a) do not included
#' any of the field names in fieldsToFilter, or b) do include the field names
#' and have one of the allowed values in valuesToFilter `killOrphans` allows you
#' to remove orphaned rows if set to `TRUE`. Change to `FALSE` to reverse this
#' behaviour.
#'
#' @param rdbesRawObjectToFilter The RDBESRawObject to filter
#' @param fieldsToFilter A vector of the field names you wish to check
#' @param valuesToFilter A vector of the field values you wish to filter for
#' @param killOrphans Controls if orphan rows are removed. Default is `TRUE`.
#'
#' @return the filtered input object of the same *class* as
#'   `rdbesRawObjectToFilter`
#'
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'   createRDBESRawObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#'
#' myFields <- c("SDctry", "VDctry", "VDflgCtry", "FTarvLoc")
#' myValues <- c("ZW", "ZWBZH", "ZWVFA")
#'
#' myFilteredObject <- filterRDBESRawObject(myH1RawObject,
#'   fieldsToFilter = myFields,
#'   valuesToFilter = myValues
#' )
#' }
filterRDBESRawObject <- function(rdbesRawObjectToFilter,
                                 fieldsToFilter,
                                 valuesToFilter,
                                 killOrphans = TRUE) {

  # Check we have a valid RDBESRawObject before doing anything else
  if (!validateRDBESRawObject(rdbesRawObjectToFilter, verbose = FALSE)) {
    stop(paste0(
      "rdbesRawObjectToFilter is not valid ",
      "- filterRDBESRawObject will not proceed"
    ))
  }

  # Check if the requested columns actually exist in the object
  allColNames <- unlist(lapply(rdbesRawObjectToFilter, names))
  missingFields <- fieldsToFilter[!fieldsToFilter %in% allColNames]
  if (length(missingFields) > 0) {
    warning(paste0(
      "The following fields were not found in the ",
      "RDBESRawObject: ",
      missingFields
    ))
  }

  alteredObject <- lapply(rdbesRawObjectToFilter, function(x) {
    foundNames <- names(x)[which(names(x) %in% fieldsToFilter)]
    if (length(foundNames) > 0) {
      x <-
        dplyr::filter(x, dplyr::if_all(foundNames, ~ .x %in% valuesToFilter))
    }
    x
  })

  # Update the original object so we don't lose its class type
  for (myTable in names(rdbesRawObjectToFilter)) {
    if (!is.null(alteredObject[[myTable]])) {
      rdbesRawObjectToFilter[[myTable]] <- alteredObject[[myTable]]
    }
  }

  # remove orphans
  if (killOrphans == TRUE) rdbesRawObjectToFilter <- findAndKillOrphans(rdbesRawObjectToFilter, verbose = FALSE)

  #
  return(rdbesRawObjectToFilter)
}
