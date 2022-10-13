#' Filter an RDBESDataObject
#'
#' The returned object will include all rows which either: a) do not included
#' any of the field names in `fieldsToFilter`, or b) do include the field names
#' and have one of the allowed values in `valuesToFilter`.
#'
#' `killOrphans` allows you to remove orphaned rows if set to `TRUE`. The
#' default is `FALSE`.
#'
#' @param RDBESDataObjectToFilter The `RDBESDataObject` to filter
#' @param fieldsToFilter A vector of the field names you wish to check
#' @param valuesToFilter A vector of the field values you wish to filter for
#' @param killOrphans Controls if orphan rows are removed. Default is `FALSE`.
#'
#' @return the filtered input object of the same class as
#'   `RDBESDataObjectToFilter`
#'
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'   createRDBESDataObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#'
#' myFields <- c("SDctry", "VDctry", "VDflgCtry", "FTarvLoc")
#' myValues <- c("ZW", "ZWBZH", "ZWVFA")
#'
#' myFilteredObject <- filterRDBESDataObject(myH1RawObject,
#'   fieldsToFilter = myFields,
#'   valuesToFilter = myValues
#' )
#' }
filterRDBESDataObject <- function(RDBESDataObjectToFilter,
                                 fieldsToFilter,
                                 valuesToFilter,
                                 killOrphans = FALSE) {

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(RDBESDataObjectToFilter, verbose = FALSE)

  # Check if the requested columns actually exist in the object
  allColNames <- unlist(lapply(RDBESDataObjectToFilter, names))
  missingFields <- fieldsToFilter[!fieldsToFilter %in% allColNames]
  if (length(missingFields) > 0) {
    warning(paste0(
      "The following fields were not found in the ",
      "RDBESDataObject: ",
      missingFields
    ))
  }

  alteredObject <- lapply(RDBESDataObjectToFilter, function(x) {
    foundNames <- names(x)[which(names(x) %in% fieldsToFilter)]
    if (length(foundNames) > 0) {
      x <-
        dplyr::filter(x, dplyr::if_all(all_of(foundNames), ~ .x %in% valuesToFilter))
    }
    x
  })

  # Update the original object so we don't lose its class type
  for (myTable in names(RDBESDataObjectToFilter)) {
    if (!is.null(alteredObject[[myTable]])) {
      RDBESDataObjectToFilter[[myTable]] <- alteredObject[[myTable]]
    }
  }

  # remove orphans
  if (killOrphans == TRUE) RDBESDataObjectToFilter <- findAndKillOrphans(RDBESDataObjectToFilter, verbose = FALSE)

  #
  return(RDBESDataObjectToFilter)
}
