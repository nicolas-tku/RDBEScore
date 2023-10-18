#' Filter and an RDBESDataObject and remove any orphan records in an
#' RDBESDataObject
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
#'   createRDBESDataObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19_13")
#'
#' myFields <- c("VSencrVessCode", "VDlenCat")
#' myValues <- c("VDcode_1","VL1518","VL2440")
#'
#' myFilteredObject <- filterAndTidyRDBESDataObject(myH1RawObject,
#'   fieldsToFilter = myFields,
#'   valuesToFilter = myValues,
#'    killOrphans = TRUE, verboseBrokenVesselLinks = TRUE
#' )
#' }
#'
#'
filterAndTidyRDBESDataObject <- function(RDBESDataObjectToFilterAndTidy,
                                 fieldsToFilter,
                                 valuesToFilter,
                                 killOrphans = FALSE,
                                 verboseOrphans = FALSE,
                                 verboseBrokenVesselLinks = FALSE)
  {

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(RDBESDataObjectToFilterAndTidy, verbose = FALSE)

  # If now fields/values to filter, then the same object is returned; else filtering
  if (!(missing(fieldsToFilter) | missing(valuesToFilter)))
    RDBESDataObjectToFilterAndTidy <- filterRDBESDataObject(RDBESDataObjectToFilterAndTidy,
                                                      fieldsToFilter,
                                                      valuesToFilter)



  # Remove orphans after filtering and removing VD-invalid data
  if (killOrphans == TRUE)
    RDBESDataObjectToFilterAndTidy <- findAndKillOrphans(RDBESDataObjectToFilterAndTidy, verbose = verboseOrphans)

  # note to myself
  if (any(grepl("VD", fieldsToFilter)))
    print(paste0("VD filtered by: ", fieldsToFilter[which(grepl("VD", fieldsToFilter))]))
  RDBESDataObjectToFilterAndTidy <- removeBrokenVesselLinks(RDBESDataObjectToFilterAndTidy, verbose = verboseBrokenVesselLinks)

  return(RDBESDataObjectToFilterAndTidy)

}
