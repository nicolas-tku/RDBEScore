#' Combine Two RDBES Raw Objects
#' combines 2 RDBESDataObjects into a single RDBESDataObject by merging individual
#' tables one by one
#'
#' @param RDBESDataObject1 The first object to combine
#' @param RDBESDataObject2 The second object to combine
#'
#' @return the combination of \code{RDBESDataObject1} and  \code{RDBESDataObject2}
#' @seealso  \link[data.table]{rbindlist}
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'     createRDBESDataObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#' myH5RawObject <-
#'     createRDBESDataObject(rdbesExtractPath = "tests\\testthat\\h5_v_1_19")
#'
#' myCombinedRawObject <- combineRDBESDataObjects(RDBESDataObject1=myH1RawObject,
#'                                              RDBESDataObject2=myH5RawObject)
#' }
combineRDBESDataObjects <- function(RDBESDataObject1, RDBESDataObject2) {
  validateRDBESDataObject(RDBESDataObject1, verbose = FALSE)
  validateRDBESDataObject(RDBESDataObject2, verbose = FALSE)
  # Create an empty RDBESDataObject as the basis of what we will return
  myRDBESDataObject <- createRDBESDataObject()

  # For each entry, bind the data tables together
  for (myTable in names(myRDBESDataObject)) {

    # Only bind the data tables if one of them is not null
    if (!(is.null(RDBESDataObject1[[myTable]]) &
      is.null(RDBESDataObject2[[myTable]]))) {
      myRDBESDataObject[[myTable]] <-
        data.table::rbindlist(list(
          RDBESDataObject1[[myTable]],
          RDBESDataObject2[[myTable]]
        ),
        use.names = T, fill = T
        )

      # De-duplicate the resulting SL,VD, CL, and CE tables
      if (myTable %in% c('VD','SL','CL','CE')){
        myRDBESDataObject[[myTable]] <-
          dplyr::distinct(myRDBESDataObject[[myTable]], .keep_all = TRUE)
      }

    }
  }

  myRDBESDataObject
}
