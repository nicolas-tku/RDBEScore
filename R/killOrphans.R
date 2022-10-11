#' Internal function to remove orphan records from an RDBESDataObject
#'
#' @param objectToCheck an RDBESDataObject
#' @param orphansToRemove The output from the findOrphansByTable function (A
#' data frame with the primary keys of the table checked, the two
#' letter table identifier, and their orphan status.)
#'
#' @return RDBESDataObject with orphan records removed
#'
killOrphans <- function(objectToCheck, orphansToRemove) {
  for (myTable in unique(orphansToRemove$Table)) {
    rowsToRemove <- orphansToRemove[orphansToRemove$Table == myTable, "pk"]
    rowsToRemove <- rowsToRemove[[1]]
    if (length(rowsToRemove) > 0) {
      objectToAlter <- objectToCheck[[myTable]]
      # Just keep the rows that aren't in the list to remove
      objectToAlter <-
        objectToAlter[!objectToAlter[[1]] %in% rowsToRemove, ]
      objectToCheck[[myTable]] <- objectToAlter
    }
  }

  objectToCheck
}
