#' Remove rows which are not pointing to a valid VesselDetails (VD) records i.e.
#' those rows which have a value of VDid that does not exist in the VD table.
#'
#' @param objectToCheck an RDBESRawObject.
#' @param verbose (Optional) If set to TRUE more detailed text will be printed
#' out by the function.  Default is TRUE.
#'
#' @return an RDBESRawObject with any records with an invalid VDid removed
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#' createRDBESRawObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#' myFields <- c("VDlenCat")
#' myValues <- c("18-<24" )
#' myFilteredObject <- filterRDBESRawObject(myH1RawObject,
#'                                         fieldsToFilter = myFields,
#'                                         valuesToFilter = myValues )
#' myObjectValidVesselLinks <- removeBrokenVesselLinks(
#'                                  objectToCheck = myFilteredObject,
#'                                  verbose = FALSE)
#' }
removeBrokenVesselLinks <- function(objectToCheck, verbose = TRUE){

  # for testing - to be removed
  #myH1RawObject <-
  #  createRDBESRawObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
  #verbose <- TRUE
  #myFields <- c("VDlenCat")
  #myValues <- c("18-<24" )
  #objectToCheck <- filterRDBESRawObject(myH1RawObject,
  #                                         fieldsToFilter = myFields,
  #                                         valuesToFilter = myValues )

  # Get all the VDid fields
  myIds <- icesRDBES::mapColNamesFieldR[
    grepl("^VDid$", icesRDBES::mapColNamesFieldR$R.Name),
    c("Table.Prefix", "R.Name")]

  # Don't need to check the VD table - VDid is a primary key there
  myIds <- myIds[!myIds$Table.Prefix == "VD",]

  # Just check the non-null entries
  nonNullEntries <- names(objectToCheck[sapply(objectToCheck, Negate(is.null))])
  vDtables <- unique(myIds$Table.Prefix)
  nonNullTablesToCheck <- vDtables[vDtables %in%  nonNullEntries]

  print(paste0("Number of rows in relevent non-null tables before removing ",
    "broken vessel links"))
  print(unlist(lapply(objectToCheck[vDtables], nrow)))

  # Check any non-null tables
  for(tableToCheck in nonNullTablesToCheck){

    myTable <- objectToCheck[[tableToCheck]]

    # if we have the possibility of orphans - let's check for them
    if (nrow(myTable) > 0){

      if (verbose) {
        print(paste0("Checking for broken vessel links in table ",
                     tableToCheck))
      }

      # Create a data frame to hold the results
      myOrphanResults <- data.table::data.table(myTable[, 1])
      names(myOrphanResults) <- c("pk")
      myOrphanResults$Table <- tableToCheck
      # Default to link not existing
      myOrphanResults[, "vdExists"] <- FALSE

      # Inner join to the VD table
      joinedTables <- dplyr::inner_join(myTable,
                                        objectToCheck[["VD"]],
                                        by = "VDid")

      # Update the results for any matches we found
      if (nrow(joinedTables) > 0) {

        # Find which PKs are in the joined table - these are matches
        myOrphanResults[
          myOrphanResults[[1]] %in% joinedTables[[1]],
          "vdExists"
        ] <- TRUE
      }

      # We want to remove any rows where the record did not exist in VD
      rowsToRemove <- myOrphanResults[!myOrphanResults$vdExists,1]
      rowsToRemove <- rowsToRemove[[1]]

      if (verbose) {
        print(paste0("Found ", length(rowsToRemove),
                     " records with broken vessel links"))
      }

      # Just keep the rows that aren't in the list to remove
      objectToAlter <- myTable
      objectToAlter <-
        objectToAlter[!objectToAlter[[1]] %in% rowsToRemove, ]

      objectToCheck[[tableToCheck]] <- objectToAlter

      if (verbose) {
        print(paste0("Records with broken vessel links have been removed"))
      }


    }
  }

  print(paste0("Number of rows in relevent non-null tables after removing ",
  "broken vessel links"))
  print(unlist(lapply(objectToCheck[vDtables], nrow)))

  objectToCheck

}
