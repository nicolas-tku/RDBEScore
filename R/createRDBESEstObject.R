#' Creates an rdbesEStObject from prepared RDBES data
#'
#' @param rdbesPrepObject The prepared RDBES object that should be used to
#' create an estimation object
#' @param hierarchyToUse The upper RDBES hiearchy to use
#'
#' @return An object of class RDBESEstObject ready for us in design based
#' estimation
#' @export
#'
#' @examples
#' \dontrun{
#' myH1RawObject <-
#'   createRDBESRawObject(rdbesExtractPath = "tests/testthat/h1_v_1_19")
#' myH1EstObj <- createRDBESEstObject(myH1RawObject, 1)
#' }
createRDBESEstObject <- function(rdbesPrepObject, hierarchyToUse) {
  warning("Function is a work in progress and not fully tested yet")


  # For testing
  #myH1RawObject <-
  #  createRDBESRawObject(rdbesExtractPath = "tests/testthat/h1_v_1_19")
  #rdbesPrepObject <- myH1RawObject
  #hierarchyToUse <- 1



  if (!validateRDBESRawObject(rdbesPrepObject, verbose = FALSE)) {
    stop(paste0(
      "rdbesPrepObject is not valid ",
      "- createRDBESEstObject will not proceed"
    ))
  }

  if (!hierarchyToUse %in% 1:13) {
    stop(paste0(
      "An invalid value was used for the 'hierarchyToUse' paramter",
      "- createRDBESEstObject will not proceed"
    ))
  }

  if (any(!is.na(rdbesPrepObject[["SA"]]$parentID))) {
    stop("Sub-sampling present in SA: not yet developed")
  }

  # Copy the input data table so we don't accidentally change the orignal data
  rdbesPrepObjectCopy <- data.table::copy(rdbesPrepObject)

  # Combine the lower hierachy tables (FM,BV)
  allLower <- processRDBESEstObjLowerHierarchies(rdbesPrepObjectCopy)

  # Combine the upper hierachy tables
  upperHierarchy <- processRDBESEstObjUpperHierarchies(
                                        rdbesPrepObject = rdbesPrepObjectCopy,
                                        hierarchyToUse = hierarchyToUse)

  # Join the upper and lower hierarchy tables together
  myRDBESEstObj <- dplyr::left_join(upperHierarchy,allLower,by="SAid")

  # Choose which VDid field to keep
  vdIDFields <- names(myRDBESEstObj)[grepl("^VDid", names(myRDBESEstObj))]
  vdIDFieldToKeep <- NA
  if (length(vdIDFields) > 1) {
    # read through all the VDid fields and note the name of the first
    # one that has some data - this is the one we will name "VDid"
    for (myVDid in vdIDFields) {
      if (is.na(vdIDFieldToKeep) && any(!is.na(myRDBESEstObj[[myVDid]]))) {
        vdIDFieldToKeep <- myVDid
        break
      }
    }
    # Rename the VDid field we want to keep
    if (!is.na(vdIDFieldToKeep)) {
      names(myRDBESEstObj)[names(myRDBESEstObj) == vdIDFieldToKeep] <- "VDid"
      vdIDFieldsToRemove <- vdIDFields[!(vdIDFields == vdIDFieldToKeep)]
    } else {
      vdIDFieldsToRemove <- vdIDFields
    }
    # Get rid of the other VDid fields
    myRDBESEstObj[, (vdIDFieldsToRemove) := NULL]
  }

  # Set the class of the object
  class(myRDBESEstObj) <- c("RDBESEstObject", class(myRDBESEstObj))

  names(myRDBESEstObj)
  myRDBESEstObj
}

#' Private function to process the lower hierarchies when creating
#' the RDBESEstObject
#'
#' @param rdbesPrepObject
#'
#' @return allLower - the FM and BV tables combined
#'
#' @examples
processRDBESEstObjLowerHierarchies <- function(rdbesPrepObject){

  # Break out the lower hierachies - they need to join to different tables
  lowerA <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "A")[, 1],"SAid"
  ]
  lowerB <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "B")[, 1],"SAid"
  ]
  lowerC <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "C")[, 1],"SAid"
  ]
  lowerD <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "D")[, 1],"SAid"
  ]

  # Join FM and BV - we do it in both ways left join on FM and right join on
  # BV.  This is because not all of the lower hierachies have FM data

  fMBV <-
    dplyr::left_join(rdbesPrepObject[["FM"]],
                     rdbesPrepObject[["BV"]],
                     by = "FMid"
    )
  # sort out the wrong SAid column name after the join
  names(fMBV)[names(fMBV) == "SAid.x"] <- "SAid"
  fMBV[, "SAid.y" := NULL]

  # Now get rid of any unneccessary id fields to avoid trouble with field naming
  allowedIds <- c("SAid", "FMid", "BVid")
  unallowedIDs <-
    paste0(names(rdbesPrepObject), "id")[
      !(paste0(names(rdbesPrepObject), "id") %in% allowedIds)
    ]
  unallowedIDs <-
    names(fMBV)[names(fMBV) %in% unallowedIDs]
  if (length(unallowedIDs) > 0) fMBV[, (unallowedIDs) := NULL]


  bVFM <- dplyr::right_join(rdbesPrepObject[["FM"]],
                            rdbesPrepObject[["BV"]],
                            by = "FMid"
  )
  # sort out the wrong SAid column name after the join
  names(bVFM)[names(bVFM) == "SAid.y"] <- "SAid"
  bVFM[, "SAid.x" := NULL]

  # Now get rid of any unneccessary id fields to avoid trouble with field naming
  allowedIds <- c("SAid", "FMid", "BVid")
  unallowedIDs <-
    paste0(names(rdbesPrepObject), "id")[
      !(paste0(names(rdbesPrepObject), "id") %in% allowedIds)
    ]
  unallowedIDs <-
    names(bVFM)[names(bVFM) %in% unallowedIDs]
  if (length(unallowedIDs) > 0) bVFM[, (unallowedIDs) := NULL]

  lowerA <- dplyr::left_join(lowerA, fMBV, by = "SAid")
  lowerB <- dplyr::left_join(lowerB, fMBV, by = "SAid")
  # Note the difference in lowerC
  lowerC <- dplyr::left_join(lowerC, bVFM, by = "SAid")
  lowerD <- dplyr::left_join(lowerD, fMBV, by = "SAid")

  # Ok, this shoudl be all the FM, and BV data for all lower hierarchies
  allLower <- rbind(lowerA, lowerB, lowerC, lowerD)

  allLower

}

#' Private function to process the upper hierarchies when creating
#' the RDBESEstObject
#'
#' @param myRDBESEstObj
#' @param rdbesPrepObject
#' @param hierarchyToUse
#' @param i
#'
#' @return
#'
#' @examples
processRDBESEstObjUpperHierarchies <- function(myRDBESEstObj = NULL,
                                               rdbesPrepObject,
                                               hierarchyToUse,
                                               i = 1){

  # Which tables do we expect to find in the data?
  targetTables <- icesRDBES::tablesInRDBESHierarchies[[hierarchyToUse]]
  thisTable <- targetTables[i]
  print(thisTable)

  if (thisTable %in% c("FM","BV")){
    # if we've got to FM or BV we're done so lets stop
    return (myRDBESEstObj)
  } else {

    # if we don't have an Est object yet we must be at the start - let's go!
    if(is.null(myRDBESEstObj)){
      myRDBESEstObj <- rdbesPrepObject[[thisTable]]

    } else {
      # If we already have an Est object let's join the new data to it

      # get the sampling unit level
      suLevel <- paste0("su",i-2)

      if (i > 1) {
        joinField <- paste0(targetTables[i - 1], "id")
      } else {
        joinField <- NA
      }

      # First get rid of any unneccessary id fields to avoid trouble
      # with field naming
      allowedIds <- c(paste0(thisTable, "id"), joinField, "VDid", "SLid")
      unallowedIDs <-
        paste0(names(rdbesPrepObject), "id")[
          !(paste0(names(rdbesPrepObject), "id") %in% allowedIds)
        ]
      unallowedIDs <-
        names(rdbesPrepObject[[thisTable]])[
          names(rdbesPrepObject[[thisTable]]) %in% unallowedIDs
        ]
      if (length(unallowedIDs) > 0) {
        rdbesPrepObject[[thisTable]][, (unallowedIDs) := NULL]
      }

      # Field names of the design variables
      designVariables <- c("stratification","stratumName","clustering",
                           "clusterName","sampler","numTotal","numSamp","selProb",
                           "incProb","selectMeth","unitName", "selectMethCluster",
                           "numTotalClusters","numSampClusters","selProbCluster",
                           "incProbCluster","samp", "noSampReason")

      # if this table has design variable columns
      if (any(names(rdbesPrepObject[[thisTable]])
              %in% paste0(thisTable,designVariables))){

        #Rename design variables using su ("sampling unit") style names
        # (we'll just ignore any of the "old" names that aren't present)
        data.table::setnames(rdbesPrepObject[[thisTable]],
                             old = paste0(thisTable,designVariables),
                             new = paste0(suLevel,designVariables),
                             skip_absent = TRUE)


        # Add a column showing which table this sampling unit is from
        suTable <- paste0(suLevel,"table")
        rdbesPrepObject[[thisTable]][,suTable] <- thisTable
      }

      # Join this new table to the existing data
      myRDBESEstObj <-
        dplyr::left_join(myRDBESEstObj,
                         rdbesPrepObject[[thisTable]],
                         by = joinField)

    }

    # recursively call this function
    processRDBESEstObjUpperHierarchies(myRDBESEstObj,
                                       rdbesPrepObject,
                                       hierarchyToUse,
                                       i+1)
  }
}
