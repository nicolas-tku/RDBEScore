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

  # Copy the inpput data table so we don't accidentally change the orignal data
  rdbesPrepObjectCopy <- data.table::copy(rdbesPrepObject)

  # Which tables do we expect to find in the data?
  targetTables <- icesRDBES::tablesInRDBESHierarchies[[hierarchyToUse]]

  # Break out the lower hierachies - they need to join to different tables
  lowerA <- rdbesPrepObjectCopy[["SA"]][
    (rdbesPrepObjectCopy[["SA"]][, "SAlowHierarchy"] == "A")[, 1],
  ]
  lowerB <- rdbesPrepObjectCopy[["SA"]][
    (rdbesPrepObjectCopy[["SA"]][, "SAlowHierarchy"] == "B")[, 1],
  ]
  lowerC <- rdbesPrepObjectCopy[["SA"]][
    (rdbesPrepObjectCopy[["SA"]][, "SAlowHierarchy"] == "C")[, 1],
  ]
  lowerD <- rdbesPrepObjectCopy[["SA"]][
    (rdbesPrepObjectCopy[["SA"]][, "SAlowHierarchy"] == "D")[, 1],
  ]

  # Join FM and BV - we do it in both ways left join on FM and right join on
  # BV.  this is because not all of the lower hierachies have FM data

  fMBV <-
    dplyr::left_join(rdbesPrepObjectCopy[["FM"]],
      rdbesPrepObjectCopy[["BV"]],
      by = "FMid"
    )
  # sort out the wrong SAid column name after the join
  names(fMBV)[names(fMBV) == "SAid.x"] <- "SAid"
  fMBV[, "SAid.y" := NULL]

  # Now get rid of any unneccessary id fields to avoid trouble with field naming
  allowedIds <- c("SAid", "FMid", "BVid")
  unallowedIDs <-
    paste0(names(rdbesPrepObjectCopy), "id")[
      !(paste0(names(rdbesPrepObjectCopy), "id") %in% allowedIds)
    ]
  unallowedIDs <-
    names(fMBV)[names(fMBV) %in% unallowedIDs]
  if (length(unallowedIDs) > 0) fMBV[, (unallowedIDs) := NULL]


  bVFM <- dplyr::right_join(rdbesPrepObjectCopy[["FM"]],
    rdbesPrepObjectCopy[["BV"]],
    by = "FMid"
  )
  # sort out the wrong SAid column name after the join
  names(bVFM)[names(bVFM) == "SAid.y"] <- "SAid"
  bVFM[, "SAid.x" := NULL]

  # Now get rid of any unneccessary id fields to avoid trouble with field naming
  allowedIds <- c("SAid", "FMid", "BVid")
  unallowedIDs <-
    paste0(names(rdbesPrepObjectCopy), "id")[
      !(paste0(names(rdbesPrepObjectCopy), "id") %in% allowedIds)
    ]
  unallowedIDs <-
    names(bVFM)[names(bVFM) %in% unallowedIDs]
  if (length(unallowedIDs) > 0) bVFM[, (unallowedIDs) := NULL]

  lowerA <- dplyr::left_join(lowerA, fMBV, by = "SAid")
  lowerB <- dplyr::left_join(lowerB, fMBV, by = "SAid")
  # Note the difference in lowerC
  lowerC <- dplyr::left_join(lowerC, bVFM, by = "SAid")
  lowerD <- dplyr::left_join(lowerD, fMBV, by = "SAid")

  # Ok, this shoudl be all the SA, FM, and BV data for all lower hierarchies
  allLower <- rbind(lowerA, lowerB, lowerC, lowerD)

  myRDBESEstObj <- NA

  for (i in 1:(length(targetTables) - 1)) {
    if (i > 1) {
      joinField <- paste0(targetTables[i - 1], "id")
    } else {
      joinField <- NA
    }
    thisTable <- targetTables[i]

    breakLoop <- FALSE

    if (thisTable == "SA") {
      myRDBESEstObj <-
        dplyr::left_join(myRDBESEstObj, allLower, by = joinField)
      breakLoop <- TRUE
      # break
    } else if (i == 1) {
      myRDBESEstObj <- rdbesPrepObjectCopy[[thisTable]]
    } else if (i >= 2) {

      # First get rid of any unneccessary id fields to avoid trouble
      # with field naming
      allowedIds <- c(paste0(thisTable, "id"), joinField, "VDid", "SLid")
      unallowedIDs <-
        paste0(names(rdbesPrepObjectCopy), "id")[
          !(paste0(names(rdbesPrepObjectCopy), "id") %in% allowedIds)
        ]
      unallowedIDs <-
        names(rdbesPrepObjectCopy[[thisTable]])[
          names(rdbesPrepObjectCopy[[thisTable]]) %in% unallowedIDs
        ]
      if (length(unallowedIDs) > 0) {
        rdbesPrepObjectCopy[[thisTable]][, (unallowedIDs) := NULL]
      }

      myRDBESEstObj <-
        dplyr::left_join(myRDBESEstObj,
          rdbesPrepObjectCopy[[thisTable]],
          by = joinField
        )
    }

    # Need to do this other we get a warning message from set()
    data.table::setDT(myRDBESEstObj)

    # Add a column with the previous table ID so that we can find it easily
    if (i == 1) {
      data.table::set(myRDBESEstObj,
        j = (paste0(thisTable, "prevTableID")), value = NA
      )
    } else {
      data.table::set(myRDBESEstObj,
        j = (paste0(thisTable, "prevTableID")),
        value = myRDBESEstObj[, joinField, with = FALSE]
      )
    }

    if (thisTable != "SD") {
      # Add a column with the stratum name concatenated with the parent ID -
      # strata need to be considered in respect of their parent record
      data.table::set(myRDBESEstObj,
        j = (paste0(thisTable, "stratumNameFull")),
        value = paste0(
          myRDBESEstObj[, get(paste0(thisTable, "prevTableID"))],
          ":",
          myRDBESEstObj[, get(paste0(thisTable, "stratumName"))]
        )
      )
    }

    if (breakLoop) break
  }


  names(myRDBESEstObj)

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
    if (!is.na(vdIDFieldToKeep)) {
      names(myRDBESEstObj)[names(myRDBESEstObj) == vdIDFieldToKeep] <- "VDid"
    }
  }

  # Set the class of the object
  class(myRDBESEstObj) <- c("RDBESEstObject", class(myRDBESEstObj))

  myRDBESEstObj
}
