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
createRDBESEstObject <- function(rdbesPrepObject,
                                 hierarchyToUse,
                                 stopTable = NULL,
                                 verbose = FALSE) {


  if (!validateRDBESRawObject(rdbesPrepObject, verbose = FALSE)) {
    stop(paste0(
      "rdbesPrepObject is not valid ",
      "- createRDBESEstObject will not proceed"
    ))
  }

  if (!hierarchyToUse %in% 1:13) {
    stop(paste0(
      "An invalid value was used for the 'hierarchyToUse' parameter",
      " - createRDBESEstObject will not proceed"
    ))
  }

  # Copy the input data table so we don't change the original data
  rdbesPrepObjectCopy <- data.table::copy(rdbesPrepObject)



  # See if the user has specified a table to stop at
  targetTables <-
    icesRDBES::tablesInRDBESHierarchies[[paste0("H", hierarchyToUse)]]
  if (length(is.null(stopTable)) == 1 &&
    !is.null(stopTable)) {
    stopTableLoc <- which(targetTables == stopTable)
    if (length(stopTableLoc) > 0) {
      targetTables <- targetTables[1:stopTableLoc]
    }
  }

  # See if we need to process the lower hweriarchy tables
  if (any(targetTables %in% c("FM", "BV"))) {
    processLowerHierarchy <- TRUE
  } else {
    processLowerHierarchy <- FALSE
  }

  # Handle any sub-sampling
  # Check if we have any SA data
  if (length(is.null(rdbesPrepObjectCopy[["SA"]])) == 1 &&
    is.null(rdbesPrepObjectCopy[["SA"]])) {
    if (verbose) {
      print("No SA data found - can't check for sub-sampling")
    }
  } else {

    # Deal with the sub-sampling
    if (verbose) {
      print("Checking for sub-sampling")
    }

    subSampleLevels <- lapply(rdbesPrepObjectCopy[["SA"]][, SAid],
      getSubSampleLevel,
      SAdata = rdbesPrepObjectCopy[["SA"]]
    )
    subSampleLevels <- do.call(rbind, subSampleLevels)
    rdbesPrepObjectCopy[["SA"]][, "SAtopLevelSAid"] <-
      subSampleLevels$topLevelSAid
    rdbesPrepObjectCopy[["SA"]][, "SAsubSampleLevel"] <-
      subSampleLevels$subSampleLevel
    numberOfSampleLevels <-
      max(rdbesPrepObjectCopy[["SA"]][, "SAsubSampleLevel"])

    if (verbose) {
      print(paste0("Max levels of sampling: ",numberOfSampleLevels))
    }

    if (numberOfSampleLevels > 1) {
      # Create new entries for each level of sampling
      for (i in 1:numberOfSampleLevels) {
        saNameNew <- paste0("SA", i)
        rdbesPrepObjectCopy[[saNameNew]] <-
          rdbesPrepObjectCopy[["SA"]][
            rdbesPrepObjectCopy[["SA"]]$SAsubSampleLevel == i,
          ]
        rdbesPrepObjectCopy[[saNameNew]]$SArecType <-
          paste0(rdbesPrepObjectCopy[[saNameNew]]$SArecType, i)
        # Rename the columns to SA2 etc (don't bother for SA1 because we
        # are going to chaneg its name to SA in a moment)
        if (i > 1) {
          names(rdbesPrepObjectCopy[[saNameNew]]) <-
            gsub("^SA", saNameNew, names(rdbesPrepObjectCopy[[saNameNew]]))
        }
      }
      # Get rid of the overall SA entry
      rdbesPrepObjectCopy[["SA"]] <- NULL
      # Rename SA1 to SA for consistency
      names(rdbesPrepObjectCopy)[names(rdbesPrepObjectCopy) == "SA1"] <- "SA"
    }

    # Replace "SA" in the target tables with "SA", "SA2" etc to handle
    # sub-sampling (if required)
    SAloc <- which(targetTables == "SA")
    if (length(SAloc) > 0 &&
        length(grep("^SA.+$", names(rdbesPrepObjectCopy))) > 0) {
      targetTables <- append(targetTables,
                             names(rdbesPrepObjectCopy)[
                               grep("^SA.+$", names(rdbesPrepObjectCopy))
                               ],
                             after = SAloc
      )
    }
  }

  if (verbose){
    print("Processing upper hierarachy data")
  }

  # Combine the upper hierachy tables
  upperHierarchy <- procRDBESEstObjUppHier(
    rdbesPrepObject = rdbesPrepObjectCopy,
    hierarchyToUse = hierarchyToUse,
    targetTables = targetTables,
    verbose = verbose
  )

  # Combine the lower hierachy tables (FM,BV)
  if (processLowerHierarchy) {
    if (verbose){
      print("Processing lower hierarachy data")
    }
    allLower <-
      procRDBESEstObjLowHier(rdbesPrepObjectCopy, verbose = verbose)
  } else {
    if (verbose) {
      print("Not processing lower hierarachy data")
    }
  }

  # Join the upper and lower hierarchy tables together
  # Check if we have any lower hierarchy data first, and whether we actually
  # need to process the lower hiaerarcy - if not just return the upper results
  if (length(is.null(rdbesPrepObjectCopy[["SA"]])) == 1 &&
    is.null(rdbesPrepObjectCopy[["SA"]])) {
    if (verbose) {
      print(paste0("No sample data found - ",
            "won't try to combine upper and lower hierarchy data"))
    }
    myRDBESEstObj <- upperHierarchy
  } else if (!processLowerHierarchy) {
    myRDBESEstObj <- upperHierarchy
  } else {
    if (verbose) {
      print("Combining upper and lower hierarachy data")
    }
    # TODO - check how we join the upper and lower hierachy data when there
    # is sub-sampling e.g. are we joining the right SAxID value?
    myRDBESEstObj <- dplyr::left_join(upperHierarchy, allLower, by = "SAid")
  }


  # Choose which VDid field to keep - to avoid confusion
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


  # If myRDBESEstObj is null let's create an empty data table to return
  if (length(is.null(myRDBESEstObj)) == 1 &&
    is.null(myRDBESEstObj)) {
    if (verbose) {
      print("Returning an empty RDBESEstObject")
    }
    myRDBESEstObj <- data.table()
  }

  # Set the class of the object
  class(myRDBESEstObj) <- c("RDBESEstObject", class(myRDBESEstObj))


  myRDBESEstObj
}

#' Private function to process the lower hierarchies when creating
#' the RDBESEstObject
#'
#' @param rdbesPrepObject A prepared RDBESRawObj
#'
#' @return allLower - the FM and BV tables combined
#'
procRDBESEstObjLowHier <- function(rdbesPrepObject,
                                   verbose = FALSE) {


  # Check if we have any SA data - if not we'll just stop now
  if (length(is.null(rdbesPrepObject[["SA"]])) == 1 &&
    is.null(rdbesPrepObject[["SA"]])) {
    if (verbose) {
      print("No SA data found - can't process the lower hierarchies")
    }
    return(NULL)
  }

  # Break out the lower hierachies - they need to join to different tables
  lowerA <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "A")[, 1], "SAid"
  ]
  lowerB <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "B")[, 1], "SAid"
  ]
  lowerC <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "C")[, 1], "SAid"
  ]
  lowerD <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "D")[, 1], "SAid"
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
#' @param myRDBESEstObj An RDBESEstObj to add data to
#' @param rdbesPrepObject A prepared RDBESRawObj
#' @param hierarchyToUse The hierarchy we are using
#' @param targetTables The RDBES tables we are interested in
#' @param i Integer to keep track of where we are in the list of tables
#'
#' @return
#'
procRDBESEstObjUppHier <- function(myRDBESEstObj = NULL,
                                   rdbesPrepObject,
                                   hierarchyToUse,
                                   i = 1,
                                   targetTables,
                                   verbose = FALSE) {

  thisTable <- targetTables[i]

  if (thisTable %in% c("FM", "BV") || (i > length(targetTables))) {
    # if we've got to FM or BV, or we've reached the end of the target tables
    # we're done so lets stop
    return(myRDBESEstObj)
  } else {

    if (verbose) {
      print(paste0("Processing ", thisTable))
    }

    # if we don't have an Est object yet we must be at the start - let's go!
    if (is.null(myRDBESEstObj)) {
      myRDBESEstObj <- rdbesPrepObject[[thisTable]][
        rdbesPrepObject[[thisTable]]$DEhierarchy == hierarchyToUse,]
    } else {
      # If we already have an Est object let's join the new data to it

      # get the sampling unit level
      suLevel <- paste0("su", i - 2)

      grep("^SA.+$", thisTable)
      grepl("^SA.+$", "SA2")

      if (i > 1) {
        joinField <- paste0(targetTables[i - 1], "id")
      } else {
        joinField <- NA
      }

      # First get rid of any unneccessary id fields to avoid trouble
      # with field naming
      allowedIds <- c(
        paste0(thisTable, "id"),
        joinField,
        "VDid",
        "SLid",
        paste0(thisTable, "parentID")
      )
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

      # If we're dealing with sub-sampling rename SAxparentID to the join field
      # - shoudl mean we can easily join with the data from the level above
      if (grepl("^SA.+$", thisTable)) {
        names(rdbesPrepObject[[thisTable]])[
          names(rdbesPrepObject[[thisTable]]) == paste0(thisTable, "parentID")
        ] <- joinField
      }

      # Field names of the design variables
      designVariables <- icesRDBES::designVariables

      # if this table has design variable columns
      if (any(names(rdbesPrepObject[[thisTable]])
      %in% paste0(thisTable, designVariables))) {

        # Rename design variables using su ("sampling unit") style names
        # (we'll just ignore any of the "old" names that aren't present)
        data.table::setnames(rdbesPrepObject[[thisTable]],
          old = paste0(thisTable, designVariables),
          new = paste0(suLevel, designVariables),
          skip_absent = TRUE
        )


        # Add a column showing which table this sampling unit is from
        suTable <- paste0(suLevel, "table")
        rdbesPrepObject[[thisTable]][, suTable] <- thisTable
      }

      # Join this new table to the existing data
      myRDBESEstObj <-
        dplyr::left_join(myRDBESEstObj,
          rdbesPrepObject[[thisTable]],
          by = joinField
        )
    }

    # recursively call this function
    procRDBESEstObjUppHier(myRDBESEstObj,
      rdbesPrepObject = rdbesPrepObject,
      hierarchyToUse = hierarchyToUse,
      i = i + 1,
      targetTables = targetTables,
      verbose = verbose
    )
  }
}


#' Private function to get sub-sample level and top-level SAid for SA data
#'
#' @param SAdata The SA data to check
#' @param SAidToCheck The SAid to check
#' @param subSampleLevel The currrent level of sampling
#'
#' @return
#'
getSubSampleLevel <- function(SAdata, SAidToCheck, subSampleLevel = 1) {

  # Fix in case the parentID is not numeric - this can be the case if it was
  # empty when it was imported
  if (!is.numeric(SAdata$SAparentID)) {
    SAdata$SAparentID <- as.numeric(SAdata$SAparentID)
  }

  dataToCheck <- SAdata[SAdata$SAid == SAidToCheck, ]

  # If we have mutiple matches we probably don't have unique SAid values
  if (nrow(dataToCheck) > 1) {
    warning("There is a problem with non-unique SAid values- check your data")
    # Just use the first match
    dataToCheck <- dataToCheck[1, ]
  }

  if (nrow(dataToCheck) == 0) {
    return(NA)
  } else if (is.na(dataToCheck$SAparentID)) {
    return(data.frame(
      "topLevelSAid" = SAidToCheck,
      "subSampleLevel" = subSampleLevel
    ))
  } else {
    return(getSubSampleLevel(
      SAdata = SAdata,
      SAidToCheck = dataToCheck$SAparentID,
      subSampleLevel = subSampleLevel + 1
    ))
  }
}
