#' Estimate totals and means, and try to generate samples variances for all
#' strata in an RDBESDataObject
#'
#' @param RDBESDataObjectForEstim The RDBESDataObject to generate estimates for
#' @param hierarchyToUse The number of the RDBES hierarchy to estimate for
#' @param verbose (Optional) If set to TRUE more detailed text will be printed
#' out by the function.  Default is TRUE.
#'
#' @return A data frame containing estimates for all strata
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'   createRDBESDataObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#'
#' # Update our test data with some random sample measurements
#' myH1RawObject[["SA"]]$SAsampWtLive <-
#'   round(runif(n = nrow(myH1RawObject[["SA"]]), min = 1, max = 100))
#'
#' myStrataEst <- doEstimationForAllStrata(
#'   RDBESDataObjectForEstim = myH1RawObject,
#'   hierarchyToUse = 1
#' )
#' }
doEstimationForAllStrata <- function(RDBESDataObjectForEstim,
                                     hierarchyToUse,
                                     verbose = TRUE) {

  # For testing
  # RDBESDataObjectForEstim <- myFilteredTestData
  # hierarchyToUse <- 1
  # verbose <- TRUE

  # TODO - function does not handle sub-sampling at the moment

  # Check we have a valid RDBESDataObject before doing anything else
  if (!validateRDBESDataObject(RDBESDataObjectForEstim, verbose = FALSE)) {
    stop(paste0(
      "RDBESDataObjectForEstim is not valid ",
      "- filterRDBESDataObject will not proceed"
    ))
  }

  # Clear out the variable that will hold our results
  myStrataResults <- NULL

  # Find what tables we need for this hierarchy
  tablesToCheck <-
    icesRDBES::getTablesInRDBESHierarchy(hierarchyToUse)
  # Loop through our tables, starting at SA and working backwards
  saPosition <- match("SA", tablesToCheck)
  for (i in saPosition:1) {
    #i <- 6
    currentTable <- tablesToCheck[i]
    parentTable <- NA
    if (i > 1) {
      parentTable <- tablesToCheck[i - 1]
    }
    if (verbose) {
      print(paste0("Processing ", currentTable))
    }

    # Work out what fields we need
    if (currentTable == "SD") {
      varsNeeded <- paste0(currentTable, c("id", "recType", "ctry", "inst"))
      varsNeeded <- c(varsNeeded, paste0(parentTable, "id"))
    } else if (currentTable == "DE") {
      varsNeeded <- paste0(currentTable, c("id", "recType", "sampScheme",
                                    "year", "stratumName", "hierarchy", "samp"))
    } else {
      designVars <- c(
        "id", "recType", "stratification", "stratumName",
        "selectMeth", "numTotal", "numSamp", "selProb", "incProb",
        "samp"
      )
      varsNeeded <- paste0(currentTable, designVars)
      varsNeeded <- c(varsNeeded, paste0(parentTable, "id"))
      if (currentTable == "SA") {
        varsNeeded <- c(varsNeeded, "SAsampWtLive")
      }
    }

    # Get our data
    myTable <- RDBESDataObjectForEstim[[currentTable]][, ..varsNeeded]
    # Get the parent table ID
    names(myTable)[names(myTable) == paste0(parentTable, "id")] <-
      paste0(currentTable, "parentTableID")
    # Remove the two letter prefix from column names so they will
    # be consistent between tables
    names(myTable) <- substring(names(myTable), 3)
    myTable$parentTable <- parentTable

    # We'll fill in the values for parent stratum as we go through the tables
    myTable$parentTableStratum <- NA

    # Make some changes for specific tables
    if (currentTable == "SA") {
      # Rename the variable we want to estimate
      names(myTable)[names(myTable) == "sampWtLive"] <- "studyVariable"
    }
    if (currentTable == "DE") {
      # Combine the year with the stratum name
      myTable$stratumName <- paste0(myTable$year, "-", myTable$stratumName)
    }
    if (currentTable == "SD") {
      # Create a pseudo-stratum name with the country and institute code
      myTable$stratumName <- paste0(myTable$ctry, "-", myTable$inst)
    }

    # Create a new field combining parent ID and stratum names - this ensures
    # that records with the same stratum name but different parent records
    # are easily distinguished
    myTable$parentIDandStratum <-
      paste0(myTable$parentTableID, ":", myTable$stratumName)


    # See we if already have some results - if we don't have any results yet we
    # can't join to previous values
    if (length(is.null(myStrataResults)) == 1 && is.null(myStrataResults)) {
      myTableWithValues <- myTable

      # if we do have previous results lets join our table with them
    } else {

      # Get the results from the previous table - we'll use these as inputs
      # for the estimation
      myPrevEstimates <-
        myStrataResults[myStrataResults$parentTable == currentTable, ] %>%
        dplyr::group_by(parentTableID) %>%
        dplyr::summarise(studyVariable = sum(est.total, na.rm = TRUE))



      # Join our current table with the previous resuults
      myTableWithValues <-
        dplyr::left_join(myTable,
          myPrevEstimates,
          by = c("id" = "parentTableID")
        )

      # Append the parent table stratum to our previous results (so we can
      # easily join the parent and child records of our final results)
      myStrataResults[myStrataResults$parentTable == currentTable, ]$
        parentTableStratum <-
         dplyr::left_join(
           myStrataResults[myStrataResults$parentTable == currentTable, ],
           myTable[,c("id","parentIDandStratum")],
           by = c("parentTableID"="id"))$"parentIDandStratum.y"


    }

    ## DE/SD/SS need to be handled differently because we can't estimate with
    # these tables
    if (currentTable %in% c("DE", "SD", "SS")) {

      # Add on a parentTableID column if it doesn't exist
      if (!"parentTableID" %in% names(myTableWithValues)) {
        myTableWithValues$parentTableID <- NA
      }
      # Add on a stratumName column if it doesn't exist
      if (!"stratumName" %in% names(myTableWithValues)) {
        myTableWithValues$stratumName <- NA
      }

      # Can't estimate with these tables - just sum up the values
      myTableWithValuesGrouped <- myTableWithValues %>%
        dplyr::group_by(
          recType,
          parentTable,
          parentTableID,
          parentTableStratum,
          stratumName,
          parentIDandStratum,
        ) %>%
        dplyr::summarise(studyVariable = sum(studyVariable, na.rm = TRUE))

      myResultsTemp <- data.frame(
        "recType" = myTableWithValuesGrouped$recType,
        "parentTable" = myTableWithValuesGrouped$parentTable,
        "parentTableID" = myTableWithValuesGrouped$parentTableID,
        "parentTableStratum" = myTableWithValuesGrouped$parentTableStratum,
        "stratumName" = myTableWithValuesGrouped$stratumName,
        "parentIDandStratum" = myTableWithValuesGrouped$parentIDandStratum,
        "est.results.available" = FALSE,
        "est.total" = myTableWithValuesGrouped$studyVariable,
        "est.mean" = NA,
        "var.total" = NA,
        "var.mean" = NA,
        "sd.total" = NA,
        "sd.mean" = NA,
        "se.total" = NA,
        "se.mean" = NA
      )
    } else {

      # For the other tables we'll run the estimation function

      # Split by parent ID and stratum name
      myTableList <- split(myTableWithValues, f = myTable$parentIDandStratum)
      # apply estimate function to each unique parent ID and stratum name
      # combination
      myResults <- lapply(myTableList, getEstimForStratum)
      # Combine our results into a data frame
      myResultsTemp <- do.call(rbind, myResults)
    }

    # Get rid of row names
    rownames(myResultsTemp) <- NULL
    # Combine the results from this loop with all the previous results
    myStrataResults <- rbind(myStrataResults, myResultsTemp)
  }

  myStrataResults
}


#' Private function used by doEstimationForAllStrata to get the estimates
#'
#' @param x The input
#'
#' @return Whoever revises this function please specify what it returns here
#'
getEstimForStratum <- function(x) {
  myReturnValues <- data.frame(
    "recType" = unique(x$recType),
    "parentTable" = unique(x$parentTable),
    "parentTableID" = unique(x$parentTableID),
    "parentTableStratum" = unique(x$parentTableStratum),
    "stratumName" = unique(x$stratumName),
    "parentIDandStratum" = unique(x$parentIDandStratum)
  )
  myEstim <- NA
  try(
    myEstim <- estimMC(
      x$studyVariable,
      x$numSamp,
      x$numTotal,
      unique(x$selectMeth),
      x$selProb,
      x$incProb
    )
  )
  if (length(is.na(myEstim)) == 1 && is.na(myEstim)) {
    myReturnValues$est.results.available <- FALSE
    myReturnValues$est.total <- NA
    myReturnValues$est.mean <- NA
    myReturnValues$var.total <- NA
    myReturnValues$var.mean <- NA
  } else {
    myReturnValues$est.results.available <- TRUE
    myReturnValues$est.total <- myEstim$est.total
    myReturnValues$est.mean <- myEstim$est.mean
    myReturnValues$var.total <- myEstim$var.total
    myReturnValues$var.mean <- myEstim$var.mean

  }

  # Calculate the standard deviation and standard error from the variance
  numberOfSamples <- unique(x$numSamp)

  if (is.numeric(myEstim$var.total) && !is.nan(myEstim$var.total) && !is.na(myEstim$var.total) && myEstim$var.total >0 ){
    #myReturnValues$sd.total <- sqrt(myEstim$var.total)
    myReturnValues$sd.total <- NA
    myReturnValues$se.total <- sqrt(myEstim$var.total)
    # if (length(numberOfSamples) == 1 && numberOfSamples >0){
    #   myReturnValues$se.total <- sqrt(myEstim$var.total)/sqrt(numberOfSamples)
    # } else {
    #   myReturnValues$se.total <- NA
    # }
  } else {
    myReturnValues$sd.total <- NA
    myReturnValues$se.total <- NA
  }


  if (is.numeric(myEstim$var.mean) && !is.nan(myEstim$var.mean) && !is.na(myEstim$var.mean) && myEstim$var.mean >0){
    #myReturnValues$sd.mean <- sqrt(myEstim$var.mean)
    myReturnValues$sd.mean <- NA
    myReturnValues$se.mean <- sqrt(myEstim$var.mean)
    # if (length(numberOfSamples) == 1 && numberOfSamples >0){
    #   myReturnValues$se.mean <- sqrt(myEstim$var.mean)/sqrt(numberOfSamples)
    # } else {
    #   myReturnValues$se.mean <- NA
    # }
  } else {
    myReturnValues$sd.mean <- NA
    myReturnValues$se.mean <- NA
  }

  myReturnValues
}
