#summariseSelectionMethods <- function(){
#
#}


myRDBESData <- createRDBESRawObject("H:\\git\\WK_RDBES\\WKRDB-EST2\\subGroup6\\icesRDBES\\tests\\testthat\\h1_v_1_19")


# For testing
objectToSummarise <- myRDBESData
yearToUse <- 1965
country <- 'ZW'
hierarchyToCheck <- 'H1'

library(icesRDBES)
library(dplyr)

#warning("Please be aware that this function does not
#          consider the clustered sampling variables yet")

# Check we have a valid RDBESRawObject before doing anything else
if (!validateRDBESRawObject(objectToSummarise, verbose = FALSE)) {
  stop(paste0(
    "rdbesRawObjectToFilter is not valid ",
    "- filterRDBESRawObject will not proceed"
  ))
}

requiredTables <- icesRDBES::tablesInRDBESHierarchies

# Find which tables we need for this file type
upperHierarchy <- substr(hierarchyToCheck, 2, nchar(hierarchyToCheck))
myRequiredTables <- requiredTables[[hierarchyToCheck]]

## Step 1 - Filter the data
# Filter by year, country, and hieracrchy at the same time - safe because we know
# the fields won't have the same values
myCSData <- filterRDBESRawObject(objectToSummarise,
                                         fieldsToFilter = c("DEhierarchy","DEyear","SDctry"),
                                         valuesToFilter = c(yearToUse,country,upperHierarchy) )


icesRDBES::validateRDBESRawObject(myCSData,verbose = FALSE)

# If myCSData is empty at this point just stop
#if (length(myCSData)==0){
#  print("Error - the data to summarise is empty")
#  stop("Could not summarise data -
#         please validate your data to identify any problems")
#}

View(myCSData[['SD']])
View(objectToSummarise[['SD']])

# Lists to hold the outputs
myData <- list()
myDataGroupedStrataMethod <- list()
myDataGroupedMethod <- list()
myJoinedData <- NULL
previousRequiredTable <- NULL

# STEP 2 Process each table to get the data in the format we need
# We won't worry about SS,SA,FM,BV at this point
for (aRequiredTable in myRequiredTables[!myRequiredTables %in% c('SS','SA','FM','BV')]){

  #aRequiredTable <- 'DE'

  # Get the data we will work with
  myTempData <- myCSData[[aRequiredTable]]

  # Process the different tables
  #
  # We need particular fields for DE
  if (aRequiredTable == 'DE'){
    # Create a stratum name for DE
    myTempData$DEfullStratumName <- paste(myTempData[,'DEsampScheme'][[1]],myTempData[,'DEyear'][[1]],myTempData[,'DEstratumName'][[1]],sep = '_')

    #Save the data to our list (DE doesn't need to be grouped)
    myData[[aRequiredTable]] <- myTempData[,c('DEid','DEstratumName', 'DEfullStratumName','DEsampScheme', 'DEyear', 'DEhierarchyCor', 'DEhierarchy')]
    myDataGroupedStrataMethod[[aRequiredTable]] <- myData[[aRequiredTable]][,c('DEstratumName', 'DEfullStratumName','DEsampScheme', 'DEyear', 'DEhierarchyCor', 'DEhierarchy')]
    #myDataGroupedMethod[[aRequiredTable]] <- myData[[aRequiredTable]][,c('DEyear', 'DEhierarchyCorrect', 'DEhierarchy')]
    myDataGroupedMethod[[aRequiredTable]] <- myDataGroupedStrataMethod[[aRequiredTable]] %>%
      dplyr::group_by(DEyear, DEhierarchyCor, DEhierarchy) %>%
      dplyr::summarise(DEstratumName = paste(unique(DEstratumName), collapse = ","))

    # We need particualr fields for SD
  } else if (aRequiredTable == 'SD'){

    #aRequiredTable <- 'SD'
    #previousRequiredTable <- 'DE'

    # Join SD to the previous table (DE in this case)
    previousHierarchyTable <- myData[[previousRequiredTable]]
    ## Assume the primary key is the first field
    previousPrimaryKey <- names(previousHierarchyTable)[1]
    currentPrimaryKey <- names(myTempData)[1]
    myTempData <- inner_join(myTempData,previousHierarchyTable, by =previousPrimaryKey)

    # Rename the stratum from DE as parent stratum name
    names(myTempData)[names(myTempData) == 'DEfullStratumName'] <- 'SDparentFullStratumName'

    # Create a stratum name and full stratum names for SD
    myTempData$SDstratumName <- paste(myTempData[,'SDctry'][[1]],myTempData[,'SDinst'][[1]],sep = '_')
    myTempData$SDfullStratumName <- paste(myTempData[,'SDparentFullStratumName'][[1]],myTempData[,'SDstratumName'][[1]],sep = ':')

    #Save the data to our list
    myData[[aRequiredTable]] <- myTempData[,c(currentPrimaryKey,previousPrimaryKey, 'SDparentFullStratumName','SDstratumName','SDfullStratumName', 'SDctry', 'SDinst'), with = FALSE]
    myDataGroupedStrataMethod[[aRequiredTable]] <- myData[[aRequiredTable]][,c('SDparentFullStratumName','SDstratumName','SDfullStratumName', 'SDctry', 'SDinst')]
    # Group SD by country and institute (1 row for each country with institute concatenated)
    myDataGroupedMethod[[aRequiredTable]] <- myDataGroupedStrataMethod[[aRequiredTable]] %>%
      dplyr::group_by(SDctry) %>%
      dplyr::summarise(SDinst = paste(unique(SDinst), collapse = ","))

  } else {

    #aRequiredTable <- 'VS'
    #previousRequiredTable <- 'SD'

    # Get the names of the fields we are interested in
    currentStratificationField <- paste(aRequiredTable,'stratification',sep ="")
    currentStratumNameField <- paste(aRequiredTable,'stratumName',sep ="")
    currentNumberSampledField <- paste(aRequiredTable,'numSamp',sep ="")
    currentNumberTotalField <- paste(aRequiredTable,'numTotal',sep ="")
    currentSelectionMethodField <- paste(aRequiredTable,'selectMeth',sep ="")
    currentSampledField <- paste(aRequiredTable,'samp',sep ="")
    currentFullStratumNameField <- paste(aRequiredTable,'fullStratumName',sep="")
    currentParentFullStratumName <- paste(aRequiredTable,'parentFullStratumName',sep="")

    # Join to the previous table
    previousHierarchyTable <- myData[[previousRequiredTable]]
    ## Assume the primary key is the first field
    previousPrimaryKey <- names(previousHierarchyTable)[1]
    currentPrimaryKey <- names(myTempData)[1]
    myTempData <- inner_join(myTempData,previousHierarchyTable, by =previousPrimaryKey)

    # Rename the fullStratumName from the previous table as parentFullStratumName
    names(myTempData)[names(myTempData) == paste(previousRequiredTable,'fullStratumName',sep="")] <- currentParentFullStratumName

    # Need to modify the stratum name values for unstratified data to include the parent xxID value (otherwise all 'U' records will look liek they are are from the same stratum)
    unStratifiedRecords <- as.vector(myTempData[,..currentStratificationField] == 'N')
    newStratumNames1 <- myTempData[unStratifiedRecords,..currentStratumNameField][[1]]
    newStratumNames2 <- myTempData[unStratifiedRecords,..previousPrimaryKey][[1]]
    newStratumNames <- paste(newStratumNames1,"(",newStratumNames2,")",sep="")
    myTempData[unStratifiedRecords,currentStratumNameField] <-
      newStratumNames

    #myTempData[myTempData[,..currentStratificationField] == 'N',currentStratumNameField] <- paste(myTempData[myTempData[,..currentStratificationField] == 'N',currentStratumNameField],"(",myTempData[myTempData[,..currentStratificationField] == 'N',previousPrimaryKey],")",sep="")

    # Create the full stratum name field
    myTempData[,currentFullStratumNameField] <- paste(myTempData[,..currentParentFullStratumName][[1]],myTempData[,..currentStratumNameField][[1]],sep = ':')

    #myTempData[,currentFullStratumNameField] <- paste(myTempData[,currentParentFullStratumName],myTempData[,currentStratumNameField],sep = ':')

    #Save the data to our list
    myData[[aRequiredTable]] <- myTempData[,c(currentPrimaryKey,previousPrimaryKey, currentParentFullStratumName,currentStratumNameField,currentFullStratumNameField, currentStratificationField,currentNumberSampledField,currentNumberTotalField,currentSelectionMethodField, currentSampledField ),with=FALSE]

    # Now we will group and summarise the data

    # Get the data we want to group
    myTempToGroup <- myData[[aRequiredTable]][,
                                              c(currentParentFullStratumName,
                                                currentStratumNameField,
                                                currentFullStratumNameField,
                                                currentStratificationField,
                                                currentSelectionMethodField,
                                                currentSampledField,
                                                currentNumberSampledField,
                                                currentNumberTotalField),
                                              with = FALSE]

    # CHanges names so we can hard-code them below
    # - dplyr can't seem to cope with variables - grrr
    names(myTempToGroup) <-
      substr(names(myTempToGroup), 3, nchar(names(myTempToGroup)))

    # Add fields to hold the calcualted number of things
    # we did and didn't sample
    myTempToGroup$numberNotSampledCalc <- NA
    myTempToGroup$numberSampledCalc <- NA

    myTempToGroup[myTempToGroup$samp == 'N' &
                    !is.na(myTempToGroup$samp),'numberNotSampledCalc'] <- 1
    myTempToGroup[myTempToGroup$samp == 'Y' &
                    !is.na(myTempToGroup$samp),'numberSampledCalc'] <- 1


    # Group 1) - Group by strata and selection method and summarise
    myDataGroupedStrataMethod[[aRequiredTable]] <- myTempToGroup %>%
      dplyr::group_by(parentFullStratumName,
                      stratumName,
                      fullStratumName,
                      stratification,
                      selectMeth) %>%
      dplyr::summarise(numberOfRows = n(),
                       numberNotSampledCalc = sum(!is.na(numberNotSampledCalc)),
                       numberSampledCalc = sum(!is.na(numberSampledCalc)))

    names(myDataGroupedStrataMethod[[aRequiredTable]]) <- paste(aRequiredTable,names(myDataGroupedStrataMethod[[aRequiredTable]]),sep='')

    # Group 2) - Group by selection method and summarise
    myDataGroupedMethod[[aRequiredTable]] <- myTempToGroup %>%
      dplyr::group_by(selectMeth) %>%
      dplyr::summarise(numberOfRows = n(),
                       numberNotSampledCalc = sum(!is.na(numberNotSampledCalc)),
                       numberSampledCalc = sum(!is.na(numberSampledCalc)))

    names(myDataGroupedMethod[[aRequiredTable]]) <- paste(aRequiredTable,names(myDataGroupedMethod[[aRequiredTable]]),sep='')


  }

  previousRequiredTable <- aRequiredTable

}

myTotalSummary <- NULL
myJoinedData <- NULL
previousRequiredTable <- NULL

# Now join our grouped data together
for (aRequiredTable in myRequiredTables[!myRequiredTables %in% c('SS','SA','FM','BV')]){

  if (aRequiredTable == 'DE'){
    # 1) Grouped by stratum and method
    myJoinedData <- myDataGroupedStrataMethod[['DE']]
    # 2) Grouped by method
    myTotalSummary <- myDataGroupedMethod[['DE']]
  } else {
    # 1) Grouped by stratum and method
    previousTableJoinField <- paste(previousRequiredTable,'fullStratumName',sep="")
    currentTableJoinField <- paste(aRequiredTable,'parentFullStratumName',sep="")
    myJoinedData <- dplyr::inner_join(myJoinedData,myDataGroupedStrataMethod[[aRequiredTable]],by = setNames(currentTableJoinField, previousTableJoinField))

    # 2) Grouped by method

    # We'll merge the myDataGroupedMethod data frames together on row numbers
    # - this will add NA values where there are more rows in some of the
    # data frames

    # Add column to each data frame with row number
    myDataGroupedMethod[[aRequiredTable]]$number <- row.names(myDataGroupedMethod[[aRequiredTable]])
    myTotalSummary$number <- row.names(myTotalSummary)

    # Merge data frames
    #"all = TRUE" will mean that NA values will be added whenever there is no match
    myTotalSummary <- merge(myTotalSummary, myDataGroupedMethod[[aRequiredTable]], by = "number", all = TRUE)

    # Now get rid of the superfluous row number column
    myTotalSummary$number <- NULL

  }

  previousRequiredTable <- aRequiredTable

}


# Return values
list(detailedData = myData,
     summaryDataByStrataAndMethod = myDataGroupedStrataMethod,
     summaryDataJoined = myJoinedData,
     overallSummary = myTotalSummary
)


}
