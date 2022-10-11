library(icesRDBES)
#library(dplyr)
# Suppress summarise info
#options(dplyr.summarise.inform = FALSE)

# Load the test data
testData <- createRDBESDataObject("\\\\galwayfs03\\fishdata\\FishnCo_Project\\Outsourcing\\Obj5\\BiasReport\\FISHNCO_Function\\testData")
# show the non-null table names
names(testData[!unlist(lapply(testData, is.null))])
# validate the data
validateRDBESDataObject(testData, verbose = FALSE)


# make some changs to our test data so that we can estimate
testData[['SA']]$SAselectMeth = 'SRSWOR'
testData[['SS']]$SSselectMeth = 'SRSWOR'
testData[['FO']]$FOselectMeth = 'SRSWOR'
testData[['FT']]$FTselectMeth = 'SRSWOR'
testData[['VS']]$VSselectMeth = 'SRSWOR'

# Get rid of SS rows without linked SA rows
temp <- dplyr::left_join(testData[['SS']],testData[['SA']][,c("SAid","SSid")],by="SSid")
unsampledSS <- temp[is.na(temp$SAid),"SSid"]
testData[['SS']] <- testData[['SS']][!testData[['SS']]$SSid %in% unsampledSS$SSid,]

# Get rid of FO rows without linked SS rows
temp <- dplyr::left_join(testData[['FO']],testData[['SS']][,c("SSid","FOid")],by="FOid")
unsampledFO <- temp[is.na(temp$SSid),"FOid"]
testData[['FO']] <- testData[['FO']][!testData[['FO']]$FOid %in% unsampledFO$FOid,]

# Get rid of FT rows without linked FO rows
temp <- dplyr::left_join(testData[['FT']],testData[['FO']][,c("FOid","FTid")],by="FTid")
unsampledFT <- temp[is.na(temp$FOid),"FTid"]
testData[['FT']] <- testData[['FT']][!testData[['FT']]$FTid %in% unsampledFT$FTid,]

# Get rid of VS rows without linked FT rows
temp <- dplyr::left_join(testData[['VS']],testData[['FT']][,c("FTid","VSid")],by="VSid")
unsampledVS <- temp[is.na(temp$FTid),"VSid"]
testData[['VS']] <- testData[['VS']][!testData[['VS']]$VSid %in% unsampledVS$VSid,]

# Get rid of SD rows without linked VS rows
temp <- dplyr::left_join(testData[['SD']],testData[['VS']][,c("VSid","SDid")],by="SDid")
unsampledSD <- temp[is.na(temp$VSid),"SDid"]
testData[['SD']] <- testData[['SD']][!testData[['SD']]$SDid %in% unsampledSD$SDid,]

#names(testData[['DE']])

#Filter our data for 2019 hierachy 1
myValues <- c(2019,1)
myFields <- c("DEyear","DEhierarchy")


myFilteredTestData <- filterRDBESDataObject(testData,
                                          fieldsToFilter = myFields,
                                          valuesToFilter = myValues )
# Remove any orphan records we created during the filtering
myFilteredTestData <- findAndKillOrphans(myFilteredTestData, verbose = FALSE)



# Generate estimates for all strata
myStrataEst <- doEstimationForAllStrata(RDBESDataObjectForEstim = myFilteredTestData ,
                                        hierarchyToUse = 1)

## OK, do the same as above but with the basic test data from the package

myH1RawObject <-
      createRDBESDataObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")

# Update our test data with some random sample measurements (it didn't include these)
myH1RawObject[['SA']]$SAsampWtLive <- round(runif(n=nrow(myH1RawObject[['SA']]),min = 1, max = 100))

# Generate estimates for all strata
myStrataEst <- doEstimationForAllStrata(RDBESDataObjectForEstim = myH1RawObject,
                                        hierarchyToUse = 1)





