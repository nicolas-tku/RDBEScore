library(icesRDBES)
library(dplyr)


# Load the test data
testData <- icesRDBES::createRDBESRawObject("./FishNCo/testData/RegionalTestData")


# show the non-null table names
names(testData[!unlist(lapply(testData, is.null))])
# validate the data
icesRDBES::validateRDBESRawObject(testData, verbose = FALSE)

# make some changs to our test data so that we can estimate
testData[['SA']]$SAselectMeth = 'SRSWOR'
testData[['SS']]$SSselectMeth = 'SRSWOR'
testData[['FO']]$FOselectMeth = 'SRSWOR'
testData[['FT']]$FTselectMeth = 'SRSWOR'
testData[['VS']]$VSselectMeth = 'SRSWOR'

# Get rid of SS rows without linked SA rows
temp <-
  dplyr::left_join(testData[['SS']],
                   testData[['SA']][,c("SAid","SSid")],
                   by="SSid")
unsampledSS <- temp[is.na(temp$SAid),"SSid"]
testData[['SS']] <-
  testData[['SS']][!testData[['SS']]$SSid %in% unsampledSS$SSid,]

# Get rid of FO rows without linked SS rows
temp <-
  dplyr::left_join(testData[['FO']],
                   testData[['SS']][,c("SSid","FOid")],
                   by="FOid")
unsampledFO <- temp[is.na(temp$SSid),"FOid"]
testData[['FO']] <-
  testData[['FO']][!testData[['FO']]$FOid %in% unsampledFO$FOid,]

# Get rid of FT rows without linked FO rows
temp <-
  dplyr::left_join(testData[['FT']],
                   testData[['FO']][,c("FOid","FTid")],
                   by="FTid")
unsampledFT <- temp[is.na(temp$FOid),"FTid"]
testData[['FT']] <-
  testData[['FT']][!testData[['FT']]$FTid %in% unsampledFT$FTid,]

# Get rid of VS rows without linked FT rows
temp <-
  dplyr::left_join(testData[['VS']],
                   testData[['FT']][,c("FTid","VSid")],
                   by="VSid")
unsampledVS <- temp[is.na(temp$FTid),"VSid"]
testData[['VS']] <-
  testData[['VS']][!testData[['VS']]$VSid %in% unsampledVS$VSid,]

# Get rid of SD rows without linked VS rows
temp <-
  dplyr::left_join(testData[['SD']],
                   testData[['VS']][,c("VSid","SDid")],
                   by="SDid")
unsampledSD <- temp[is.na(temp$VSid),"SDid"]
testData[['SD']] <-
  testData[['SD']][!testData[['SD']]$SDid %in% unsampledSD$SDid,]


#myFields <- c("DEyear", "DEhierarchy")
#myValues <- c(2019,1) # filter values here
myFields <- c("DEhierarchy")
myValues <- c(1) # filter values here

myFilteredTestData <- icesRDBES::filterRDBESRawObject(testData,
                                                      fieldsToFilter = myFields,
                                                      valuesToFilter = myValues )

# Remove any orphan records we created during the filtering
myFilteredTestData <-
  icesRDBES::findAndKillOrphans(myFilteredTestData, verbose = FALSE)

# Generate estimates for all strata
StrataEst <-
  doEstimationForAllStrata(rdbesRawObjectForEstim = myFilteredTestData ,
                           hierarchyToUse = 1)

saveRDS(StrataEst, file = "./FishNCo/testData/StrataEst.rds")
