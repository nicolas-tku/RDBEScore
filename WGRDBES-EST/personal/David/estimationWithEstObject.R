library(dplyr)
library(icesRDBES)

## Step 1) load and prepare some test data

myH1RawObject <-
  createRDBESDataObject(rdbesExtractPath = "./tests/testthat/h1_v_1_19_13")

#Filter our data for WGRDBES-EST TEST 1, 1965, H1
myValues <- c(1965,1,"National Routine",1019159)
myFields <- c("DEyear","DEhierarchy","DEsampScheme","SAspeCode")

myH1RawObject <- filterRDBESDataObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )
myH1RawObject <- findAndKillOrphans(myH1RawObject)

# Edit our data so that we have SRSWOR on each level and calculate the probs
myH1RawObject[["VS"]]$VSselectMeth <- "SRSWOR"
myH1RawObject[["VS"]]$VSincProb <- myH1RawObject[["VS"]]$VSnumSamp / myH1RawObject[["VS"]]$VSnumTotal
myH1RawObject[["VS"]]$VSselProb <- 1/myH1RawObject[["VS"]]$VSnumTotal
myH1RawObject[["FT"]]$FTselectMeth <- "SRSWOR"
myH1RawObject[["FT"]]$FTincProb <- myH1RawObject[["FT"]]$FTnumSamp / myH1RawObject[["FT"]]$FTnumTotal
myH1RawObject[["FT"]]$FTselProb <- 1/myH1RawObject[["FT"]]$FTnumTotal
myH1RawObject[["FO"]]$FOselectMeth <- "SRSWOR"
myH1RawObject[["FO"]]$FOincProb <- myH1RawObject[["FO"]]$FOnumSamp / myH1RawObject[["FO"]]$FOnumTotal
myH1RawObject[["FO"]]$FOselProb <- 1/myH1RawObject[["FO"]]$FOnumTotal
myH1RawObject[["SS"]]$SSselectMeth <- "SRSWOR"
myH1RawObject[["SS"]]$SSincProb <- myH1RawObject[["SS"]]$SSnumSamp / myH1RawObject[["SS"]]$SSnumTotal
myH1RawObject[["SS"]]$SSselProb <- 1/myH1RawObject[["SS"]]$SSnumTotal
myH1RawObject[["SA"]]$SAselectMeth <- "SRSWOR"
myH1RawObject[["SA"]]$SAincProb <- myH1RawObject[["SA"]]$SAnumSamp / myH1RawObject[["SA"]]$SAnumTotal
myH1RawObject[["SA"]]$SAselProb <- 1/myH1RawObject[["SA"]]$SAnumTotal

# Update our test data with some random sample measurements (it didn't include these)
myH1RawObject[['SA']]$SAsampWtLive <- round(runif(n=nrow(myH1RawObject[['SA']]),min = 1, max = 100))

## Step 2) Create an estimation object, but stop at SA since we'll just use the SAsampWtLive
myH1EstObj <- createRDBESEstObject(myH1RawObject, 1, stopTable = "SA", verbose = TRUE)
# Get rid of rows that don't have an SA row
myH1EstObj <- myH1EstObj[!is.na(myH1EstObj$SAid),]



# Parameters
x <- myH1EstObj
targetValue <- "SAsampWtLive"

#x[,..targetValue]

## Step 3) Calcuate a point estimate using targetValue

x$studyVariable <- x[,..targetValue]

targetProbColumns <- names(x)[grep("^.*incProb$", names(x))]
targetNumSampColumns <- names(x)[grep("^.*numSamp$", names(x))]
targetNumTotalColumns <- names(x)[grep("^.*numTotal$", names(x))]

x$totalIncProb <- apply(x[, ..targetProbColumns], 1, prod)
x$numSampProd <- apply(x[, ..targetNumSampColumns], 1, prod)
x$numSampTotal <- apply(x[, ..targetNumTotalColumns], 1, prod)

#x$pointEstimate <- x[[targetValue]] / totalIncProb
x$pointEstimate <- x$studyVariable / x$totalIncProb

# (THis is needed later)
# Note - we are using full product of numSamp and numTotal - we don't want to do this -
# we just want to use product of numSamp and numTotal from SA to FT
targetNumSampColumnSu2 <- targetNumSampColumns[targetNumSampColumns != "su1numSamp"]
targetNumTotalColumnSu2 <- targetNumTotalColumns[targetNumTotalColumns != "su1numTotal"]
x$numSampProdSu2 <- apply(x[, ..targetNumSampColumnSu2], 1, prod)
x$numSampTotalSu2 <- apply(x[, ..targetNumTotalColumnSu2], 1, prod)


# Run estimation using our study variable, and the products of all the numSamp, and the products of all the numTotal, and a hard-coded value for the selection method
myEstimateSA <- estimMC(x$studyVariable,
                        x$numSampProd,
                        x$numSampTotal,
                        "SRSWOR" )


# Compare the total estimates from the sum of point estimates, and estimMC - they are the same :-)
# (What about the estimate of the mean though?)

# 1) Add up all point estimates
sum(x$pointEstimate , na.rm = TRUE)

# 2) Total from estimMC
myEstimateSA$est.total


# Now find the Variance

# For each VSid - run the estimMC function
#xList <- split(x, f = x$VSid)
# For each FTid - run the estimMC function - this will give us a study variable to use
# for VS
xList <- split(x, f = x$FTid)



myResults <- lapply(xList, function(z){
  myOutput <- estimMC(z$studyVariable,
                      #z$numSampProd,
                      #z$numSampTotal,
                      z$numSampProdSu2,
                      z$numSampTotalSu2,
                      unique(z$su1selectMeth))
  myOutput$PI <- NULL # PI causes a mess when I combine the results
  #myOutput$id <- unique(z$VSid)
  myOutput$id <- unique(z$FTid)
  myOutput
})
# Put our results into a data frame
myResults <- bind_rows(myResults)

# Check the total estimate :-)
sum(myResults$est.total)

# Join our estimate for FT, with our original data
xTemp <- left_join(x,myResults,by = c("FTid"="id"))

# Get the unique values we're interested in

xTempFT <- xTemp[, c("VSid", "su1stratumName", "su1numSamp", "su1numTotal","est.total", "su1selectMeth")]
varTemp <- estimMC(xTempFT$est.total, xTempFT$su1numSamp, xTempFT$su1numTotal, unique(xTempFT$su1selectMeth))

# Correct total but var is NA
sum(myResults$est.total)

View(myResults)




## OLD CODE BELOW...


## Step 4) Group by the su1 strata and sum our estimates for these

su1id <- unique(paste0(x[["su1table"]],"id"))
x$su1id <- x[[su1id]]
# Need to take into account the parent record so we don't incorrectly group
# strata from different years/countries etc.
x$su1stratumNameFull <- paste0(x$SDid,":",x$su1stratumName)
#write.csv(x, file="./WGRDBES-EST/personal/David/estTest.csv", row.names = FALSE)

y <- x %>%
  group_by(su1id,su1stratumNameFull,su1numSamp, su1numTotal,su1selProb, su1incProb, su1selectMeth) %>%
  summarise(su1Estimate = sum(pointEstimate, na.rm = TRUE))

# For each strata - run the estimMC function
yList <- split(y, f = y$su1stratumNameFull)

myResults <- lapply(yList, function(z){
  myOutput <- estimMC(z$su1Estimate, z$su1numSamp, z$su1numTotal, unique(z$su1selectMeth))
  myOutput$PI <- NULL # PI causes a mess when I combine the results
  myOutput
})
# Put our strata results into a data frame
myResults <- bind_rows(myResults, .id = "su1stratumNameFull")

## Step 5) Calculate the total estimate and variance of the first sampling unit stage

# Total estimate
# Add up total estimate from estimMC funcion
sum(myResults$est.total)
# Total estimate - add up all point estimates
sum(x$pointEstimate , na.rm = TRUE)
# (These estimates of the total are giving the same values - will this always be the case?)

# Variance of su1
myResults[,c("su1stratumNameFull","est.total","var.total")]




