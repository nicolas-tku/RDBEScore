library(dplyr)

## Step 1) load nad prepare some test data

myH1RawObject <-
  createRDBESDataObject(rdbesExtractPath = "tests/testthat/h1_v_1_19")

#Filter our data for WGRDBES-EST TEST 1, 1965, H1
myValues <- c(1965,1,"WGRDBES-EST TEST 1",1019159)
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


## Step 3) Calcuate a point estimate using targetValue
# pointEstimate
targetProbColumns <- names(x)[grep("^.*incProb$", names(x))]
totalIncProb <- apply(x[, ..targetProbColumns], 1, prod)
x$pointEstimate <- x[[targetValue]] / totalIncProb


## Step 4) Group by the su1 strata and sum our estimates for these

su1id <- unique(paste0(x[["su1table"]],"id"))
x$su1id <- x[[su1id]]
# Need to take into account the parent record so we don't incorrectly group
# strata from different years/countries etc.
x$su1stratumNameFull <- paste0(x$SDid,":",x$su1stratumName)

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




