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


x <- myH1EstObj

suLevels <- names(x)[grep("^su.table$", names(x))]
#suLevels <- gsub("table","",suLevels)
#suLevels <- gsub("su","",suLevels)

targetValue <- "SAsampWtLive"

n <- length(suLevels)

# Hard code to SS being above SA - TODO
xList <- split(x, f = x$SSid)

myResults <- lapply(xList, function(z){
  myOutput <- estimMC(z[[targetValue]],
                         sampled = z[[paste0("su",n,"numSamp")]],
                         total = z[[paste0("su",n,"numTotal")]],
                         method = unique(z[[paste0("su",n,"selectMeth")]])
  )
  myOutput$PI <- NULL # PI causes a mess when I combine the results
  myOutput$SSid <- unique(z$SSid)
  myOutput[[paste0()]] <- myOutput$est.total
  myOutput
})
# Put our results into a data frame
myResults <- bind_rows(myResults)

# Go left 1 level
n <- n-1

left_join(x,myResults[,c("SSid",)])



myResult <- estimMC(x[[targetValue]],
        sampled = x[[paste0("su",n,"numSamp")]],
        total = x[[paste0("su",n,"numTotal")]],
        method = unique(x[[paste0("su",n,"selectMeth")]])
        )




# Parameters
x <- myH1EstObj
targetValue <- "SAsampWtLive"
