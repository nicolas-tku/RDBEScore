library(icesRDBES)
library(ggplot2)

# This code illustrates how increasing the number of samples decreases the spread of the estimated vales i.e. the variance

# First we will build some simple population data
vessels <- seq(1:10)
numberOfTripsForEachVessel <- 10
myTestData <- data.frame(vesselId = rep(vessels, each = numberOfTripsForEachVessel))
myTestData$tripId <- seq(1,nrow(myTestData))
#discards <- c(100,200,300,400,500)
myTestData$speciesCode <- 'OTH'
discards <- round(runif(n=nrow(myTestData),min = 1, max = 100))
myTestData$discardedWeight <- discards

# This is the real population total
popTotal <- sum(myTestData$discardedWeight)


# Function to re-sample the data a number of times (50) and get an estimate each time
reSampleData <- function(numberOfSamples, selectionMethod = 'SRSWOR'){

  mySampResults <- NULL
  if (selectionMethod == 'SRSWOR'){
    replaceParam <- FALSE
  } else if (selectionMethod == 'SRSWR'){
    replaceParam <- TRUE
  } else {
    stop("Unknown value for selectionMethod")
  }

  # Resample the data 50 times and create an estimate each time
  for (i in 1:50){

    y_srswor <- myTestData[sample(nrow(myTestData),numberOfSamples, replace = replaceParam),]
    y_srswor$selectionMethod <- selectionMethod
    y_srswor$numberSampled <- numberOfSamples
    y_srswor$numberTotal <- nrow(myTestData)

    # Estimte the discards
    myEstim <- estimMC(y_srswor$discardedWeight, y_srswor$numberSampled, y_srswor$numberTotal, unique(y_srswor$selectionMethod))
    sampTotal <- myEstim$est.total
    varTotal <- myEstim$var.total
    mySampResults <- rbind(mySampResults,
                             data.frame(numberOfSamples = numberOfSamples,
                                        selectionMethod =  unique(y_srswor$selectionMethod),
                                        estimateTotal = sampTotal,
                                        estimatevariance = varTotal))
  }
  mySampResults
}



## SRSWOR
#
mySampResultsAll <- NULL
for (i in seq(from=5,to=100,by=5)) {
  srswori <- reSampleData(i,selectionMethod = 'SRSWOR')
  mySampResultsAll <- rbind(mySampResultsAll,srswori)
}
mySampResultsAll$numberOfSamples <- as.factor(mySampResultsAll$numberOfSamples)

# Plot the data - as we increase the number of samples we decrease the variance of our estimates
ggplot(data = mySampResultsAll, mapping = aes(x = numberOfSamples, y = estimateTotal)) +
  geom_boxplot() +
  geom_hline(yintercept=popTotal, color = "red", size=1) +
  ggtitle("SRSWOR")

# Plot the data - as we increase the number of samples we decrease the variance of our estimates
ggplot(data = mySampResultsAll, mapping = aes(x = numberOfSamples, y = estimatevariance)) +
  geom_boxplot() +
  ggtitle("SRSWOR")



## SRSWR
#
mySampResultsAll <- NULL
for (i in seq(from=5,to=100,by=5)) {
  srswri <- reSampleData(i,selectionMethod = 'SRSWR')
  mySampResultsAll <- rbind(mySampResultsAll,srswri)
}
mySampResultsAll$numberOfSamples <- as.factor(mySampResultsAll$numberOfSamples)

# Plot the data - as we increase the number of samples we decrease the variance of our estimates
# - not as much as when using SRSWOR though
ggplot(data = mySampResultsAll, mapping = aes(x = numberOfSamples, y = estimateTotal)) +
  geom_boxplot() +
  geom_hline(yintercept=popTotal, color = "red", size=1) +
  ggtitle("SRSWR")

ggplot(data = mySampResultsAll, mapping = aes(x = numberOfSamples, y = estimatevariance)) +
  geom_boxplot() +
  ggtitle("SRSWR")

