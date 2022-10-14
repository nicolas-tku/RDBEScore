## Multiple-count (MC) estimator Functions
# Compare these implmentations of generalised functions against
# the implementation in estimMC

library(RDBEScore)
# Get the estimMC function for comparison
#source("R/estimMC.R")

# Get the expected value E(nk) for the MC estimator
mcGetExpectedValueK <- function(y,k,selectionMethod){

  Enk <- NA

  if (selectionMethod == "SRSWOR"){
    Enk <- y[k,'numberSampled'] / y[k,'numberTotal']
  } else if (selectionMethod == "SRSWR"){
    # Note: this takes the same form as SRSWOR because the "numberSampled" value will either be the
    # the number of unique units sampled for SRSWOR, or the number of draws made in the case of SRSWR
    Enk <- y[k,'numberSampled'] / y[k,'numberTotal']
  }
  Enk
}

# Get the expected value E(nknl) for the MC estimator
mcGetExpectedValueKL <- function(y,k,l,selectionMethod){

  Enknl <- NA

  if (selectionMethod == "SRSWOR"){
    Enknl <- (y[k,'numberSampled'] * (y[k,'numberSampled'] - 1)) /
      (y[k,'numberTotal'] * (y[k,'numberTotal'] - 1))
  } else if (selectionMethod == "SRSWR"){
    Enknl <- (y[k,'numberSampled'] * (y[k,'numberSampled'] - 1)) /
      (y[k,'numberTotal'] * y[k,'numberTotal'])
  }
  #print(Enknl)
  Enknl
}

# Multiple-count (MC) population univariate estimator
mcPopulationEstimator <- function(y){

  selectionMethod <- NA

  # See which selection methods are present
  if (length(unique(y$SAselectionMethod)) > 1 ) {
    stop("Can't handle mutiple selection methods")
  } else if (unique(y$SAselectionMethod) %in% c("SRSWOR","SRSWR")) {
    selectionMethod <- unique(y$SAselectionMethod)
  } else {
    stop(paste0("Function not implemented for ",unique(y$SAselectionMethod)," yet"))
  }

  popEstimate <- NA

  for (k in 1:nrow(y)){
    #print(popEstimate)
    Enk <- mcGetExpectedValueK(y,k,selectionMethod)
    #print(Enk)
    popEstimate_k <- y[k,'studyVariable'] / Enk
    #print(popEstimate_k)
    if (is.na(popEstimate)){
      popEstimate <- popEstimate_k
    } else {
      popEstimate <- popEstimate + popEstimate_k
    }
  }
  popEstimate
}




# Mutiple-count estimator of variance (attempt 2 following equation 4 in
# "Some Generalizations of the Horvitz-Thompson Estimator" http://www.asasrms.org/Proceedings/y2009/Files/302918.pdf)
mcVarianceEstimator <- function(y){

  selectionMethod <- NA

  # See which selection methods are present
  if (length(unique(y$SAselectionMethod)) > 1 ) {
    stop("Can't handle mutiple selection methods")
  } else if (unique(y$SAselectionMethod) %in% c("SRSWOR","SRSWR")) {
    selectionMethod <- unique(y$SAselectionMethod)
  } else {
    stop(paste0("Function not implemented for ",unique(y$SAselectionMethod)," yet"))
  }

  varEstimate <- NA

  # There's probably a more efficient way to do this in R but I wanted to
  # stay close to how the equations are written to start with
  for (k in 1:nrow(y)){
    for (l in 1:nrow(y)){
      # Don't include k == l
      if (k != l){
        Enk <- mcGetExpectedValueK(y,k,selectionMethod)
        Enl <- mcGetExpectedValueK(y,l,selectionMethod)
        Enknl <- mcGetExpectedValueKL(y,k,l,selectionMethod)
        part1 <- (Enk*Enl - Enknl)/Enknl
        part2a <- y[k,'studyVariable'] / Enk
        part2b <- y[l,'studyVariable'] / Enl
        part2 <- (part2a - part2b)^2
        varEstimate_kl <- part1 * part2
        if (is.na(varEstimate)){
          varEstimate <- varEstimate_kl
        } else {
          varEstimate <- varEstimate + varEstimate_kl
      }
      }
    }
  }

  varEstimate <- varEstimate/2

  varEstimate

}



# Now we will build some simple test data
vessels <- seq(1:10)
numberOfTripsForEachVessel <- 10
myTestData <- data.frame(vesselId = rep(vessels, each = numberOfTripsForEachVessel))
myTestData$tripId <- seq(1,nrow(myTestData))
discards <- c(100,200,300,400,500)
myTestData$speciesCode <- 'OTH'
myTestData$discardedWeight <- discards

## SRSWOR
# Sample the data using single stage simple random sampling without replacement (srswor)
numberOfSamples <- 1
y_srswor <- myTestData[sample(nrow(myTestData),numberOfSamples, replace = FALSE),]
y_srswor$SAselectionMethod <- 'SRSWOR'
y_srswor$SAnumberSampled <- numberOfSamples
y_srswor$SAnumberTotal <- nrow(myTestData)
# Get correctly named fields in our sample data
y_srswor$studyVariable <- y_srswor$discardedWeight
y_srswor$numberSampled <- y_srswor$SAnumberSampled
y_srswor$numberTotal <- y_srswor$SAnumberTotal

# Use our functions to make a univariate estimate of the total discards

# Estimte the total discards
mcPopulationEstimator(y_srswor)

# What are the actual total discards of the population?
sum(myTestData$discardedWeight)

# Calculate the variance of our estimate - 0 if I sample everything
mcVarianceEstimator(y_srswor)

# Compare these results with those from "estimMC.R" - hopefully should be the same
estimReturn <- estimMC(y = y_srswor$studyVariable, sampled = y_srswor$numberSampled, total = y_srswor$numberTotal, method = unique(y_srswor$SAselectionMethod) )
estimReturn$est.total
estimReturn$var.total


## SRSWR
# Sample the data using single stage simple random sampling with replacement (SRSWR)
numberOfSamples <- 1
y_srswr <- myTestData[sample(nrow(myTestData),numberOfSamples, replace = TRUE),]
y_srswr$SAselectionMethod <- 'SRSWR'
y_srswr$SAnumberSampled <- numberOfSamples
y_srswr$SAnumberTotal <- nrow(myTestData)
# Get correctly named fields in our sample data
y_srswr$studyVariable <- y_srswr$discardedWeight
y_srswr$numberSampled <- y_srswr$SAnumberSampled
y_srswr$numberTotal <- y_srswr$SAnumberTotal

# Estimte the total discards
mcPopulationEstimator(y_srswr)

# What are the actual total discards of the population?
sum(myTestData$discardedWeight)

# Calculate the variance of our estimate
mcVarianceEstimator(y_srswr)

# Compare these results with those from "estimMC.R" - hopefully should be the same
estimReturn <- estimMC(y = y_srswr$studyVariable, sampled = y_srswr$numberSampled, total = y_srswr$numberTotal, method = unique(y_srswr$SAselectionMethod) )
estimReturn$est.total
estimReturn$var.total

