## Multiple-count (MC) estimator Functions

# Get the expected value E(nk) for the MC estimator
mcGetExpectedValueK <- function(y,k,selectionMethod){

  Enk <- NA

  if (selectionMethod == "SRSWOR"){
    Enk <- y[k,'numberSampled'] / (y[k,'numberTotal'])
  } else if (selectionMethod == "SRSWR"){
    # Note: this takes the same form as SRSWOR because the "numberSampled" value will either be the
    # the number of unique units sampled for SRSWOR, or the number of draws made in the case of SRSWR
    Enk <- y[k,'numberSampled'] /( y[k,'numberTotal'])
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
    warning("selection probabilities of unit k and l needed?" )
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

# Mutiple-count estimator of variance
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
    #print(k)
    for (l in 1:nrow(y)){
      #print(l)
      Enk <- mcGetExpectedValueK(y,k,selectionMethod)
      Enl <- mcGetExpectedValueK(y,l,selectionMethod)
      Enknl <- mcGetExpectedValueKL(y,k,l,selectionMethod)
      #part1 <-  abs(Enknl - (Enk * Enl)) / Enknl
      part1 <-  (Enknl - (Enk * Enl)) / Enknl
      part2 <- y[k,'numberSampled'] / Enk
      part3 <- y[l,'numberSampled'] / Enl
      varEstimate_kl <- part1 * part2 * part3
      if (is.na(varEstimate)){
        varEstimate <- varEstimate_kl
      } else {
        varEstimate <- varEstimate + varEstimate_kl
      }
    }
  }
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









