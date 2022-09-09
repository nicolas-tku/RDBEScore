# a few convenience functions to check results
getCorrectEstimates <- function(tblName, hd, startEstim, propSampled) {
  Ty <- paste0(tblName, "y")
  numStages <- which(names(hd) == tblName) # from A to B
  prevRows <- nrow(hd[[1]])^(numStages - 2)
  prevID <- paste0(names(hd)[numStages - 1], "id")



  b <- hd[[tblName]][hd[[tblName]][[prevID]] %in% c(1:prevRows), ]
  # TODO the variance checks might be misleading repair
  if (propSampled != 1) {
  }

  expMean <- mean(b[[Ty]])
  expTotal <- sum(b[[Ty]]) * ((1 / propSampled)^numStages)
  expVar <- ifelse(propSampled == 1, 0, var(b[[Ty]]))
  expVarTot <- expVar * ((1 / propSampled)^numStages)

  return(list(
    expMean = expMean, expTotal = expTotal,
    expMeanVar = expVar, expTotalVar = expVarTot
  ))
}
## ------------CENSUS-----------------------------
testMain <- "CENSUS all  elements in sample"
propSampled <- 1
hd <- generateTestTbls(LETTERS[1:5], propSamp = propSampled)
startEstim <- list(parentIdCol = "Aid", ids = c(1))

test_that(paste0("total if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    # expect_equal(res$estim$est.mean, exp$expMean)#, "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    expect_equal(res$estim$est.total, exp$expTotal) # ,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})

test_that(paste0("total variance if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    # expect_equal(res$estim$est.mean, exp$expMean)#, "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    # expect_equal(res$estim$est.total, exp$expTotal)#,"Total"
    expect_equal(res$estim$var.total, exp$expTotalVar) # , "Var total"
  }
})

test_that(paste0("mean variance if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    # expect_equal(res$estim$est.mean, exp$expMean)#, "Mean"
    expect_equal(res$estim$var.mean, exp$expMeanVar) # , "Var mean"
    # expect_equal(res$estim$est.total, exp$expTotal)#,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})

test_that(paste0("mean if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    expect_equal(res$estim$est.mean, exp$expMean) # , "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    # expect_equal(res$estim$est.total, exp$expTotal)#,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})


## ------------SRSWOR-----------------------------
varSRSWOR <- function(y, n, N) {
  meanY <- mean(y)
  S2 <- (1 / (n - 1)) * sum((y - meanY)^2)
  N^2 * (1 - n / N) * (S2 / n)
}
### --------- all elements in sample-------------
testMain <- "SRSWOR all elements in sample "
propSampled <- 1
hd <- generateTestTbls(LETTERS[1:5], selMeth = "SRSWOR", propSamp = propSampled)
startEstim <- list(parentIdCol = "Aid", ids = c(1))

test_that(paste0("total if", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    # expect_equal(res$estim$est.mean, exp$expMean)#, "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    expect_equal(res$estim$est.total, exp$expTotal) # ,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})

test_that(paste0("total variance if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    # expect_equal(res$estim$est.mean, exp$expMean)#, "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    # expect_equal(res$estim$est.total, exp$expTotal)#,"Total"
    expect_equal(res$estim$var.total, exp$expTotalVar) # , "Var total"
  }
})

test_that(paste0("mean variance if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    # expect_equal(res$estim$est.mean, exp$expMean)#, "Mean"
    expect_equal(res$estim$var.mean, exp$expMeanVar) # , "Var mean"
    # expect_equal(res$estim$est.total, exp$expTotal)#,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})

test_that(paste0("mean if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    expect_equal(res$estim$est.mean, exp$expMean) # , "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    # expect_equal(res$estim$est.total, exp$expTotal)#,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})





### --------- half of elements in sample-------------
testMain <- "SRSWOR 50% of elements in sample"
propSampled <- 0.5
hd <- generateTestTbls(LETTERS[1:5], selMeth = "SRSWOR", propSamp = propSampled)
startEstim <- list(parentIdCol = "Aid", ids = c(1))

test_that(paste0("total if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    # expect_equal(res$estim$est.mean, exp$expMean)#, "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    expect_equal(res$estim$est.total, exp$expTotal) # ,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})

test_that(paste0("mean if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    expect_equal(res$estim$est.mean, exp$expMean) # , "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    # expect_equal(res$estim$est.total, exp$expTotal)#,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})



######## --------- 10% in sample-------------######
testMain <- "SRSWOR 10% elements in sample"
propSampled <- 0.1
hd <- generateTestTbls(LETTERS[1:5], selMeth = "SRSWOR", propSamp = propSampled)
startEstim <- list(parentIdCol = "Aid", ids = c(1))

test_that(paste0("total if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    # expect_equal(res$estim$est.mean, exp$expMean)#, "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    expect_equal(res$estim$est.total, exp$expTotal) # ,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})


test_that(paste0("mean if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    expect_equal(res$estim$est.mean, exp$expMean) # , "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    # expect_equal(res$estim$est.total, exp$expTotal)#,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})



######## ------------SRSWR----------------------------######
varSRSWR <- function(y, n, N) {
  meanY <- mean(y)
  S2 <- 1 / (n - 1) * (sum((y - meanY)^2))
  N^2 * (S2 / n)
}
######## --------- all elements in sample-------------######
testMain <- "SRSWR all elements in sample"
propSampled <- 1
startEstim <- list(parentIdCol = "Aid", ids = c(1))
hd <- generateTestTbls(LETTERS[1:5], selMeth = "SRSWR", propSamp = propSampled)

test_that(paste0("total if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    # expect_equal(res$estim$est.mean, exp$expMean)#, "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    expect_equal(res$estim$est.total, exp$expTotal) # ,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})


test_that(paste0("mean if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    expect_equal(res$estim$est.mean, exp$expMean) # , "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    # expect_equal(res$estim$est.total, exp$expTotal)#,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})



### --------- half of elements in sample-------------
testMain <- "SRSWR 50% of elements in sample"
propSampled <- 0.5
startEstim <- list(parentIdCol = "Aid", ids = c(1))
hd <- generateTestTbls(LETTERS[1:5], selMeth = "SRSWR", propSamp = propSampled)

test_that(paste0("total if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    # expect_equal(res$estim$est.mean, exp$expMean)#, "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    expect_equal(res$estim$est.total, exp$expTotal) # ,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})


test_that(paste0("mean if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    expect_equal(res$estim$est.mean, exp$expMean) # , "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    # expect_equal(res$estim$est.total, exp$expTotal)#,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})

######## --------- 10% in sample-------------######
testMain <- "SRSWR 10% elements in sample"
propSampled <- 0.1
startEstim <- list(parentIdCol = "Aid", ids = c(1))
hd <- generateTestTbls(LETTERS[1:5], selMeth = "SRSWR", propSamp = propSampled)

test_that(paste0("total if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    # expect_equal(res$estim$est.mean, exp$expMean)#, "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    expect_equal(res$estim$est.total, exp$expTotal) # ,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})


test_that(paste0("mean if ", testMain), {
  for (L in LETTERS[2:5]) {
    Ty <- paste0(L, "y")
    res <- doMultiStageEstimation(tbls = hd, startEstim, target = Ty)
    exp <- getCorrectEstimates(L, hd, startEstim, propSampled)
    expect_equal(res$estim$est.mean, exp$expMean) # , "Mean"
    # expect_equal(res$estim$var.mean, exp$expMeanVar)#, "Var mean"
    # expect_equal(res$estim$est.total, exp$expTotal)#,"Total"
    # expect_equal(res$estim$var.total, exp$expTotalVar)#, "Var total"
  }
})








### --------- errors-------------
testMain <- "error"
