capture.output({  ## suppresses printing of console output when running test()

## ------------SRSWOR-----------------------------
varSRSWOR <- function(y, n, N) {
  meanY <- mean(y)
  S2 <- (1 / (n - 1)) * sum((y - meanY)^2)
  N^2 * (1 - n / N) * (S2 / n)
}
### --------- all elements in sample-------------
testMain <- "SRSWOR all elements in sample"
test_that(paste0("total variance is 0 if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$var.total, 0)
})

test_that(paste("mean variance is 0 if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$var.mean, 0)
})

test_that(paste("correct total if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, mean(items) * tot)

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, mean(items) * tot)
})

test_that(paste("correct mean if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.mean, mean(items))

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.mean, sum(items) / elems)
})



### --------- half of elements in sample-------------
testMain <- "SRSWOR 50% of elements in sample"
test_that(paste("total variance is correct if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expected <- varSRSWOR(items, elems, tot)
  expect_equal(x$var.total, expected)

  tot <- 4
  items <- c(2,2)
  elems <-length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expected <- varSRSWOR(items, elems, tot)
  expect_equal(x$var.total, expected)
})

test_that(paste("mean variance is correct if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expected <- varSRSWOR(items, elems, tot) / tot
  expect_equal(x$var.mean, expected)
})

test_that(paste("correct total if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, mean(items) * tot)

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, mean(items) * tot)

  tot <- 4
  items <- c(3, 4)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, mean(items) * tot)
})

test_that(paste("correct mean if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.mean, mean(items))

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.mean, sum(items) / elems)
})


######## --------- 10% in sample-------------######
testMain <- "SRSWOR 10% elements in sample"
test_that(paste("total variance is correct if", testMain), {
  tot <- 20
  items <- c(3, 4)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expected <- varSRSWOR(items, elems, tot)
  expect_equal(x$var.total, expected)

  tot <- 40
  items <- c(3, 4, 3, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expected <- varSRSWOR(items, elems, tot)
  expect_equal(x$var.total, expected)
})

test_that(paste("total variance is NaN for 1 sample if", testMain), {
  tot <- 10
  items <- c(3)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expected <- varSRSWOR(items, elems, tot)
  expect_equal(x$var.total, expected)
})

test_that(paste("mean variance is correct if", testMain), {
  tot <- 20
  items <- c(3, 4)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expected <- varSRSWOR(items, elems, tot) / tot
  expect_equal(x$var.mean, expected)

})

test_that(paste("correct total if", testMain), {
  tot <- 20
  items <- c(3, 4)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, mean(items) * tot)

  items <- c(4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, mean(items) * tot)

  tot <- 30
  items <- c(3, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, mean(items) * tot)

  tot <- 40
  items <- c(3, 4, 5, 6)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, mean(items) * tot)
})

test_that(paste("correct mean if", testMain), {
  tot <- 20
  items <- c(3, 4)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.mean, mean(items))

  items <- c(4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.mean, mean(items))
})


######## ------------SRSWR----------------------------######
varSRSWR <- function(y, n, N) {
  meanY <- mean(y)
  S2 <- 1 / (n - 1) * (sum((y - meanY)^2))
  N^2 * (S2 / n)
}
######## --------- all elements in sample-------------######
testMain <- "SRSWR all elements in sample"
test_that(paste0("total variance if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expected <- varSRSWR(items, elems, tot)
  expect_equal(x$var.total, expected)
})

test_that(paste("mean variance if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expected <- varSRSWR(items, elems, tot) / tot
  expect_equal(x$var.mean, expected)
})

test_that(paste("correct total if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.total, mean(items) * tot)

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.total, mean(items) * tot)
})

test_that(paste("correct mean if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.mean, mean(items))

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.mean, mean(items))
})


###

### --------- half of elements in sample-------------
testMain <- "SRSWR 50% of elements in sample"
test_that(paste("total variance is correct if", testMain), {
  tot <- 10
  items <- c(3, 4, 4, 5, 6)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expected <- varSRSWR(items, elems, tot)
  expect_equal(x$var.total, expected)

  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expected <- varSRSWR(items, elems, tot)
  expect_equal(x$var.total, expected)
})

test_that(paste("mean variance is correct if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expected <- varSRSWR(items, elems, tot) / tot
  expect_equal(x$var.mean, expected)
})

test_that(paste("correct total if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.total, mean(items) * tot)

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.total, mean(items) * tot)
})

test_that(paste("correct mean if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.mean, mean(items))

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.mean, mean(items))
})

######## --------- 10% in sample-------------######
testMain <- "SRSWR 10% elements in sample"
test_that(paste("total variance is correct if", testMain), {
  tot <- 20
  items <- c(3, 4)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expected <- varSRSWR(items, elems, tot)
  expect_equal(x$var.total, expected)

  tot <- 10
  items <- c(4)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expected <- varSRSWR(items, elems, tot)
  expect_equal(x$var.total, expected)
})

test_that(paste("mean variance is correct if", testMain), {
  tot <- 20
  items <- c(3, 4)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expected <- varSRSWR(items, elems, tot) / tot
  expect_equal(x$var.mean, expected)
})

test_that(paste("correct total if", testMain), {
  tot <- 20
  items <- c(3, 4)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.total, mean(items) * tot)

  items <- c(4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.total, mean(items) * tot)

  tot <- 30
  items <- c(3, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.total, mean(items) * tot)
})

test_that(paste("correct mean if", testMain), {
  tot <- 20
  items <- c(3, 4)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.mean, mean(items))

  items <- c(4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "SRSWR")
  expect_equal(x$est.mean, mean(items))
})

## ------------CENSUS-----------------------------
testMain <- "CENSUS all  elements in sample"
test_that(paste0("total variance if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "CENSUS")
  expect_equal(x$var.total, 0)
})

test_that(paste("mean variance if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "CENSUS")
  expect_equal(x$var.mean, 0)
})

test_that(paste("correct total if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "CENSUS")
  expect_equal(x$est.total, sum(items))

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "CENSUS")
  expect_equal(x$est.total, sum(items))
})

test_that(paste("correct mean if", testMain), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "CENSUS")
  expect_equal(x$est.mean, mean(items))

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems), "CENSUS")
  expect_equal(x$est.mean, sum(items) / elems)
})


### --------- errors-------------
testMain <- "error"

test_that(paste("all inputs must have same length"), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  expect_error(estimMC(items, elems, rep(tot, elems)))
  expect_error(estimMC(items, rep(elems, elems), tot))
})

test_that(paste("all inputs must be numeric"), {
  tot <- 4
  items <- letters[1:5]
  elems <- length(items)
  expect_error(estimMC(items, rep(elems, elems), rep(tot, elems)))
  expect_error(estimMC(1:5, rep(elems, elems), rep("4", elems)))
})

test_that(paste("sampled and total must be provided for SRSWR"), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  expect_error(estimMC(items,rep(NA, elems), rep(NA, elems),"SRSWR"),"sampled and total must be provided - NAs not allowed!")
})

test_that(paste("sampled and total must be provided for SRSWOR"), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  expect_error(estimMC(items,rep(NA, elems), rep(NA, elems),"SRSWOR"),"sampled and total must be provided - NAs not allowed!")
})

test_that(paste("sampled and total must be provided for CENSUS"), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  expect_error(estimMC(items,rep(NA, elems), rep(NA, elems),"CENSUS"),"sampled and total must be provided - NAs not allowed!")
})

test_that(paste("incProb must be provided for UPSWOR - NAs"), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  expect_error(estimMC(items, rep(elems, elems), rep(tot, elems),"UPSWOR", NA, NA), "incProb must be provided - NAs or NULL not allowed!")
})

test_that(paste("incProb must be provided for UPSWOR - NULL"), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  expect_error(estimMC(items, rep(elems, elems), rep(tot, elems),"UPSWOR"), "incProb must be provided - NAs or NULL not allowed!")
})

test_that(paste("selProb must be provided for UPSWR - NAs"), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  expect_error(estimMC(items, rep(elems, elems), rep(tot, elems),"UPSWR", NA, NA), "selProb must be provided - NAs or NULL not allowed!")
})

test_that(paste("selProb must be provided for UPSWR - NAs"), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  expect_error(estimMC(items, rep(elems, elems), rep(tot, elems),"UPSWR"), "selProb must be provided - NAs or NULL not allowed!")
})

### ------------PROBABILITIES-----------------------------
testMain <- "UPSWOR 50% of elements in sample - use equal probabilites"
test_that(paste("correct total if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  selProb <- rep(1/tot, elems)
  incProb <- rep(elems/tot, elems)
  x <- estimMC(items,
               sampled = rep(elems, elems),
               total = rep(tot, elems),
               method = "UPSWOR",
               selProb = selProb,
               incProb = incProb)
  expect_equal(x$est.total, mean(items) * tot)

  tot <- 4
  items <- c(2,2)
  elems <-length(items)
  selProb <- rep(1/tot, elems)
  incProb <- rep(elems/tot, elems)
  x <- estimMC(items,
               sampled = rep(elems, elems),
               total = rep(tot, elems),
               method = "UPSWOR",
               selProb = selProb,
               incProb = incProb)
  expect_equal(x$est.total, mean(items) * tot)
})
test_that(paste("NA variance total if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  selProb <- rep(1/tot, elems)
  incProb <- rep(elems/tot, elems)
  x <- estimMC(items,
               sampled = rep(elems, elems),
               total = rep(tot, elems),
               method = "UPSWOR",
               selProb = selProb,
               incProb = incProb)
  expect_equal(x$var.total, NA)

  tot <- 4
  items <- c(2,2)
  elems <-length(items)
  selProb <- rep(1/tot, elems)
  incProb <- rep(elems/tot, elems)
  x <- estimMC(items,
               sampled = rep(elems, elems),
               total = rep(tot, elems),
               method = "UPSWOR",
               selProb = selProb,
               incProb = incProb)
  expect_equal(x$var.total, NA)
})
testMain <- "UPSWR 50% of elements in sample - use equal probabilites"
test_that(paste("correct total if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  selProb <- rep(1/tot, elems)
  incProb <- rep(elems/tot, elems)
  x <- estimMC(items,
               sampled = rep(elems, elems),
               total = rep(tot, elems),
               method = "UPSWR",
               selProb = selProb,
               incProb = incProb)
  expect_equal(x$est.total, mean(items) * tot)

  tot <- 4
  items <- c(2,2)
  elems <-length(items)
  selProb <- rep(1/tot, elems)
  incProb <- rep(elems/tot, elems)
  x <- estimMC(items,
               sampled = rep(elems, elems),
               total = rep(tot, elems),
               method = "UPSWR",
               selProb = selProb,
               incProb = incProb)
  expect_equal(x$est.total, mean(items) * tot)
})
test_that(paste("total variance is correct if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  selProb <- rep(1/tot, elems)
  incProb <- rep(elems/tot, elems)
  x <- estimMC(items,
               sampled = rep(elems, elems),
               total = rep(tot, elems),
               method = "UPSWR",
               selProb = selProb,
               incProb = incProb)
  expected <- varSRSWR(items, elems, tot)
  expect_equal(x$var.total, expected)

  tot <- 4
  items <- c(2,2)
  elems <-length(items)
  selProb <- rep(1/tot, elems)
  incProb <- rep(elems/tot, elems)
  x <- estimMC(items,
               sampled = rep(elems, elems),
               total = rep(tot, elems),
               method = "UPSWR",
               selProb = selProb,
               incProb = incProb)
  expected <- varSRSWR(items, elems, tot)
  expect_equal(x$var.total, expected)
})

### ------------WORKED EXAMPLES-----------------------------
testMain <- "Annica DeG worked examples"
test_that(paste("SRSWOR", testMain), {
  y <- c(147.1,102.2,79.5)
  sampled <- c(3,3,3)
  total <- c(9,9,9)
  x <- estimMC(y, sampled, total, method = "SRSWOR")
  expect_equal(round(x$est.total,1), 986.4)
  expect_equal(round(x$var.total,1), 21303.2)
})
test_that(paste("SRSWR", testMain), {
  y <- c(18,10,12,18)
  sampled <- c(4,4,4,4)
  total <- c(12,12,12,12)
  x <- estimMC(y, sampled, total, method = "SRSWR")
  expect_equal(round(x$est.total,1), 174.0)
  expect_equal(round(x$var.total,1), 612.0)
})
test_that(paste("UPSWOR", testMain), {
  tot <- 9
  items <- c(147.1,102.2,79.5)
  elems <- length(items)
  selProb <- NA
  incProb <- c(0.18565,0.19853,0.41690)
  x <- estimMC(items,
               sampled = rep(elems, elems),
               total = rep(tot, elems),
               method = "UPSWOR",
               selProb = selProb,
               incProb = incProb)
  expect_equal(round(x$est.total,0), 1498)
  expect_equal(x$var.total, NA)
})
test_that(paste("UPSWR", testMain), {
  tot <- 15
  items <- c(75,203,203,191,168)
  elems <- length(items)
  selProb <- c(24/647,100/647,100/647,76/647,44/647)
  incProb <- NA
  x <- estimMC(items,
               sampled = rep(elems, elems),
               total = rep(tot, elems),
               method = "UPSWR",
               selProb = selProb,
               incProb = incProb)
  expect_equal(round(x$est.total,1), 1749.0)
  expect_equal(round(x$var.total,1), 49471.5)
})

}) ## end capture.output
