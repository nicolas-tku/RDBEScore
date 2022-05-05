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
test_that(paste("unequal probality sampling is not implemented"), {
  tot <- 4
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  expect_error(estimMC(items, rep(elems, elems), rep(tot, elems),
    method = "UPSWOR"
  ))
  expect_error(estimMC(items, rep(elems, elems), rep(tot, elems),
    method = "UPSWR"
  ))
})

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
