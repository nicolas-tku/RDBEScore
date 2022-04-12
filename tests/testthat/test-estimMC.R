
### --------- all elements in sample-------------
testMain <- "all elements in sample"
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
  expect_equal(x$est.total, sum(items))

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, sum(items))
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
testMain <- "half of elements in sample"
test_that(paste("total variance is correct if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$var.total, 5 + 1 / 3)
})

test_that(paste("mean variance is correct if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$var.mean, 2 / 3)
})

test_that(paste("correct total if", testMain), {
  tot <- 8
  items <- c(3, 4, 4, 5)
  elems <- length(items)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, sum(items) * 2)

  items <- c(3, 1, 4, 2)
  x <- estimMC(items, rep(elems, elems), rep(tot, elems))
  expect_equal(x$est.total, sum(items) * 2)
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
