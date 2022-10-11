test_that("validateDBEobject does not produce errors or warnings",  {

  myEmptyObject <- createRDBESDataObject()

  expect_warning(validateRDBESDataObject(objectToCheck = myEmptyObject,
                                        verbose = FALSE),NA)
  expect_error(validateRDBESDataObject(objectToCheck = myEmptyObject,
                                      verbose = FALSE),NA)
})
test_that("validateRDBESDataObject returns T for valid empty object",  {

  myEmptyObject <- createRDBESDataObject()
  myReturn <- validateRDBESDataObject(objectToCheck = myEmptyObject,
                                     verbose = FALSE)
  expect_true(myReturn)

})
test_that("validateRDBESDataObject returns T for valid object from H1 data",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_true(myReturn)

})
test_that("validateRDBESDataObject returns T for valid object from H1 data (also checking data types)",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_true(myReturn)

})
test_that("validateRDBESDataObject returns T for valid object from H5 data",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h5_v_1_19_13")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_true(myReturn)

})
test_that("validateRDBESDataObject returns T for valid object from H5 data (also checking data types)",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h5_v_1_19_13")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_true(myReturn)

})
test_that("validateRDBESDataObject returns F for NA",  {

  myReturn <- validateRDBESDataObject(objectToCheck = NA,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESDataObject returns F for object that is not a list",  {

  myNonList <- data.frame (tableNames  = c("DE", "SE"))
  myReturn <- validateRDBESDataObject(objectToCheck = myNonList,
                                     verbose = FALSE)
  expect_false(myReturn)

})

test_that("validateRDBESDataObject returns F for object with extra name",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['XX']] <- F
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESDataObject returns F for object without all names",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['DE']] <- NULL
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESDataObject returns F for object with a required field
          removed",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['DE']]$DEsampScheme <- NULL
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESDataObject returns F for object with duplicate rows",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['DE']] <- data.table::rbindlist(list(myObject[['DE']],myObject[['DE']]))
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESDataObject returns F for object with duplicate DEid values",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['DE']][,"DEid"]  <- replicate(nrow(myObject[['DE']]),1)
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESDataObject returns F for object with invalid data types (when checking data types)",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[["DE"]]$DEid <- as.character(myObject[["DE"]]$DEid)
  myObject[["SD"]]$SDid <- as.character(myObject[["SD"]]$SDid)
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_false(myReturn)

})
test_that("validateRDBESDataObject returns F for object with with duplicate DEid values and invalid data types (when checking data types)",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['DE']][,"DEid"]  <- replicate(nrow(myObject[['DE']]),1)
  myObject[["DE"]]$DEid <- as.character(myObject[["DE"]]$DEid)
  myObject[["SD"]]$SDid <- as.character(myObject[["SD"]]$SDid)
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_false(myReturn)

})
