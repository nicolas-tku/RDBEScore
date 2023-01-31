capture.output({  ## suppresses printing of console output when running test()

test_that("validateRDBESobject does not produce errors or warnings",  {

  myEmptyObject <- createRDBESDataObject()

  expect_warning(validateRDBESDataObject(objectToCheck = myEmptyObject,
                                        verbose = FALSE),NA)
  expect_error(validateRDBESDataObject(objectToCheck = myEmptyObject,
                                      verbose = FALSE),NA)
})
test_that(
  "validateRDBESDataObject returns empty object for valid empty object",  {

  myEmptyObject <- createRDBESDataObject()
  myReturn <- validateRDBESDataObject(objectToCheck = myEmptyObject,
                                     verbose = FALSE)
  expect_equal(myEmptyObject, myReturn)

})
test_that(
  "validateRDBESDataObject returns input object for valid object from H1 data",
  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_equal(myObject, myReturn)

})
test_that("validateRDBESDataObject returns same object for valid object from H1
          data (also checking data types)",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_equal(myObject, myReturn)

})
test_that("validateRDBESDataObject returns same object for valid object from H5
          data",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h5_v_1_19_13")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_equal(myObject, myReturn)

})
test_that("validateRDBESDataObject returns same object for valid object from H5
          data (also checking data types)",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h5_v_1_19_13")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_equal(myObject, myReturn)

})
test_that("validateRDBESDataObject produces error for NA",  {

  expect_error(validateRDBESDataObject(objectToCheck = NA,
                                     verbose = FALSE))

})
test_that("validateRDBESDataObject produces error for object that is not a
          list",  {

  myNonList <- data.frame (tableNames  = c("DE", "SE"))
  expect_error(validateRDBESDataObject(objectToCheck = myNonList,
                                     verbose = FALSE))
})

test_that("validateRDBESDataObject produces error for object with extra name",
          {
  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['XX']] <- F
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object without all names",
          {
  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['DE']] <- NULL
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object with a required
          field removed",  {
  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['DE']]$DEsampScheme <- NULL
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object with duplicate
          rows",  {
  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['DE']] <- data.table::rbindlist(list(myObject[['DE']],myObject[['DE']]))
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object with duplicate
          DEid values",  {
  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['DE']][,"DEid"]  <- replicate(nrow(myObject[['DE']]),1)
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object with invalid data
          types (when checking data types)",  {
  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[["DE"]]$DEid <- as.character(myObject[["DE"]]$DEid)
  myObject[["SD"]]$SDid <- as.character(myObject[["SD"]]$SDid)
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object with with
          duplicate DEid values and invalid data types (when checking data
          types)",  {
  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['DE']][,"DEid"]  <- replicate(nrow(myObject[['DE']]),1)
  myObject[["DE"]]$DEid <- as.character(myObject[["DE"]]$DEid)
  myObject[["SD"]]$SDid <- as.character(myObject[["SD"]]$SDid)
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE))
})

test_that("validateRDBESDataObject prints empty table note",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject[['FT']] <-   myObject[['FT']][FALSE,]
  expect_output(validateRDBESDataObject(objectToCheck = myObject,
                                      verbose = TRUE), regexp = "Note that FT has 0 rows")

})

test_that("validateRDBESDataObject produces error if keys aren't set on a data table",  {

  myObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  # Remove the key from DE
  data.table::setkey(myObject[['DE']],NULL)
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                        verbose = TRUE), regexp = "DE does not have a key set")
})


}) ## end capture.output
