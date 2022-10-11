test_that("createRDBESDataObject can create an empty object without errors
          or warnings",  {

  myObject <- expect_warning(createRDBESDataObject(),NA)
  myObject <-expect_error(createRDBESDataObject(),NA)
})
test_that("createRDBESDataObject can create an object from an H1 data extract
          without errors or warnings",  {

  myPath <- "./h1_v_1_19_13"

  myObject <- expect_warning(createRDBESDataObject(rdbesExtractPath = myPath),NA)
  myObject <- expect_error(createRDBESDataObject(rdbesExtractPath = myPath),NA)
})
test_that("createRDBESDataObject can create an object from an H1 data extract
          without errors or warnings (when castToCorrectDataTypes = FALSE)",  {

  myPath <- "./h1_v_1_19_13"

  myObject <- expect_warning(
    createRDBESDataObject(rdbesExtractPath = myPath,
                          castToCorrectDataTypes = FALSE),NA)
  myObject <- expect_error(
    createRDBESDataObject(rdbesExtractPath = myPath,
                          castToCorrectDataTypes = FALSE),NA)
})
test_that("createRDBESDataObject can create an object from an H5 data extract
          without errors or warnings",  {

  myPath <- "./h5_v_1_19_13"

  myObject <- expect_warning(createRDBESDataObject(rdbesExtractPath = myPath),NA)
  myObject <- expect_error(createRDBESDataObject(rdbesExtractPath = myPath),NA)
})
test_that("createRDBESDataObject can create an object from an H5 data extract
          without errors or warnings (when castToCorrectDataTypes = FALSE)",  {

    myPath <- "./h5_v_1_19_13"

    myObject <- expect_warning(
              createRDBESDataObject(rdbesExtractPath = myPath,
                                   castToCorrectDataTypes = FALSE),NA)
    myObject <- expect_error(
              createRDBESDataObject(rdbesExtractPath = myPath,
                                   castToCorrectDataTypes = FALSE),NA)
})
test_that("createRDBESDataObject can create an object from an H1 data extract by specifying file names without errors or warnings",  {

  myPath <- "./h1_v_1_19_13"
  myFileNames <- list("DE"="DE.csv","SD"="SD.csv")

  myObject <- expect_warning(createRDBESDataObject(rdbesExtractPath = myPath, listOfFileNames = myFileNames),NA)
  myObject <- expect_error(createRDBESDataObject(rdbesExtractPath = myPath, listOfFileNames = myFileNames),NA)
})
test_that("createRDBESDataObject will give a warning if given a dir with no relevent files in it",  {

  myPath <- "."
  myObject <- expect_warning(createRDBESDataObject(rdbesExtractPath = myPath),"No relevent files found in given directory - an empty object will be created")
})
test_that("createRDBESDataObject creates an object with the correct data types",  {

  myPath <- "./h1_v_1_19_13"

  myRDBESDataObject <- createRDBESDataObject(rdbesExtractPath = myPath,
                         castToCorrectDataTypes = TRUE)

  myDiffs <- validateRDBESDataObjectDataTypes(myRDBESDataObject)

  numberOfDifferences <- nrow(myDiffs)
  expect_equal(numberOfDifferences,0)


})
