#capture.output({  ## suppresses printing of console output when running test()

test_that("importRDBESDataCSV can create an empty object without errors
          or warnings",  {

  myObject <- expect_warning(importRDBESDataCSV(),NA)
  myObject <-expect_error(importRDBESDataCSV(),NA)
})
test_that("importRDBESDataCSV can create an object from an H1 data extract
          without errors or warnings",  {

  myPath <- "./h1_v_1_19_18"

  myObject <- expect_warning(importRDBESDataCSV(rdbesExtractPath = myPath),NA)
  myObject <- expect_error(importRDBESDataCSV(rdbesExtractPath = myPath),NA)
})
test_that("importRDBESDataCSV can create an object from an H1 data extract
          without errors or warnings (when castToCorrectDataTypes = FALSE)",  {

  myPath <- "./h1_v_1_19_18"

  myObject <- expect_warning(
    importRDBESDataCSV(rdbesExtractPath = myPath,
                          castToCorrectDataTypes = FALSE),NA)
  myObject <- expect_error(
    importRDBESDataCSV(rdbesExtractPath = myPath,
                          castToCorrectDataTypes = FALSE),NA)
})
test_that("importRDBESDataCSV can create an object from an H5 data extract
          without errors or warnings",  {

  myPath <- "./h5_v_1_19_18"

  myObject <- expect_warning(importRDBESDataCSV(rdbesExtractPath = myPath),NA)
  myObject <- expect_error(importRDBESDataCSV(rdbesExtractPath = myPath),NA)
})
test_that("importRDBESDataCSV can create an object from an H5 data extract
          without errors or warnings (when castToCorrectDataTypes = FALSE)",  {

    myPath <- "./h5_v_1_19_18"

    myObject <- expect_warning(
              importRDBESDataCSV(rdbesExtractPath = myPath,
                                   castToCorrectDataTypes = FALSE),NA)
    myObject <- expect_error(
              importRDBESDataCSV(rdbesExtractPath = myPath,
                                   castToCorrectDataTypes = FALSE),NA)
})
test_that("importRDBESDataCSV can create an object from an H1 data extract by specifying file names without errors or warnings",  {

  myPath <- "./h1_v_1_19_18"
  myFileNames <- list("DE"="DE.csv","SD"="SD.csv")

  myObject <- expect_warning(importRDBESDataCSV(rdbesExtractPath = myPath, listOfFileNames = myFileNames),NA)
  myObject <- expect_error(importRDBESDataCSV(rdbesExtractPath = myPath, listOfFileNames = myFileNames),NA)
})
test_that("importRDBESDataCSV will give a warning if given a dir with no relevent files in it",  {

  myPath <- "."
  myObject <- expect_warning(importRDBESDataCSV(rdbesExtractPath = myPath),"No relevent files found in given directory - an empty object will be created")
})
test_that("importRDBESDataCSV creates an object with the correct data types",  {

  myPath <- "./h1_v_1_19_18"

  myRDBESDataObject <- importRDBESDataCSV(rdbesExtractPath = myPath,
                         castToCorrectDataTypes = TRUE)

  myDiffs <- validateRDBESDataObjectDataTypes(myRDBESDataObject)

  numberOfDifferences <- nrow(myDiffs)
  expect_equal(numberOfDifferences,0)
})
test_that("importRDBESDataCSV creates an H1 object with keys on the data tables",  {

  myPath <- "./h1_v_1_19_18"

  myRDBESDataObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

  # Not all of the RDBES table types are in the sample data
  expectedNumberOfTablesWithKeys <- 13
  actualNumberOfTablesWithKeys <- 0

  for(aTable in names(myRDBESDataObject)){
    if ('data.table' %in% class(myRDBESDataObject[[aTable]])){
      if (!is.null(key(myRDBESDataObject[[aTable]]))){
        actualNumberOfTablesWithKeys <- actualNumberOfTablesWithKeys + 1
      }
    }
  }

  expect_equal(expectedNumberOfTablesWithKeys,actualNumberOfTablesWithKeys)

})
test_that("importRDBESDataCSV creates an H5 object with keys on the data tables",  {

  myPath <- "./h5_v_1_19_18"

  myRDBESDataObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

  # Not all of the RDBES table types are in the sample data
  expectedNumberOfTablesWithKeys <- 13
  actualNumberOfTablesWithKeys <- 0

  for(aTable in names(myRDBESDataObject)){
    if ('data.table' %in% class(myRDBESDataObject[[aTable]])){
      if (!is.null(key(myRDBESDataObject[[aTable]]))){
        actualNumberOfTablesWithKeys <- actualNumberOfTablesWithKeys + 1
      }
    }
  }

  expect_equal(expectedNumberOfTablesWithKeys,actualNumberOfTablesWithKeys)

})
test_that("importRDBESDataCSV creates an H7 object with keys on the data tables",  {

  myPath <- "./h7_v_1_19_18"

  myRDBESDataObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

  # Not all of the RDBES table types are in the sample data
  expectedNumberOfTablesWithKeys <- 12
  actualNumberOfTablesWithKeys <- 0

  for(aTable in names(myRDBESDataObject)){
    if ('data.table' %in% class(myRDBESDataObject[[aTable]])){
      if (!is.null(key(myRDBESDataObject[[aTable]]))){
        actualNumberOfTablesWithKeys <- actualNumberOfTablesWithKeys + 1
      }
    }
  }

  expect_equal(expectedNumberOfTablesWithKeys,actualNumberOfTablesWithKeys)

})

#}) ## end capture.output
