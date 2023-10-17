capture.output({  ## suppresses printing of console output when running test()


  # Test ZIP inputs ---------------------------------------------------------

  # common parameters
  dirH1 <- "tests/testthat/h1_v_1_19_18"
  expObjH1 <- readRDS("tests/testthat/h1_v_1_19_18/H1_2023_10_16.rds")
  class(expObjH1$FT$LEid) <- "integer"
  zipH1 <- "tests/testthat/h1_v_1_19_18/H1_2023_10_16.zip"

  test_that("importing zipped H1 example data works", {
    zipFiles <- c(
      "H1_2023_10_16.zip"
    )

    genObj <- createRDBESDataObject(zipH1,
                                    castToCorrectDataTypes = TRUE)

    expect_equal(genObj, expObjH1)

  })

  test_that("importing some data that is not zipped H1 example data works", {
    zipFiles <- c(
      "H1_2023_10_16.zip",
      "HSL_2023_10_16.zip",
      "VesselDetails.csv"
    )

    genObj <- expect_warning(
      RDBEScore:::importRDBESDataZIP(paste0(ddir, zipFiles),
                                     castToCorrectDataTypes = TRUE),
      "Overwriting file: VesselDetails.csv, this might be intended!"
    )

    expect_equal(genObj, expObj)
  })

  test_that("importing subset H1 example data works", {
    zipFiles <- c(
      "HSL_2023_10_16.zip",
      "VesselDetails.csv"
    )

    genObj <- RDBEScore:::importRDBESDataZIP(paste0(ddir, zipFiles),
                                             castToCorrectDataTypes = TRUE)
    expect_equal(genObj$VD, expObj$VD)
    expect_equal(genObj$SS, NULL)
    expect_equal(genObj$SL, expObj$SL)
  })

  test_that("Overwriting a table from a csv file produces a warning", {
    zipFiles <- c(
      "HVD_2023_10_16.zip",
      "VesselDetails.csv"
    )

    expect_warning(
      RDBEScore:::importRDBESDataZIP(paste0(ddir, zipFiles),
                                     castToCorrectDataTypes = FALSE),
      "Overwriting file: VesselDetails.csv, this might be intended!"
    )
  })


  test_that("Overwriting a table from a zip file produces a warning", {
    zipFiles <- c(
      "HVD_2023_10_16.zip",
      "H1_2023_10_16.zip"
    )

    expect_warning(
      RDBEScore:::importRDBESDataZIP(paste0(ddir, zipFiles),
                                     castToCorrectDataTypes = FALSE),
      "Duplicate unzipped files detected in"
    )
  })


# Test CSV inputs ---------------------------------------------------------

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



}) ## end capture.output
