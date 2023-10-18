capture.output({  ## suppresses printing of console output when running test()




  # common parameters
  dirH1 <- "./h1_v_1_19_18/"
  dirH5 <- "./h5_v_1_19_18/"

  expObjH1 <- readRDS(paste0(dirH1, "H1_2023_10_16.rds"))

  # LEid is present in H1 due to the export
  class(expObjH1$FT$LEid) <- "integer"
  class(expObjH1$SA$LEid) <- "integer"
  zipH1 <- "./h1_v_1_19_18/H1_2023_10_16.zip"

  # TEST GENERAL BEHAVIOUR --------------------------------------------------

  test_that("createRDBESDataObject will give a warning if given a dir with no relevant  files in it",  {

    myPath <- "."
    expect_warning(createRDBESDataObject(input = myPath),"No relevent files found in given directory - an empty object will be created")


  })

  # Test ZIP inputs ---------------------------------------------------------

  test_that("importing zipped H1 example data works", {
    zipFiles <- c(
      "H1_2023_10_16.zip"
    )

    genObj <- createRDBESDataObject(zipH1,
                                    castToCorrectDataTypes = TRUE)

    expect_equal(genObj, expObjH1)

  })

  test_that("importing some data that is not zipped with some data that are zipped H1 example data should not work", {
    zipFiles <- c(
      "H1_2023_10_16.zip",
      "HSL_2023_10_16.zip",
      "VesselDetails.csv"
    )

    genObj <- expect_error(
      createRDBESDataObject(paste0(dirH1, zipFiles),
                                     castToCorrectDataTypes = TRUE),
      "You cannot import a mix of 'csv' and 'zip' inputs. To import multiple tables unzip all files and import as a folder of 'csv' files."
    )

  })

  test_that("importing subset H1 example data works", {
    zipFiles <- c(
      "HSL_2023_10_16.zip",
      "HVD_2023_10_16.zip"
    )

    genObj <- createRDBESDataObject(paste0(dirH1, zipFiles),
                                             castToCorrectDataTypes = TRUE)
    expect_equal(genObj$VD, expObjH1$VD)
    expect_equal(genObj$SS, NULL)
    expect_equal(genObj$SL, expObjH1$SL)
  })

  test_that("Overwriting a table from a zip file produces a warning", {
    zipFiles <- c(
      "H1_2023_10_16.zip",
      "HVD_2023_10_16.zip")


    expect_warning(
      createRDBESDataObject(paste0(dirH1, zipFiles),
                                     castToCorrectDataTypes = FALSE),
      "Duplicate unzipped files detected:\n")


  })

  test_that("The order of importing should not matter", {
    zipFiles1 <- c(
      "H1_2023_10_16.zip",
      "HVD_2023_10_16.zip")

    genObj1 <- suppressWarnings(
    createRDBESDataObject(paste0(dirH1, zipFiles1),
                                    castToCorrectDataTypes = TRUE))

    zipFiles2 <- c("HVD_2023_10_16.zip",
                   "H1_2023_10_16.zip")

    genObj2 <- suppressWarnings(
      createRDBESDataObject(paste0(dirH1, zipFiles2),
                                     castToCorrectDataTypes = TRUE))

    expect_equal(genObj1$VD, genObj2$VD)
    expect_equal(genObj1$SS, genObj2$SS)
    expect_equal(genObj1$SL, genObj2$SL)
    expect_equal(genObj1$FT, genObj2$FT)
    expect_equal(genObj1, genObj2)
  })




# Test CSV inputs ---------------------------------------------------------



  # NULL tests ?
  # test_that("createRDBESDataObject can create an empty object without errors
  #         or warnings",  {
  #
  #           myObject <- expect_warning(createRDBESDataObject(),NA)
  #           myObject <-expect_error(createRDBESDataObject(),NA)
  #         })
  #


  test_that("createRDBESDataObject can create an object from a H1 data extract
          without errors or warnings",  {

            csvFilesH1 <- dirH1

            expect_warning(createRDBESDataObject(input = csvFilesH1), NA)
            expect_error(createRDBESDataObject(input = csvFilesH1), NA)
          })



  test_that("createRDBESDataObject can create an object from a H1 data extract
          without errors or warnings (when castToCorrectDataTypes = FALSE)",  {

            csvFilesH1 <- dirH1

            expect_warning(
              createRDBESDataObject(input = csvFilesH1,
                                 castToCorrectDataTypes = FALSE), NA)
            expect_error(
              createRDBESDataObject(input = csvFilesH1,
                                 castToCorrectDataTypes = FALSE), NA)
          })


  test_that("createRDBESDataObject can create an object from an H5 data extract
          without errors or warnings",  {

            csvFilesH5 <- dirH5

          expect_warning(createRDBESDataObject(input = csvFilesH5), NA)
          expect_error(createRDBESDataObject(input = csvFilesH5), NA)
          })


  test_that("createRDBESDataObject can create an object from an H5 data extract
          without errors or warnings (when castToCorrectDataTypes = FALSE)",  {

            csvFilesH5 <- dirH5

           expect_warning(
              createRDBESDataObject(input = csvFilesH5,
                                 castToCorrectDataTypes = FALSE), NA)
            expect_error(
              createRDBESDataObject(input = csvFilesH5,
                                 castToCorrectDataTypes = FALSE), NA)
          })


  test_that("createRDBESDataObject can create an object from an H1 data extract by specifying file names without errors or warnings",  {

    csvFilesH1 <- dirH1
    myFileNames <- list("DE"="DE.csv","SD"="SD.csv")

    myObject <- expect_warning(createRDBESDataObject(input = csvFilesH1, listOfFileNames = myFileNames), NA)
    myObject <- expect_error(createRDBESDataObject(input = csvFilesH1, listOfFileNames = myFileNames), NA)

  })



  test_that("createRDBESDataObject will give a warning if given a dir with no relevent files in it",  {

    myPath <- "."
    expect_warning(createRDBESDataObject(input = myPath),"No relevent files found in given directory - an empty object will be created")


  })



  test_that("createRDBESDataObject creates an object with the correct data types",  {

    myPath <- "./h1_v_1_19_18"

    myRDBESDataObject <- createRDBESDataObject(rdbesExtractPath = myPath,
                                            castToCorrectDataTypes = TRUE)

    myDiffs <- validateRDBESDataObjectDataTypes(myRDBESDataObject)

    numberOfDifferences <- nrow(myDiffs)
    expect_equal(numberOfDifferences,0)
  })




  test_that("createRDBESDataObject creates an H1 object with keys on the data tables",  {

    myPath <- "./h1_v_1_19_18"

    myRDBESDataObject <- createRDBESDataObject(rdbesExtractPath = myPath)

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
  test_that("createRDBESDataObject creates an H5 object with keys on the data tables",  {

    myPath <- "./h5_v_1_19_18"

    myRDBESDataObject <- createRDBESDataObject(rdbesExtractPath = myPath)

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
  test_that("createRDBESDataObject creates an H7 object with keys on the data tables",  {

    myPath <- "./h7_v_1_19_18"

    myRDBESDataObject <- createRDBESDataObject(rdbesExtractPath = myPath)

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



