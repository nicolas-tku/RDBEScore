capture.output({  ## suppresses printing of console output when running test()

# common parameters
ddir <- "./h1_v_1_19_13/"
expObj <- readRDS(paste0(ddir, "h1_RDBESDataObject.rds"))


test_that("importing zipped H1 example data works", {
  zipFiles <- c(
    "H1_2022_10_05.zip",
    "HSL_2022_10_05.zip",
    "HVD_2022_10_05.zip"
  )

  genObj <- importRDBESDownloadData(paste0(ddir, zipFiles),
                                    castToCorrectDataTypes = TRUE)

  expect_equal(genObj, expObj)

})

test_that("importing some data that is not zipped H1 example data works", {
  zipFiles <- c(
    "H1_2022_10_05.zip",
    "HSL_2022_10_05.zip",
    "VesselDetails.csv"
  )

  genObj <- expect_warning(
    importRDBESDownloadData(paste0(ddir, zipFiles),
                            castToCorrectDataTypes = TRUE),
    "Overwriting file: VesselDetails.csv, this might be intended!"
  )

  expect_equal(genObj, expObj)
})

test_that("importing subset H1 example data works", {
  zipFiles <- c(
    "HSL_2022_10_05.zip",
    "VesselDetails.csv"
  )

  genObj <- importRDBESDownloadData(paste0(ddir, zipFiles),
                                    castToCorrectDataTypes = TRUE)
  expect_equal(genObj$VD, expObj$VD)
  expect_equal(genObj$SS, NULL)
  expect_equal(genObj$SL, expObj$SL)
})

test_that("Overwriting a table from a csv file produces a warning", {
  zipFiles <- c(
    "HVD_2022_10_05.zip",
    "VesselDetails.csv"
  )

  expect_warning(
    importRDBESDownloadData(paste0(ddir, zipFiles),
                            castToCorrectDataTypes = FALSE),
    "Overwriting file: VesselDetails.csv, this might be intended!"
  )
})


test_that("Overwriting a table from a zip file produces a warning", {
  zipFiles <- c(
    "HVD_2022_10_05.zip",
    "H1_2022_10_05.zip"
  )

  expect_warning(
    importRDBESDownloadData(paste0(ddir, zipFiles),
                            castToCorrectDataTypes = FALSE),
    "Duplicate unzipped files detected in"
  )
})

}) ## end capture.output
