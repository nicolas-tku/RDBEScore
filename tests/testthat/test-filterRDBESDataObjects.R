capture.output({  ## suppresses printing of console output when running test()

test_that("filterRDBESDataObject returns the correct result for
          H1 (1)", {

  myH1RawObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_18")

  # Only use a subset of the test data
  myH1RawObject <- filterRDBESDataObject(myH1RawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

  myFields <- c("FTarvLoc")
  myValues <- c("ZWBFO")
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues
  )
  myFilteredObject <- findAndKillOrphans(myFilteredObject, verbose = FALSE)

  # Check the filtered object is ok (should return the object)
  expect_equal(myFilteredObject, validateRDBESDataObject(myFilteredObject,
                                                         verbose = FALSE))
  # Check the expected number of FT rows are returned
  expect_equal(nrow(myFilteredObject[["FT"]]), 20)
})

test_that("filterRDBESDataObject returns the correct result for H1 (2)", {
  myH1RawObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_18")

  # Only use a subset of the test data
  myH1RawObject <- filterRDBESDataObject(myH1RawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

  myFields <- c("FTarvLoc")
  myValues <- c("ZWBFO", "ZWBZH")
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues
  )

  # Check the filtered object is ok (should return TRUE)
  expect_equal(myFilteredObject, validateRDBESDataObject(myFilteredObject,
                                                         verbose = FALSE))
  # Check the expected number of FT rows are returned
  expect_equal(nrow(myFilteredObject[["FT"]]), 35)
})
test_that("filterRDBESDataObject returns the correct result for two fields/
          two values", {
  myH1RawObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_18")
  myFields <- c("DEyear", "DEhierarchy")
  myValues <- c(1965, 2)
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues
  )

  # Check the filtered object is ok (should return TRUE)
  expect_equal(myFilteredObject, validateRDBESDataObject(myFilteredObject,
                                                         verbose = FALSE))
  # Check the expected number of DE rows are returned
  expect_equal(nrow(myFilteredObject[["DE"]]), 0)
})
test_that("filterRDBESDataObject returns the correct result for H1 (3)", {
  myH1RawObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_18")
  # Only use a subset of the test data
  myH1RawObject <- filterRDBESDataObject(myH1RawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)
  myFields <- c("DEyear", "DEhierarchy")
  myValues <- c(1965, 1)
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues
  )

  # Check the filtered object is ok (should return TRUE)
  expect_equal(myFilteredObject, validateRDBESDataObject(myFilteredObject,
                                                         verbose = FALSE))
  # Check the expected number of DE rows are returned
  expect_equal(nrow(myFilteredObject[["DE"]]), 3)
})
test_that("filterRDBESDataObject returns the correct result for three fields/
          three values", {
  myH1RawObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_18")
  myFields <- c("DEsampSchemeType", "DEsampScheme", "DEstratumName")
  myValues <- c("NatPilCF", "National Routine", "DE_stratum3_H1")
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues
  )

  # Check the filtered object is ok (should return TRUE)
  expect_equal(myFilteredObject, validateRDBESDataObject(myFilteredObject,
                                                         verbose = FALSE))
  # Check the expected number of DE rows are returned
  expect_equal(nrow(myFilteredObject[["DE"]]), 1)
})
test_that("filterRDBESDataObject returns a warning if incorrect field name is used", {
  myH1RawObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_18")
  myFields <- c("DEabc")
  myValues <- c("ZWBFO")

  expect_warning(
    filterRDBESDataObject(myH1RawObject,
      fieldsToFilter = myFields,
      valuesToFilter = myValues
    ),
    "The following fields were not found in the RDBESDataObject: DEabc"
  )
})

test_that("filterRDBESDataObject successfully removes orphans", {
  myH1RawObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_18")

  # remove all the VS rows (but not any other rows)
  myFields <- c("VSunitName")
  myValues <- c("VDcode_8")
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues,
    killOrphans = TRUE
  )

  expect_equal(
    nrow(myFilteredObject$BV),
    2160
  )
  expect_equal(
    nrow(myFilteredObject$FM),
    1080
  )
})

test_that("filterRDBESDataObject does not removes orphans when killOrphans = FALSE", {
  myH1RawObject <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_18")

  # remove all the VS rows (but not any other rows)
  myFields <- c("VSunitName")
  myValues <- c("VDcode_8")
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues,
    killOrphans = FALSE
  )

  expect_equal(
    nrow(myFilteredObject$BV),
    nrow(myH1RawObject$BV)
  )
})

}) ## end capture.output
