test_that("filterRDBESRawObject returns the correct result for single field/value",  {

  myH1RawObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19_13")
  myFields <- c("FTarvLoc")
  myValues <- c("ZWBFO" )
  myFilteredObject <- filterRDBESRawObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )

  # Check the filtered object is ok (should return TRUE)
  expect_true(validateRDBESRawObject(myFilteredObject, verbose = FALSE))
  # Check the expected number of FT rows are returned
  expect_equal(nrow(myFilteredObject[["FT"]]),17)

})
test_that("filterRDBESRawObject returns the correct result for single field/ two values",  {

  myH1RawObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19_13")
  myFields <- c("FTarvLoc")
  myValues <- c("ZWBFO","ZWBZH" )
  myFilteredObject <- filterRDBESRawObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )

  # Check the filtered object is ok (should return TRUE)
  expect_true(validateRDBESRawObject(myFilteredObject, verbose = FALSE))
  # Check the expected number of FT rows are returned
  expect_equal(nrow(myFilteredObject[["FT"]]),37)

})
test_that("filterRDBESRawObject returns the correct result for two fields/ two values", {

  myH1RawObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19_13")
  myFields <- c("DEyear","DEhierarchy")
  myValues <- c(1965,2)
  myFilteredObject <- filterRDBESRawObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )

  # Check the filtered object is ok (should return TRUE)
  expect_true(validateRDBESRawObject(myFilteredObject, verbose = FALSE))
  # Check the expected number of DE rows are returned
  expect_equal(nrow(myFilteredObject[["DE"]]),0)

})
test_that("filterRDBESRawObject returns the correct result for two fields/ two values", {

  myH1RawObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19_13")
  myFields <- c("DEyear","DEhierarchy")
  myValues <- c(1965,1)
  myFilteredObject <- filterRDBESRawObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )

  # Check the filtered object is ok (should return TRUE)
  expect_true(validateRDBESRawObject(myFilteredObject, verbose = FALSE))
  # Check the expected number of DE rows are returned
  expect_equal(nrow(myFilteredObject[["DE"]]),3)

})
test_that("filterRDBESRawObject returns the correct result for three fields/ three values", {

  myH1RawObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19_13")
  myFields <- c("DEsampSchemeType","DEsampScheme","DEstratumName")
  myValues <- c("NatPilCF","National Routine","DE_stratum3_H1")
  myFilteredObject <- filterRDBESRawObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )

  # Check the filtered object is ok (should return TRUE)
  expect_true(validateRDBESRawObject(myFilteredObject, verbose = FALSE))
  # Check the expected number of DE rows are returned
  expect_equal(nrow(myFilteredObject[["DE"]]),1)

})
test_that("filterRDBESRawObject returns a warning if incorrect field name is used",  {

  myH1RawObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19_13")
  myFields <- c("DEabc")
  myValues <- c("ZWBFO" )

  expect_warning(filterRDBESRawObject(myH1RawObject,
                                      fieldsToFilter = myFields,
                                      valuesToFilter = myValues ),
                 "The following fields were not found in the RDBESRawObject: DEabc" )

})
