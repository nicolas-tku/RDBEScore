test_that("findAndKillOrphans runs without errors on an empty RDBESRawObject",  {

  myEmptyObject <- createRDBESRawObject()

  expect_error(findAndKillOrphans(objectToCheck = myEmptyObject,
                                  verbose = FALSE),NA)


})
test_that("findAndKillOrphans runs without errors on an RDBESRawObject with no orphans",  {

  myH1RawObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")

  expect_error(findAndKillOrphans(objectToCheck = myH1RawObject,
                                  verbose = FALSE),NA)

})
test_that("findAndKillOrphans removes orphans on an filtered RDBESRawObject",  {

  myH1RawObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19")

  # remove all the VS rows (but not any other rows)
  myFields <- c("VSunitName")
  myValues <- c("blah" )
  myFilteredObject <- filterRDBESRawObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )

  # Check the expected number of rows are returned
  expect_equal(nrow(myFilteredObject[["VS"]]),0)
  expect_equal(nrow(myFilteredObject[["FT"]]),243)

  # Remove the orphans
  myObjectNoOrphans <- findAndKillOrphans(objectToCheck = myFilteredObject,
                                          verbose = FALSE)

  # Check the expected number of rows are returned
  expect_equal(nrow(myObjectNoOrphans[["DE"]]),4)
  expect_equal(nrow(myObjectNoOrphans[["SD"]]),4)
  expect_equal(nrow(myObjectNoOrphans[["VS"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["FT"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["FO"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["SS"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["SA"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["FM"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["BV"]]),0)

})
