test_that("createRDBESEstObject can create an object from an H1 data extract
          with no warnings or errors",  {

  myPath <- ".\\h1_v_1_19"
  myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)

  myEstObject <- expect_warning(createRDBESEstObject(myRawObject,1),NA)
  myEstObject <- expect_error(createRDBESEstObject(myRawObject,1),NA)
  # Check we have the right number of sampling unit fields
  expect_equal(length(grep("^su1.*",names(myEstObject))),19)
  expect_equal(length(grep("^su2.*",names(myEstObject))),19)
  expect_equal(length(grep("^su3.*",names(myEstObject))),19)
  expect_equal(length(grep("^su4.*",names(myEstObject))),19)
  expect_equal(length(grep("^su5.*",names(myEstObject))),11)
  expect_equal(length(grep("^su6.*",names(myEstObject))),0)
  expect_equal(length(grep("^su7.*",names(myEstObject))),0)
  expect_equal(length(grep("^su8.*",names(myEstObject))),0)


})
test_that("createRDBESEstObject can create an object from an H5 data extract
          with no warnings or errors",  {

  myPath <- ".\\h5_v_1_19"
  myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)

  myEstObject <- expect_warning(createRDBESEstObject(myRawObject,5),NA)
  myEstObject <- expect_error(createRDBESEstObject(myRawObject,5),NA)
  # Check we have the right number of sampling unit fields
  expect_equal(length(grep("^su1.*",names(myEstObject))),19)
  expect_equal(length(grep("^su2.*",names(myEstObject))),19)
  expect_equal(length(grep("^su3.*",names(myEstObject))),19)
  expect_equal(length(grep("^su4.*",names(myEstObject))),19)
  expect_equal(length(grep("^su5.*",names(myEstObject))),11)
  expect_equal(length(grep("^su6.*",names(myEstObject))),0)
  expect_equal(length(grep("^su7.*",names(myEstObject))),0)
  expect_equal(length(grep("^su8.*",names(myEstObject))),0)


})
test_that("createRDBESEstObject can create an object from an empty H1 data extract
          with no warnings or errors",  {

  myRawObject <- createRDBESRawObject()

  myEstObject <- expect_warning(createRDBESEstObject(myRawObject,1),NA)
  myEstObject <- expect_error(createRDBESEstObject(myRawObject,1),NA)
  # Check we have the right number of sampling unit fields
  expect_equal(length(grep("^su1.*",names(myEstObject))),0)
  expect_equal(length(grep("^su2.*",names(myEstObject))),0)
  expect_equal(length(grep("^su3.*",names(myEstObject))),0)
  expect_equal(length(grep("^su4.*",names(myEstObject))),0)
  expect_equal(length(grep("^su5.*",names(myEstObject))),0)
  expect_equal(length(grep("^su6.*",names(myEstObject))),0)
  expect_equal(length(grep("^su7.*",names(myEstObject))),0)
  expect_equal(length(grep("^su8.*",names(myEstObject))),0)


})
test_that("createRDBESEstObject can create an object from an H1 data extract with sub-sampling
          with no warnings or errors",  {

  myPath <- ".\\h1_v_1_19"
  myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)

  # Put some sub-sampling in the test data
  # Remove column because it seems to be logical data type
  myRawObject[["SA"]][, SAparentID:= NULL]
  # Generate some new parent ids
  myRawObject[["SA"]][51:100,"SAparentID"] <- myRawObject[["SA"]][1:50,"SAid"]
  myRawObject[["SA"]][101:125,"SAparentID"] <- myRawObject[["SA"]][51:75,"SAid"]
  # Fix the column order
  setcolorder(myRawObject[["SA"]],
              c("SAid","SAparentID", setdiff(names(myRawObject[["SA"]]), c("SAid","SAparentID"))   )
  )

  myEstObject <- expect_warning(createRDBESEstObject(myRawObject,1),NA)
  myEstObject <- expect_error(createRDBESEstObject(myRawObject,1),NA)
  # Check we have the right number of sampling unit fields
  expect_equal(length(grep("^su1.*",names(myEstObject))),19)
  expect_equal(length(grep("^su2.*",names(myEstObject))),19)
  expect_equal(length(grep("^su3.*",names(myEstObject))),19)
  expect_equal(length(grep("^su4.*",names(myEstObject))),19)
  expect_equal(length(grep("^su5.*",names(myEstObject))),11)
  expect_equal(length(grep("^su6.*",names(myEstObject))),11)
  expect_equal(length(grep("^su7.*",names(myEstObject))),11)
  expect_equal(length(grep("^su8.*",names(myEstObject))),0)


})
test_that("createRDBESEstObject fails when an invalid hierarchy is requested",  {

  myPath <- ".\\h1_v_1_19"
  myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)
  myEstObject <- expect_error(createRDBESEstObject(myRawObject,99),"An invalid value was used for the 'hierarchyToUse' parameter - createRDBESEstObject will not proceed")

})
test_that("createRDBESEstObject can create an object from an H1 data extract
          with no warnings or errors, stopping at VS",  {

    myPath <- ".\\h1_v_1_19"
    myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)

    myEstObject <- expect_warning(createRDBESEstObject(myRawObject,1,stopTable = "VS"),NA)
    myEstObject <- expect_error(createRDBESEstObject(myRawObject,1,stopTable = "VS"),NA)
    expect_equal(unique(myEstObject$su1table),"VS")
    # Check we have the right number of sampling unit fields
    expect_equal(length(grep("^su1.*",names(myEstObject))),19)
    expect_equal(length(grep("^su2.*",names(myEstObject))),0)
    expect_equal(length(grep("^su3.*",names(myEstObject))),0)
    expect_equal(length(grep("^su4.*",names(myEstObject))),0)
    expect_equal(length(grep("^su5.*",names(myEstObject))),0)
    expect_equal(length(grep("^su6.*",names(myEstObject))),0)
    expect_equal(length(grep("^su7.*",names(myEstObject))),0)
    expect_equal(length(grep("^su8.*",names(myEstObject))),0)


})
