capture.output({  ## suppresses printing of console output when running test()

test_that("combineRDBESDataObjects returns error for NA",  {

    expect_error(combineRDBESDataObjects(RDBESDataObject1=NA,
                                        RDBESDataObject2=NA),
                 "objectToCheck is NA")
})

test_that("combineRDBESDataObjects returns invalid RDBESDataObject when
          supplied with duplicate RDBESDataObjects",  {

  myPath <- "./h1_v_1_19_13"
  myObject1 <- createRDBESDataObject(rdbesExtractPath = myPath)
  myObject2 <- createRDBESDataObject(rdbesExtractPath = myPath)

  # Check these are valid objects before we try and combine them
  expect_error(validateRDBESDataObject(myObject1), NA)
  expect_error(validateRDBESDataObject(myObject2), NA)

  myCombinedObject <- combineRDBESDataObjects(RDBESDataObject1=myObject1,
                                             RDBESDataObject2=myObject2)

  expect_error(validateRDBESDataObject(myCombinedObject), "duplicate rows")
})

test_that("combineRDBESDataObjects returns valid RDBESDataObject when supplied
          with valid, different RDBESDataObjects",  {

  myObject1 <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject2 <- createRDBESDataObject(rdbesExtractPath = "./h5_v_1_19_13")

  # Check these are valid objects before we try and combine them
  expect_error(validateRDBESDataObject(myObject1), NA)
  expect_error(validateRDBESDataObject(myObject2), NA)

  myCombinedObject <- combineRDBESDataObjects(RDBESDataObject1=myObject1,
                                             RDBESDataObject2=myObject2)

  expect_error(validateRDBESDataObject(myCombinedObject), NA)
})

}) ## end capture.output
