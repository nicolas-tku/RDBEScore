test_that("combineRDBESDataObjects returns error for NA",  {

    expect_error(combineRDBESDataObjects(RDBESDataObject1=NA,
                                        RDBESDataObject2=NA),"At least one of the RDBESDataObjects is not valid - mergeRDBESDataObjects will not proceed")
})
test_that("combineRDBESDataObjects returns invalid RDBESDataObject when supplied with duplicate RDBESDataObjects",  {

  myPath <- "./h1_v_1_19_13"
  myObject1 <- createRDBESDataObject(rdbesExtractPath = myPath)
  myObject2 <- createRDBESDataObject(rdbesExtractPath = myPath)

  # Check these are valid objects before we try and combine them
  expect_true(validateRDBESDataObject(myObject1))
  expect_true(validateRDBESDataObject(myObject2))

  myCombinedObject <- combineRDBESDataObjects(RDBESDataObject1=myObject1,
                                             RDBESDataObject2=myObject2)

  expect_false(validateRDBESDataObject(myCombinedObject))
})
test_that("combineRDBESDataObjects returns valid RDBESDataObject when supplied with valid, different RDBESDataObjects",  {

  myObject1 <- createRDBESDataObject(rdbesExtractPath = "./h1_v_1_19_13")
  myObject2 <- createRDBESDataObject(rdbesExtractPath = "./h5_v_1_19_13")

  # Check these are valid objects before we try and combine them
  expect_true(validateRDBESDataObject(myObject1))
  expect_true(validateRDBESDataObject(myObject2))

  myCombinedObject <- combineRDBESDataObjects(RDBESDataObject1=myObject1,
                                             RDBESDataObject2=myObject2)

  expect_true(validateRDBESDataObject(myCombinedObject))

})



