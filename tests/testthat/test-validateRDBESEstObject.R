test_that("validateRDBESEstObject does not produce errors or warnings",  {

  myEmptyObject <- createRDBESRawObject()
  myEStObj <- createRDBESEstObject(myEmptyObject,1)

  expect_warning(validateRDBESEstObject(myEStObj),NA)
  expect_error(validateRDBESEstObject(myEStObj),NA)
})
test_that("validateRDBESEstObject returns T for valid empty object",  {

  myEmptyObject <- createRDBESRawObject()
  myEStObj <- createRDBESEstObject(myEmptyObject,1)
  myReturn <- validateRDBESEstObject(myEStObj)
  expect_true(myReturn)

})
test_that("validateRDBESEstObject returns T for valid object from H1 data",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h1_v_1_19_13")
  myEStObj <- createRDBESEstObject(myObject,1)
  myReturn <- validateRDBESEstObject(myEStObj)
  expect_true(myReturn)

})
test_that("validateRDBESEstObject returns T for valid object from H5 data",  {

  myObject <- createRDBESRawObject(rdbesExtractPath = ".\\h5_v_1_19_13")
  myEStObj <- createRDBESEstObject(myObject,5)
  myReturn <- validateRDBESEstObject(myEStObj)
  expect_true(myReturn)

})
test_that("validateRDBESEstObject returns F for NA",  {

  myReturn <- validateRDBESEstObject(NA)
  expect_false(myReturn)

})
test_that("validateRDBESEstObject returns F for a data frame",  {

  myTemp <- data.frame(a=c(0,1))
  myReturn <- validateRDBESEstObject(myTemp)
  expect_false(myReturn)

})
test_that("validateRDBESEstObject returns F for an incorrectly defined class",  {

  myTemp <- data.frame(a=c(0,1))
  class(myTemp) <- c("RDBESEstObject", class(myTemp))
  myReturn <- validateRDBESEstObject(myTemp)
  expect_false(myReturn)

})
test_that("validateRDBESEstObject returns F for an RDBESRawObject",  {

  myEmptyObject <- createRDBESRawObject()
  myReturn <- validateRDBESEstObject(myEmptyObject)
  expect_false(myReturn)

})
