test_that("createRDBESEstObject can create an object from an H1 data extract
          with only the expected warning",  {

  myPath <- ".\\h1_v_1_19"
  myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)
  myEstObject <- expect_warning(createRDBESEstObject(myRawObject,1),"Function is a work in progress and not fully tested yet")
  #myEstObject <- expect_error(createRDBESEstObject(myRawObject,1),NA)


})
