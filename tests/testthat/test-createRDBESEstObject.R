test_that("createRDBESEstObject can create an object from an H1 data extract
          with no warnings or errors",  {

  myPath <- ".\\h1_v_1_19"
  myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)

  myEstObject <- expect_warning(createRDBESEstObject(myRawObject,1),NA)
  myEstObject <- expect_error(createRDBESEstObject(myRawObject,1),NA)


})
test_that("createRDBESEstObject can create an object from an empty H1 data extract
          with no warnings or errors",  {

  myRawObject <- createRDBESRawObject()

  myEstObject <- expect_warning(createRDBESEstObject(myRawObject,1),NA)
  myEstObject <- expect_error(createRDBESEstObject(myRawObject,1),NA)


})
test_that("createRDBESEstObject can create an object from an H1 data extract with sub-sampling
          with no warnings or errors",  {

  myPath <- ".\\h1_v_1_19"
  myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)

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


})
test_that("createRDBESEstObject fails when an invalid hierarchy is requested",  {

            myPath <- ".\\h1_v_1_19"
            myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)
            myEstObject <- expect_error(createRDBESEstObject(myRawObject,99),"An invalid value was used for the 'hierarchyToUse' parameter - createRDBESEstObject will not proceed")


})
