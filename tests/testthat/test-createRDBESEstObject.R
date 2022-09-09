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
test_that("createRDBESEstObject can correctly create an object when there is no BV data",  {

    myPath <- ".\\h1_v_1_19"
    myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)

    # Filter the object
    myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "DEid", valuesToFilter = 396)
    myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "VSid", valuesToFilter = 4874)
    myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "FTunitName", valuesToFilter = "FT_unit_1")
    myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "FOunitName", valuesToFilter = "FO_unit_1")
    myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "SSid", valuesToFilter = 29877)
    myRawObject <- findAndKillOrphans(myRawObject)

    # get rid of BV data
    myRawObject[["SA"]]$SAlowHierarchy <- "B"
    myRawObject["BV"] <- list(NULL)

    myEstObject <- createRDBESEstObject(myRawObject, hierarchyToUse = 1)
    expect_equal(nrow(myEstObject),20)

})
test_that("createRDBESEstObject can correctly create an object when there is no FM data",  {

  myPath <- ".\\h1_v_1_19"
  myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)

  # Filter the object
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "DEid", valuesToFilter = 396)
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "VSid", valuesToFilter = 4874)
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "FTunitName", valuesToFilter = "FT_unit_1")
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "FOunitName", valuesToFilter = "FO_unit_1")
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "SSid", valuesToFilter = 29877)
  myRawObject <- findAndKillOrphans(myRawObject)

  # get rid of FM data
  myRawObject[["SA"]]$SAlowHierarchy <- "C"
  myRawObject["FM"] <- list(NULL)

  myRawObject[["BV"]][1:20,"SAid"]<-33933
  myRawObject[["BV"]][21:40,"SAid"]<-34908
  myRawObject[["BV"]]$FMid <- NA

  myEstObject <- createRDBESEstObject(myRawObject, hierarchyToUse = 1)
  expect_equal(nrow(myEstObject),40)

})
test_that("createRDBESEstObject can correctly create an object when there is no FM or BV data",  {

  myPath <- ".\\h1_v_1_19"
  myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)

  # Filter the object
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "DEid", valuesToFilter = 396)
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "VSid", valuesToFilter = 4874)
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "FTunitName", valuesToFilter = "FT_unit_1")
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "FOunitName", valuesToFilter = "FO_unit_1")
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "SSid", valuesToFilter = 29877)
  myRawObject <- findAndKillOrphans(myRawObject)

  # get rid of FM and BV data
  myRawObject[["SA"]]$SAlowHierarchy <- "D"
  myRawObject["FM"] <- list(NULL)
  myRawObject["BV"] <- list(NULL)

  myEstObject <- createRDBESEstObject(myRawObject, hierarchyToUse = 1)
  expect_equal(nrow(myEstObject),2)

})
test_that("createRDBESEstObject creates the correct number of rows when there is sub-sampling present (1)",  {

  myPath <- ".\\h1_v_1_19"
  myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)

  # Filter the object
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "DEid", valuesToFilter = 396)
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "VSid", valuesToFilter = 4874)
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "FTunitName", valuesToFilter = "FT_unit_1")
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "FOunitName", valuesToFilter = "FO_unit_1")
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "SSid", valuesToFilter = 29877)
  myRawObject <- findAndKillOrphans(myRawObject)

  # Introduce sub-sampling - only the lowest level has data
  myRawObject[["SA"]]$SAsamp <- "N"
  myRawObject[["SA"]]$SAlowHierarchy <- "D"
  newSARows <- data.table::copy(myRawObject[["SA"]])
  newSARows$SAparentID <- newSARows$SAid
  newSARows$SAid <- newSARows$SAid +1
  newSARows2 <- data.table::copy(newSARows)
  newSARows2$SAparentID <- newSARows2$SAid
  newSARows2$SAid <- newSARows2$SAid +1
  newSARows2$SAsamp <- "Y"
  newSARows2$SAlowHierarchy <- "B"

  myRawObject["BV"] <- list(NULL)
  myRawObject[["SA"]]<-rbind(myRawObject[["SA"]],newSARows,newSARows2)

  myRawObject[["FM"]][myRawObject[["FM"]]$SAid == 33933,"SAid"] <- 33935
  myRawObject[["FM"]][myRawObject[["FM"]]$SAid == 34908,"SAid"] <- 34910

  myEstObject <- createRDBESEstObject(myRawObject, hierarchyToUse = 1)
  expect_equal(nrow(myEstObject),20)

})
test_that("createRDBESEstObject creates the correct number of rows when there is sub-sampling present (2)",  {

  myPath <- ".\\h1_v_1_19"
  myRawObject <- createRDBESRawObject(rdbesExtractPath = myPath)

  # Filter the object
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "DEid", valuesToFilter = 396)
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "VSid", valuesToFilter = 4874)
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "FTunitName", valuesToFilter = "FT_unit_1")
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "FOunitName", valuesToFilter = "FO_unit_1")
  myRawObject <- filterRDBESRawObject(myRawObject, fieldsToFilter = "SSid", valuesToFilter = 29877)
  myRawObject <- findAndKillOrphans(myRawObject)

  # Introduce sub-sampling - the upper and lowest level of sampling have data
  myRawObject[["SA"]]$SAsamp <- "N"
  myRawObject[["SA"]]$SAlowHierarchy <- "D"
  newSARows <- data.table::copy(myRawObject[["SA"]])
  newSARows$SAparentID <- newSARows$SAid
  newSARows$SAid <- newSARows$SAid +1
  newSARows2 <- data.table::copy(newSARows)
  newSARows2$SAparentID <- newSARows2$SAid
  newSARows2$SAid <- newSARows2$SAid +1
  newSARows2[1,"SAsamp"] <- "Y"
  newSARows2[1,"SAlowHierarchy"] <- "B"
  myRawObject[["SA"]][2,"SAsamp"] <- "Y"
  myRawObject[["SA"]][2,"SAlowHierarchy"] <- "B"

  myRawObject["BV"] <- list(NULL)
  myRawObject[["SA"]]<-rbind(myRawObject[["SA"]],newSARows,newSARows2)

  myRawObject[["FM"]][myRawObject[["FM"]]$SAid == 33933,"SAid"] <- 33935

  myEstObject <- createRDBESEstObject(myRawObject, hierarchyToUse = 1)
  expect_equal(nrow(myEstObject),21)

})
