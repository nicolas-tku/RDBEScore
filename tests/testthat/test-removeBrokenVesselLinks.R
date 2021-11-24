test_that("removeBrokenVesselLinks runs without errors
          or warnings when there are no invalid vessel links",  {

  myPath <- ".\\h1_v_1_19"
  myH1RawObject <-
    createRDBESRawObject(rdbesExtractPath = myPath)

  myObjectValidVesselLinks <- expect_warning(
    removeBrokenVesselLinks(objectToCheck = myH1RawObject, verbose = FALSE)
    ,NA)

})
test_that("removeBrokenVesselLinks runs without errors
          or warnings when there are invalid vessel links",  {

  myPath <- ".\\h1_v_1_19"
  myH1RawObject <-
      createRDBESRawObject(rdbesExtractPath = myPath)
  myFields <- c("VDlenCat")
  myValues <- c("18-<24" )
  myFilteredObject <- filterRDBESRawObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )
  myObjectValidVesselLinks <- expect_warning(
    removeBrokenVesselLinks(objectToCheck = myFilteredObject,verbose = FALSE)
    ,NA)

})
