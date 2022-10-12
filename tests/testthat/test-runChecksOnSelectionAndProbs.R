capture.output({  ## suppresses printing of console output when running test()

test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present",  {

  myPath <- "./h1_v_1_19_13"
  myObject <- createRDBESDataObject(rdbesExtractPath = myPath)
  myObject[["SA"]]$SAlowHierarchy <- "D"
  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})
  test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present - different numTotal for different strata",  {

    myPath <- "./h1_v_1_19_13"
    myObject <- createRDBESDataObject(rdbesExtractPath = myPath)
    myObject[["SA"]]$SAlowHierarchy <- "D"
    myObject[["VS"]][myObject[["VS"]]$VSstratumName == "VS_stratum2","VSnumTotal"] <- 40
    expect_error(runChecksOnSelectionAndProbs(myObject),NA)

  })
test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present - different numSampled for different strata",  {

  myPath <- "./h1_v_1_19_13"
  myObject <- createRDBESDataObject(rdbesExtractPath = myPath)
  myObject[["SA"]]$SAlowHierarchy <- "D"
  myObject[["VS"]][myObject[["VS"]]$VSstratumName == "VS_stratum2","VSnumSamp"] <- 2
  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})

})
