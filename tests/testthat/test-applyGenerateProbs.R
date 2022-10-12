capture.output({  ## suppresses printing of console output when running test()

  test_that("applyGenerateProbs runs without errors when stratification is present",  {

    myPath <- "./h1_v_1_19_13"
    myObject <- createRDBESDataObject(rdbesExtractPath = myPath)
    myObject[["SA"]]$SAlowHierarchy <- "D"
    myObject[["VS"]][myObject[["VS"]]$VSstratumName == "VS_stratum2","VSnumTotal"] <- 40
    expect_error(applyGenerateProbs(myObject, "selection"),NA)

  })

})
