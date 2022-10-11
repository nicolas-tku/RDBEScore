test_that("doEstimationForAllStrata can run on an object from an H1 data extract
          without warnings or errors",  {

  myPath <- "./h1_v_1_19_13"
  myH1RawObject <-
     createRDBESRawObject(rdbesExtractPath = myPath)

  # Update our test data with some random sample measurements
  myH1RawObject[["SA"]]$SAsampWtLive <-
    round(runif(n = nrow(myH1RawObject[["SA"]]), min = 1, max = 100))

  expect_warning(doEstimationForAllStrata(
                                        rdbesRawObjectForEstim = myH1RawObject,
                                        hierarchyToUse = 1,
                                        verbose = FALSE),
                                      NA)

  expect_error(doEstimationForAllStrata(
                                        rdbesRawObjectForEstim = myH1RawObject,
                                        hierarchyToUse = 1,
                                        verbose = FALSE),
                                    NA)


})
