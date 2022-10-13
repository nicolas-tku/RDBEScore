capture.output({  ## suppresses printing of console output when running test()

test_that("doEstimationForAllStrata can run on an object from an H1 data extract
          without warnings or errors",  {

  myPath <- "./h1_v_1_19_13"
  myH1RawObject <-
     createRDBESDataObject(rdbesExtractPath = myPath)

  # Update our test data with some random sample measurements
  myH1RawObject[["SA"]]$SAsampWtLive <-
    round(runif(n = nrow(myH1RawObject[["SA"]]), min = 1, max = 100))

  expect_warning(doEstimationForAllStrata(
                                        RDBESDataObjectForEstim = myH1RawObject,
                                        hierarchyToUse = 1,
                                        verbose = FALSE),
                                      NA)

  expect_error(doEstimationForAllStrata(
                                        RDBESDataObjectForEstim = myH1RawObject,
                                        hierarchyToUse = 1,
                                        verbose = FALSE),
                                    NA)


})

}) ## end capture.output
