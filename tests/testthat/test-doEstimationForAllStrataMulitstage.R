capture.output({  ## suppresses printing of console output when running test()

test_that("doEstimationForAllStrata creates get correct results for Survey apiclust2 mulitstage",  {

  myTestData <- RDBEScore::Pckg_Survey_apiclust2_asH1 #this is valid H1 RDBESDataObj

    # create an est object
  myEstData <- createRDBESEstObject(myTestData, 1, "SA" )

  # Run estimation
  myResults <- doEstimationForAllStrata(myEstData, "SAsampWtMes")

  # Get the results for the VS strata
  #NC_actual <-  myResults[myResults$recType == "VS" & myResults$stratumName == "NC" ,c("stratumName","est.total","est.mean", "se.total","se.mean")]
  #NE_actual <-  myResults[myResults$recType == "VS" & myResults$stratumName == "NE" ,c("stratumName","est.total","est.mean", "se.total","se.mean")]
  #S_actual <-  myResults[myResults$recType == "VS" & myResults$stratumName == "S" ,c("stratumName","est.total","est.mean", "se.total","se.mean")]
  #W_actual <-  myResults[myResults$recType == "VS" & myResults$stratumName == "W" ,c("stratumName","est.total","est.mean", "se.total","se.mean")]

  # Check if the results are correct

  # NC stratum
  #expect_equal(round(NC_actual$est.total,0), 316731380)
  #expect_equal(round(NC_actual$se.total,0), 16977399)
  #expect_equal(round(NC_actual$est.mean,2), 300504.16)
  #expect_equal(round(NC_actual$se.mean,2), 16107.59)

  # NE stratum
  #expect_equal(round(NE_actual$est.total,0), 21478558)
  #expect_equal(round(NE_actual$se.total,0), 3992889)
  #expect_equal(round(NE_actual$est.mean,2), 97629.81)
  #expect_equal(round(NE_actual$se.mean,2), 18149.49)

  # S stratum
  #expect_equal(round(S_actual$est.total,0), 292037391)
  #expect_equal(round(S_actual$se.total,0), 26154840)
  #expect_equal(round(S_actual$est.mean,2), 211315.04)
  #expect_equal(round(S_actual$se.mean,2), 18925.35)

  # W stratum
  #expect_equal(round(W_actual$est.total,0), 279488706)
  #expect_equal(round(W_actual$se.total,0), 39416342)
  #expect_equal(round(W_actual$est.mean,2), 662295.51)
  #expect_equal(round(W_actual$se.mean,2), 93403.65)


})

}) ## end capture.output
