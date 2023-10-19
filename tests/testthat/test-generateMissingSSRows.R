#capture.output({ ## suppresses printing of console output when running test()


  test_that("generateMissingSSRows does not add any SS rows if none are missing (H1)", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    # Change all FO rows to just be Lan (to match SS which just has Lan)
    myH1RawObject[["FO"]]$FOcatReg <- "Lan"

    # Try to generate any missing SS rows - there shouldn't be any missing
    mySSAfter <- generateMissingSSRows(myH1RawObject,
                                           "ZW_1965_SpeciesList",
                                           verbose = FALSE)
    SSbefore <- nrow(myH1RawObject[["SS"]])
    SSafter <- nrow(mySSAfter)

    expect_equal(SSafter,SSbefore)
  })

  test_that("generateMissingSSRows produces an error if there is no SL data", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    tableToRemove <- "SL"
    # Get rid of data
    myH1RawObject <- myH1RawObject[!(sapply(myH1RawObject, is.null))]
    myDFs <- lapply(myH1RawObject, as.data.frame)
    myDFs[[tableToRemove]] <- NULL
    myH1RawObject2 <- suppressWarnings(createRDBESDataObject(myDFs, castToCorrectDataTypes = FALSE))

    expect_error(generateMissingSSRows(myH1RawObject2,
                                       "ZW_1965_SpeciesList",
                                       verbose = FALSE),
                 paste0(tableToRemove," data does not exist in the input data but it is required"))
  })

  test_that("generateMissingSSRows produces an error if there is no SS data", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    tableToRemove <- "SS"
    # Get rid of data
    myH1RawObject <- myH1RawObject[!(sapply(myH1RawObject, is.null))]
    myDFs <- lapply(myH1RawObject, as.data.frame)
    myDFs[[tableToRemove]] <- NULL
    myH1RawObject2 <- suppressWarnings(createRDBESDataObject(myDFs, castToCorrectDataTypes = FALSE))

    expect_error(generateMissingSSRows(myH1RawObject2,
                                       "ZW_1965_SpeciesList",
                                       verbose = FALSE),
                 paste0(tableToRemove," data does not exist in the input data but it is required"))
  })

  test_that("generateMissingSSRows produces an error if there is no FO data", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    tableToRemove <- "FO"
    # Get rid of data
    myH1RawObject <- myH1RawObject[!(sapply(myH1RawObject, is.null))]
    myDFs <- lapply(myH1RawObject, as.data.frame)
    myDFs[[tableToRemove]] <- NULL
    myH1RawObject2 <- suppressWarnings(createRDBESDataObject(myDFs, castToCorrectDataTypes = FALSE))

    expect_error(generateMissingSSRows(myH1RawObject2,
                                       "ZW_1965_SpeciesList",
                                       verbose = FALSE),
                 paste0(tableToRemove," data does not exist in the input data but it is required"))
  })

  test_that("generateMissingSSRows produces an error if a species list names that does not exist is provided", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    expect_error(generateMissingSSRows(myH1RawObject,
                                       "Stupid name",
                                       verbose = FALSE),
                 "The requested species list name does not exist in the input data")
  })

  test_that("generateMissingSSRows runs correctly for FO Dis, when there are no missing SS Dis rows (H1)", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    # Set FO to Dis
    myH1RawObject[["FO"]]$FOcatReg <- "Dis"
    # Ensure only SS Dis rows exists
    myH1RawObject[["SS"]]$SScatchFra <- "Dis"

    # Try to generate any missing SS rows
    mySSAfter <- generateMissingSSRows(myH1RawObject,
                                       "ZW_1965_SpeciesList",
                                       verbose = FALSE)
    SSbefore <- nrow(myH1RawObject[["SS"]])
    catchFracBefore <- sort(unique(myH1RawObject[["SS"]]$SScatchFra))
    SSafter <- nrow(mySSAfter)
    catchFracAfter <- sort(unique(mySSAfter$SScatchFra))

    expect_equal(SSafter,SSbefore)
    expect_equal(catchFracAfter,catchFracBefore)
  })

  test_that("generateMissingSSRows runs correctly for FO Dis, generates missing SS Dis rows (H1)", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    # Set FO to Dis
    myH1RawObject[["FO"]]$FOcatReg <- "Dis"
    # Ensure only SS Lan rows exists
    myH1RawObject[["SS"]]$SScatchFra <- "Lan"

    # Try to generate any missing SS rows
    # All FO rows now have catReg "Dis", but SS only has Lan rows - so we expect Dis
    # rows to be added to SS
    mySSAfter <- generateMissingSSRows(myH1RawObject,
                                       "ZW_1965_SpeciesList",
                                       verbose = FALSE)
    SSbefore <- nrow(myH1RawObject[["SS"]])
    catchFracBefore <- sort(unique(myH1RawObject[["SS"]]$SScatchFra))
    SSafter <- nrow(mySSAfter)
    catchFracAfter <- sort(unique(mySSAfter$SScatchFra))

    # We expect the number of SS rows to double
    expect_equal(SSafter,2*SSbefore)
    # We expect to have Dis rows added to SS
    expect_equal(catchFracAfter,c("Dis",catchFracBefore))
  })

  test_that("generateMissingSSRows runs correctly for FO Lan, when there are no missing SS Lan rows (H1)", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    # Set FO to Lan
    myH1RawObject[["FO"]]$FOcatReg <- "Lan"
    # Ensure only SS Lan rows exists
    myH1RawObject[["SS"]]$SScatchFra <- "Lan"

    # Try to generate any missing SS rows
    mySSAfter <- generateMissingSSRows(myH1RawObject,
                                       "ZW_1965_SpeciesList",
                                       verbose = FALSE)
    SSbefore <- nrow(myH1RawObject[["SS"]])
    catchFracBefore <- sort(unique(myH1RawObject[["SS"]]$SScatchFra))
    SSafter <- nrow(mySSAfter)
    catchFracAfter <- sort(unique(mySSAfter$SScatchFra))

    expect_equal(SSafter,SSbefore)
    expect_equal(catchFracAfter,catchFracBefore)
  })

  test_that("generateMissingSSRows runs correctly for FO Lan, generates missing SS Lan rows (H1)", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    # Set FO to Lan
    myH1RawObject[["FO"]]$FOcatReg <- "Lan"
    # Ensure only SS Dis rows exists
    myH1RawObject[["SS"]]$SScatchFra <- "Dis"

    # Try to generate any missing SS rows
    mySSAfter <- generateMissingSSRows(myH1RawObject,
                                       "ZW_1965_SpeciesList",
                                       verbose = FALSE)
    SSbefore <- nrow(myH1RawObject[["SS"]])
    catchFracBefore <- sort(unique(myH1RawObject[["SS"]]$SScatchFra))
    SSafter <- nrow(mySSAfter)
    catchFracAfter <- sort(unique(mySSAfter$SScatchFra))

    # We expect the number of SS rows to double
    expect_equal(SSafter,2*SSbefore)
    # We expect to have Lan rows added to SS
    expect_equal(catchFracAfter,c(catchFracBefore,"Lan"))
  })

  test_that("generateMissingSSRows runs correctly for FO All, generates missing SS Lan rows (H1)", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    # Set FO to Lan
    myH1RawObject[["FO"]]$FOcatReg <- "All"
    # Ensure only SS Dis rows exists
    myH1RawObject[["SS"]]$SScatchFra <- "Dis"

    # Try to generate any missing SS rows
    mySSAfter <- generateMissingSSRows(myH1RawObject,
                                       "ZW_1965_SpeciesList",
                                       verbose = FALSE)
    SSbefore <- nrow(myH1RawObject[["SS"]])
    catchFracBefore <- sort(unique(myH1RawObject[["SS"]]$SScatchFra))
    SSafter <- nrow(mySSAfter)
    catchFracAfter <- sort(unique(mySSAfter$SScatchFra))

    # We expect the number of SS rows to double
    expect_equal(SSafter,2*SSbefore)
    # We expect to have Lan rows added to SS
    expect_equal(catchFracAfter,c(catchFracBefore,"Lan"))
  })

  test_that("generateMissingSSRows runs correctly for FO All, generates missing SS Dis rows (H1)", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    # Set FO to Lan
    myH1RawObject[["FO"]]$FOcatReg <- "All"
    # Ensure only SS Dis rows exists
    myH1RawObject[["SS"]]$SScatchFra <- "Lan"

    # Try to generate any missing SS rows
    mySSAfter <- generateMissingSSRows(myH1RawObject,
                                       "ZW_1965_SpeciesList",
                                       verbose = FALSE)
    SSbefore <- nrow(myH1RawObject[["SS"]])
    catchFracBefore <- sort(unique(myH1RawObject[["SS"]]$SScatchFra))
    SSafter <- nrow(mySSAfter)
    catchFracAfter <- sort(unique(mySSAfter$SScatchFra))

    # We expect the number of SS rows to double
    expect_equal(SSafter,2*SSbefore)
    # We expect to have Lan rows added to SS
    expect_equal(catchFracAfter,c("Dis",catchFracBefore))
  })

  test_that("generateMissingSSRows runs correctly for FO All, when there are no missing SS Catch rows (H1)", {

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
    # Only use a subset of the test data
    myH1RawObject <- filterRDBESDataObject(myH1RawObject, c("DEstratumName"), c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1"))
    myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

    # Set FO to Lan
    myH1RawObject[["FO"]]$FOcatReg <- "All"
    # Ensure only SS Dis rows exists
    myH1RawObject[["SS"]]$SScatchFra <- "Catch"

    # Try to generate any missing SS rows
    mySSAfter <- generateMissingSSRows(myH1RawObject,
                                       "ZW_1965_SpeciesList",
                                       verbose = FALSE)
    SSbefore <- nrow(myH1RawObject[["SS"]])
    catchFracBefore <- sort(unique(myH1RawObject[["SS"]]$SScatchFra))
    SSafter <- nrow(mySSAfter)
    catchFracAfter <- sort(unique(mySSAfter$SScatchFra))

    expect_equal(SSafter,SSbefore)
    expect_equal(catchFracAfter,catchFracBefore)
  })


#}) ## end capture.output
