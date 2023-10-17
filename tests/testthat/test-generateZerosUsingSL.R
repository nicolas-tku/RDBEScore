capture.output({  ## suppresses printing of console output when running test()

test_that("generateZerosUsingSL creates rows for SLcou*SLinst*SLspeclistName*SLyear*SLcatchFrac*SLcommTaxon", {

# create test data from download [to be used in different tests]

	myH1DataObject <- RDBEScore:::importRDBESDataZIP("./h1_v_1_19_18/ZW_1965_WGRDBES-EST_TEST_1.zip")

	# Only use a subset of the test data
	myH1DataObject <- filterRDBESDataObject(myH1DataObject,c("DEstratumName"),c("Pckg_survey_apistrat_H1"))
	myH1DataObject <- filterRDBESDataObject(myH1DataObject,c("SLspeclistName"),c("WGRDBES-EST_TEST_1_Pckg_survey_apistrat_H1"))
	myH1DataObject <- findAndKillOrphans(myH1DataObject, verbose = FALSE)

	validateRDBESDataObject(myH1DataObject, checkDataTypes = TRUE)

	df1 <- data.frame('31831','SL','ZW','4484',myH1DataObject[["SL"]]$SLspeclistName,'1965','Dis','107254','107254')


	colnames(df1) <- names(myH1DataObject[["SL"]])
	myH1DataObject[["SL"]] <- rbind(myH1DataObject[["SL"]],df1)
	myH1DataObject[["SL"]]$SLid <- as.integer(myH1DataObject[["SL"]]$SLid)
	myH1DataObject[["SL"]]$SLyear <- as.integer(myH1DataObject[["SL"]]$SLyear)
	myH1DataObject[["SL"]]$SLcommTaxon <- as.integer(myH1DataObject[["SL"]]$SLcommTaxon)
	myH1DataObject[["SL"]]$SLsppCode <- as.integer(myH1DataObject[["SL"]]$SLsppCode)
	# ensure key is set on SL
	setkey(myH1DataObject[["SL"]], SLid)

	myH1DataObject[["SS"]]<-rbind(myH1DataObject[["SS"]][1,],myH1DataObject[["SS"]][1,])
	myH1DataObject[["SS"]]$SScatchFra[2]<-"Dis"
	myH1DataObject[["SS"]]$SSid[2]<-myH1DataObject[["SS"]]$SSid[1]+1
	myH1DataObject[["SS"]]$SSid<-as.integer(myH1DataObject[["SS"]]$SSid)
	# ensure key is set on SS
	setkey(myH1DataObject[["SS"]], SSid)

	myH1DataObject1 <- filterRDBESDataObject(myH1DataObject, c("SAid"), c(572813), killOrphans = TRUE)
	validateRDBESDataObject(myH1DataObject, checkDataTypes = TRUE)
	validateRDBESDataObject(myH1DataObject1, checkDataTypes = TRUE)

# species*catchFrac in SL and not in SA: expected behavior -> generate a 0 row in SA

	  # check generateZerosUsingSL is creating missing row in SA
		myTest3 <- generateZerosUsingSL(myH1DataObject1)

	  # create aux id_table [Nuno's function] and tmpKey to use in test
		aux<-createTableOfRDBESIds(x = myTest3, addSAseqNums=FALSE)

		myTest3$SA$SLid <- myTest3$SS$SLid[match(aux$SSid[match(myTest3$SS$SSid,aux$SSid)], myTest3$SS$SSid)]
		myTest3$SL <- myTest3$SL[myTest3$SL$SLid %in% myTest3$SA$SLid, ]

	#run tests
	# all species in species list are now present in SA
		expect_equal(all(paste0(myTest3$SL$SLid, myTest3$SL$SLcommTaxon) %in% paste0(myTest3$SA$SLid, myTest3$SA$SAspeCode)),TRUE)
	# right number of rows generated
		expect_equal(nrow(myTest3$SA),2)


# species*catchFrac in SL and in SA: expected behavior -> do not generate a 0 row in SA

	myH1DataObject2<-myH1DataObject1
	myH1DataObject2[["SS"]]<-myH1DataObject2[["SS"]][1,]
	myH1DataObject2<-filterRDBESDataObject(myH1DataObject2, c("SLid"), myH1DataObject2[["SS"]]$SLid, killOrphans = TRUE)

	validateRDBESDataObject(myH1DataObject2, checkDataTypes = TRUE)

	result <- generateZerosUsingSL(myH1DataObject2)

	expect_equal(result, myH1DataObject2)

# if SScatchFra=="Catch" it should not generate 0s for any SA strata if spp present in list

	myH1DataObject2<-myH1DataObject1
		myH1DataObject2[["SS"]]<-myH1DataObject2[["SS"]][1,]
		myH1DataObject2<-filterRDBESDataObject(myH1DataObject2, c("SLid"), myH1DataObject2[["SS"]]$SLid, killOrphans = TRUE)
		myH1DataObject2[["SS"]]$SScatchFra<-"Catch"
		myH1DataObject2[["SL"]]$SLcatchFrac<-"Catch"
		# creates stratification in SA
		myH1DataObject2[["SA"]]<-rbind(myH1DataObject2[["SA"]][1,],myH1DataObject2[["SA"]][1,])
		myH1DataObject2[["SA"]]$SAstratification<-"Y"
		myH1DataObject2[["SA"]]$SAid[2]<-myH1DataObject2[["SA"]]$SAid[1]+1
		myH1DataObject2[["SA"]]$SAstratumName<-c("Landings","Discards")
		myH1DataObject2[["SA"]]$SAcatchCat<-c("Lan","Dis")	# this is just cosmetics
		setkey(myH1DataObject2[["SA"]], "SAid")

	validateRDBESDataObject(myH1DataObject2, checkDataTypes = TRUE)

	result <- generateZerosUsingSL(myH1DataObject2)

	expect_equal(result, myH1DataObject2)

# if SScatchFra=="Catch" it should generate 0s for all SA strata present if spp absent(but in SL)

	myH1DataObject2<-myH1DataObject1
		myH1DataObject2[["SS"]]<-myH1DataObject2[["SS"]][1,]
		myH1DataObject2<-filterRDBESDataObject(myH1DataObject2, c("SLid"), myH1DataObject2[["SS"]]$SLid, killOrphans = TRUE)
		myH1DataObject2[["SS"]]$SScatchFra<-"Catch"
		myH1DataObject2[["SL"]]$SLcatchFrac<-"Catch"

		# use in debug
		#myH1DataObject2[c(10,11,15)]

		# creates stratification in SA
		myH1DataObject2[["SA"]]<-rbind(myH1DataObject2[["SA"]][1,],myH1DataObject2[["SA"]][1,])
		myH1DataObject2[["SA"]]$SAstratification<-"Y"
		myH1DataObject2[["SA"]]$SAid[2]<-myH1DataObject2[["SA"]]$SAid[1]+1
		myH1DataObject2[["SA"]]$SAstratumName<-c("Landings","Discards")
		myH1DataObject2[["SA"]]$SAcatchCat<-c("Lan","Dis")	# this is just cosmetics
		setkey(myH1DataObject2[["SA"]], "SAid")
		# creates a spp not existing in SA
		myH1DataObject2[["SL"]]$SLcommTaxon<-as.integer(127007)

	validateRDBESDataObject(myH1DataObject2, checkDataTypes = TRUE)

	result <- generateZerosUsingSL(myH1DataObject2)

	expect_equal(nrow(result$SA),4)

# issue error if >1 SAcatchCat OR SAsex OR SAlandCat in same SSid*SAstratumName
	# explanation:
		# in a normal situation these variables are filled in the new from the 1st row of SSid*SAstratumName
		# if it happens that there are >1 values in that SSid*SAstratumName this is not valid - an error is issued
	myH1DataObject2<-myH1DataObject
	myH1DataObject2$SA$SSid[1]<-myH1DataObject2$SA$SSid[2]
	myH1DataObject2$SA$SAcatchCat[1]<-"Dis"
	expect_error(generateZerosUsingSL(myH1DataObject2))

	myH1DataObject2<-myH1DataObject
	myH1DataObject2$SA$SSid[1]<-myH1DataObject2$SA$SSid[2]
	myH1DataObject2$SA$SAsex[1]<-"F"
	expect_error(generateZerosUsingSL(myH1DataObject2))

	myH1DataObject2<-myH1DataObject
	myH1DataObject2$SA$SSid[1]<-myH1DataObject2$SA$SSid[2]
	myH1DataObject2$SA$SAlandCat[1]<-"HuC"
	expect_error(generateZerosUsingSL(myH1DataObject2))


})

}) ## end capture.output


