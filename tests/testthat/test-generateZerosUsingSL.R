capture.output({  ## suppresses printing of console output when running test()

test_that("generateZerosUsingSL creates rows for SLcou*SLinst*SLspeclistName*SLyear*SLcatchFrac*SLcommTaxon", {


# species*catchFrac in SL and not in SA: expected behavior -> generate a 0 row in SA

	# create test data from download
	myH1DataObject <- importRDBESDownloadData("./h1_v_1_19_18/WGRDBES-EST_TEST_Pckg_Survey_data_apistrat_H1.zip")
	validateRDBESDataObject(myH1DataObject, checkDataTypes = TRUE)

	df1 <- data.frame('31830','SL','ZW','4484','WGRDBES-EST_TEST_Pckg_Survey_data_apistrat_H1','1968','Dis','107254','107254')
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


	myH1DataObject <- filterRDBESDataObject(myH1DataObject, c("SAid"), c(653280), killOrphans = TRUE)
	validateRDBESDataObject(myH1DataObject, checkDataTypes = TRUE)

	  # check generateZerosUsingSL is creating missing catchCateg in SA
		myTest3 <- generateZerosUsingSL(myH1DataObject)
	  # create aux id_table [Nuno's function] and tmpKey to use in test
		aux<-createTableOfRDBESIds(x = myTest3, addSAseqNums=FALSE)
		myTest3$SA$SDctry<-myTest3$SD$SDctry[match(aux$SDid[match(myTest3$SA$SAid,aux$SAid)], myTest3$SD$SDid)]
		myTest3$SA$SDinst <- myTest3$SD$SDinst[match(aux$SDid[match(myTest3$SA$SAid,aux$SAid)], myTest3$SD$SDid)]
		myTest3$SA$SSspecListName <- myTest3$SS$SSspecListName[match(aux$SSid[match(myTest3$SA$SAid,aux$SAid)], myTest3$SS$SSid)]
		myTest3$SA$DEyear <- myTest3$DE$DEyear[match(aux$DEid[match(myTest3$SA$SAid,aux$SAid)], myTest3$DE$DEid)]
		myTest3$SA[ ,tmpKey := paste(SDctry, SDinst, SSspecListName, DEyear, SAcatchCat, SAspeCode)]

	#run tests
	expect_equal(nrow(myTest3$SA[tmpKey %in% "ZW 4484 WGRDBES-EST_TEST_Pckg_Survey_data_apistrat_H1 1968 Dis 107254",]),1)
	expect_equal(nrow(myTest3$SA),2)


# species*catchFrac in SL and in SA: expected behavior -> do not generate a 0 row in SA

	myH1DataObject <- importRDBESDownloadData("./h1_v_1_19_18/WGRDBES-EST_TEST_Pckg_Survey_data_apistrat_H1.zip")
	myH1DataObject[["SS"]]<-myH1DataObject[["SS"]][1,]
	myH1DataObject <- filterRDBESDataObject(myH1DataObject, c("SAid"), c(653280), killOrphans = TRUE)
	validateRDBESDataObject(myH1DataObject, checkDataTypes = TRUE)

	expect_equal(generateZerosUsingSL(myH1DataObject), myH1DataObject)

})

}) ## end capture.output


