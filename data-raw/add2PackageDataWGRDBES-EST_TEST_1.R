#read the downloaded zip file

zipFname <- "./data-raw/exampleData/WGRDBES-EST_TEST_1/H1_2023_10_17_094101.zip"
data <- RDBEScore:::importRDBESDataZIP(zipFname)

#keep only the selected sampling scheme
samp_scheme <-"WGRDBES-EST TEST 1"
data <- RDBEScore::filterRDBESDataObject(data,
                                         fieldsToFilter = "DEsampScheme",
                                         valuesToFilter = samp_scheme,
                                         killOrphans = T)
datasetNames <- unique(data$DE$DEstratumName)

#add all stratums to package data
for(dname in datasetNames){
  datasetName <- gsub(" ", "_",paste0(samp_scheme,"_", dname))
  deData <-  RDBEScore::filterRDBESDataObject(data,
                                              fieldsToFilter = "DEstratumName",
                                              valuesToFilter = dname,
                                              killOrphans = T)
  assign(datasetName, deData)
  do.call("use_data", list(as.name(datasetName), overwrite = TRUE))
}
