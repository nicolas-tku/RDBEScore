## Load the csv files from our data-raw
ddir <- "./data-raw/exampleData/"
outputDir <- "./tests/testthat/h1_v_1_19_18/"

H1_2023_10_16 <- createRDBESDataObject(paste0(ddir, "H1_2023_10_16.zip"),
                                       castToCorrectDataTypes = TRUE)
validateRDBESDataObject(H1_2023_10_16)

saveRDS(H1_2023_10_16, file=paste0(outputDir,"H1_2023_10_16.rds"))


