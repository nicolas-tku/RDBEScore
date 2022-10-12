

# Trying to re-create issue #27

library(icesRDBES)

path_in <- "Q:/mynd/kibi/RDBES/WGRDBES-EST/dnk_data/"


cl <-
  icesRDBES::importRDBESDownloadData(paste0(path_in, "HCL_2022_10_12_085635.zip"),
                                     castToCorrectDataTypes = T)
cl <-
  icesRDBES::importRDBESDownloadData(paste0(path_in, "HCL_2022_10_12_085635.zip"),
                                     castToCorrectDataTypes = F)

icesRDBES::validateRDBESDataObject(objectToCheck = cl, verbose = T)

# Do not assign to a new object, then you just have a empty value called the -> issue raised by Nick

# cl <- icesRDBES::validateRDBESDataObject(objectToCheck = cl, verbose = T)

unique(cl[["CL"]]$CLeconZone)

# Read in CE and test

ce <-
  icesRDBES::importRDBESDownloadData(paste0(path_in, "HCE_2022_10_12_085803.zip"),
                                     castToCorrectDataTypes = T)

dat <- ce[["CE"]]

# I get some 'NAs introduced by coercion to integer range.
# It would be nice to know where, but since this is checked in the upload I assume this is correct

ce <-
  icesRDBES::importRDBESDownloadData(paste0(path_in, "HCE_2022_10_12_085803.zip"),
                                     castToCorrectDataTypes = F)
as.integer(ce[["CE"]]$CEgTFishHour)
as.numeric(ce[["CE"]]$CEgTFishHour)
as.double(ce[["CE"]]$CEgTFishHour)


as.double(999999999999)
as.double(-999999999999)
as.double(-999999999999)

icesRDBES::validateRDBESDataObject(objectToCheck = ce, verbose = T)

dat <- ce[["CE"]]

str(dat)

, 7, 8, 9, 26, 31, 32, 37:49

for (i in c(45)) {
  print(i)
  as.integer(dat[[i]])
}
