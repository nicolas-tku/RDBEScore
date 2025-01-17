library(openxlsx)

rdbesDataModel <- "./data-raw/dataFormat/RDBES Data Model CS.xlsx"
rdbesDataModelVDSL <- "./data-raw/dataFormat/RDBES Data Model VD SL.xlsx"
rdbesDataModelCLCE <- "./data-raw/dataFormat/RDBES Data Model CL CE.xlsx"

outFile <- "./data/mapColNamesFieldR.rds"

mapColNames <-NULL

for (i in 2:14) {

  dat_0 <- read.xlsx(rdbesDataModel, sheet = i)

  tablePrefix <- NA
  if(length(dat_0)>0){
    tablePrefix <- substr(dat_0[1,"Field.Name"],1,2)
  }
  dat_0$Table.Prefix <- tablePrefix

  mapColNames <-
    rbind(mapColNames, dat_0[,c("Table.Prefix","Field.Name", "R.Name","Type")])
}


for (i in 1:2) {
  dat_1 <- read.xlsx(rdbesDataModelVDSL, sheet = i)

  tablePrefix <- NA
  if(length(dat_0)>0){
    tablePrefix <- substr(dat_1[1,"Field.Name"],1,2)
  }
  dat_1$Table.Prefix <- tablePrefix

  mapColNames <-
    rbind(mapColNames, dat_1[,c("Table.Prefix","Field.Name", "R.Name","Type")])
}

for (i in 1:2) {
  dat_2 <- read.xlsx(rdbesDataModelCLCE, sheet = i)

  tablePrefix <- NA
  if(length(dat_0)>0){
    tablePrefix <- substr(dat_2[1,"Field.Name"],1,2)
  }
  dat_2$Table.Prefix <- tablePrefix

  mapColNames <-
    rbind(mapColNames, dat_2[,c("Table.Prefix","Field.Name", "R.Name","Type")])
}

# Get rid of NA field names
mapColNamesFieldR <- mapColNames[!is.na(mapColNames$Field.Name),]

# Fix for two issues
mapColNamesFieldR[mapColNamesFieldR$R.Name == "Clid","Field.Name"] <- "CLid"
mapColNamesFieldR[mapColNamesFieldR$R.Name == "Clid","R.Name"] <- "CLid"

# Determine which R data type each field should be
mapColNamesFieldR$RDataType <- NA

# Set xxID fields to integer data type
mapColNamesFieldR[
  grepl("^..id$",mapColNamesFieldR$Field.Name, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Field.Name) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "integer"

# Set integer fields to integer data type
mapColNamesFieldR[
  grepl("Int",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "integer"

# Set decimal fields to numeric data type
mapColNamesFieldR[
  grepl("Dec",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "numeric"

# Set string fields to character data type
mapColNamesFieldR[
  grepl("Str",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "character"

# Set Date fields to character data type
mapColNamesFieldR[
  grepl("Date",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "character"

# Set Time fields to character data type
mapColNamesFieldR[
  grepl("Time",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "character"

# Set Y/N fields to character data type
mapColNamesFieldR[
  grepl("Y/N",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "character"

# Change SAid and SAseqNum to be numeric rather than integer -
# this is required when we generate true zeros
mapColNamesFieldR[
mapColNamesFieldR$Table.Prefix == "SA" &
  mapColNamesFieldR$Field.Name == "SAid","RDataType"] <- "numeric"
mapColNamesFieldR[
  mapColNamesFieldR$Table.Prefix == "SA" &
    mapColNamesFieldR$Field.Name == "SAsequenceNumber","RDataType"] <- "numeric"
mapColNamesFieldR[
  mapColNamesFieldR$Table.Prefix == "SA" &
    mapColNamesFieldR$Field.Name == "SAparentSequenceNumber","RDataType"] <- "numeric"

# Save the data
usethis::use_data(mapColNamesFieldR)
