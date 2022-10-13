## Load the csv files from our data-raw
ddir <- "./data-raw/exampleData/"

unzip(zipfile = paste0(ddir, "exampleData.zip"), exdir = paste0(ddir, "H1"))

h1Example <- list()
h1Example[["DE"]] <- read.csv(paste0(ddir, "H1/H1_DE.csv"), stringsAsFactors = FALSE)
h1Example[["SD"]] <- read.csv(paste0(ddir, "H1/H1_SD.csv"), stringsAsFactors = FALSE)
h1Example[["VS"]] <- read.csv(paste0(ddir, "H1/H1_VS.csv"), stringsAsFactors = FALSE)
h1Example[["FT"]] <- read.csv(paste0(ddir, "H1/H1_FT.csv"), stringsAsFactors = FALSE)
h1Example[["FO"]] <- read.csv(paste0(ddir, "H1/H1_FO.csv"), stringsAsFactors = FALSE)
h1Example[["SS"]] <- read.csv(paste0(ddir, "H1/H1_SS.csv"), stringsAsFactors = FALSE)
h1Example[["SA"]] <- read.csv(paste0(ddir, "H1/H1_SA.csv"), stringsAsFactors = FALSE)
h1Example[["FM"]] <- read.csv(paste0(ddir, "H1/H1_FM.csv"), stringsAsFactors = FALSE)
h1Example[["BV"]] <- read.csv(paste0(ddir, "H1/H1_BV.csv"), stringsAsFactors = FALSE)
h1Example[["SL"]] <- read.csv(paste0(ddir, "H1/H1_SL.csv"), stringsAsFactors = FALSE)
h1Example[["VD"]] <- read.csv(paste0(ddir, "H1/H1_VD.csv"), stringsAsFactors = FALSE)

usethis::use_data(h1Example, overwrite = TRUE)
