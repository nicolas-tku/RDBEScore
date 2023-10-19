#' Generate zeros in samples using Species List information
#'
#' examples for now see
#' https://github.com/ices-eg/WK_RDBES/tree/master/WKRDB-EST2/chairs/Nuno
#'
#' @param x RDBES data frame
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return RDBES data frame where SA was complemented with species looked for
#' (sensu in sampling objectives) but not registered in sample
#' @export
#'


generateZerosUsingSL <- function(x,
                                 verbose = FALSE,
                                 strict = TRUE) {

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(x, verbose = verbose, strict = strict)

  if (!(nrow(x[["SA"]]) >= 1 && nrow(x[["SL"]]) >= 1)) stop("no SA and/or SL")

  # Take a copy of SA and SL since we'll change some column data types and
  # we don't want to update the original version
  tmpSA <- data.table::copy(x[["SA"]])
  tmpSL <- data.table::copy(x[["SL"]])
  # Now convert some columns from int to numeric
  colsToConvertToNumeric <- c("SAid", "SAseqNum")
  tmpSA[, (colsToConvertToNumeric) := lapply(.SD, as.double),
        .SDcols = colsToConvertToNumeric]

# create aux id_table
	aux<-createTableOfRDBESIds(x = x, addSAseqNums=FALSE)

	tmpSA$SDctry <- x$SD$SDctry[match(aux$SDid[match(tmpSA$SAid,aux$SAid)], x$SD$SDid)]
	tmpSA$SDinst <- x$SD$SDinst[match(aux$SDid[match(tmpSA$SAid,aux$SAid)], x$SD$SDid)]
	tmpSA$SSspecListName <- x$SS$SSspecListName[match(aux$SSid[match(tmpSA$SAid,aux$SAid)], x$SS$SSid)]
	tmpSA$DEyear <- x$DE$DEyear[match(aux$DEid[match(tmpSA$SAid,aux$SAid)], x$DE$DEid)]
	tmpSA$SScatchFra <- x$SS$SScatchFra[match(aux$SSid[match(tmpSA$SAid,aux$SAid)], x$SS$SSid)]
	tmpSA$SLid<-x$SS$SLid[match(aux$SSid[match(tmpSA$SAid,aux$SAid)], x$SS$SSid)]
	tmpSA$SSuseCalcZero<-x$SS$SSuseCalcZero[match(aux$SSid[match(tmpSA$SAid,aux$SAid)], x$SS$SSid)]
	tmpSA$SLspeclistName<-x$SL$SLspeclistName[match(tmpSA$SLid, x$SL$SLid)]

	tmpSA[ ,tmpKey := paste(DEyear, SDctry, SDinst, SSspecListName, SScatchFra, SAspeCode)]
	
	# restricts to SSuseCalcZero=='Y'
	tmpSA<-tmpSA[SSuseCalcZero=='Y',]

if(nrow(tmpSA)>0)
{
colsToDelete<-c("SDctry", "SDinst","SSspecListName","DEyear","SScatchFra")

	tmpSA[, (colsToDelete) := lapply(.SD, function(x) x<-NULL),
        .SDcols = colsToDelete]

	# creates tmpKey in SL
	tmpSL[, tmpKey := paste(SLyear, SLcou, SLinst, SLspeclistName, SLcatchFrac, SLcommTaxon)]

  # stop: (rare?) situation still to be considered [multiple SAcatchCat, SAsex, SAlandCat per id]
  if (any(tmpSA[, .N, .(SSid,SAstratumName, SAcatchCat, SAsex, SAlandCat)][
			,.N, .(SSid,SAstratumName)]$N>1)) stop("cannot generateZerosUsingSL because >1 SAcatchCat
								OR SAsex OR SAlandCat in same SSid*SAstratumName: situation
										still to be analyzed - likely you should have them ")
          
  ls1 <- split(tmpSA, paste(tmpSA$SSid, tmpSA$SAstratumName))
  ls2 <- lapply(ls1, function(x) {
    for (w in tmpSL$tmpKey[tmpSL$SLspeclistName==x$SLspeclistName]) {
         if (!w %in% tmpSA$tmpKey) {
		  # duplicates SA row
          y <- x[1, ]
  		  # handles the key
		  y$SAspeCode <- as.integer(unlist(strsplit(w," "))[6])
		  # handles the remainder
		  y$SAspeCodeFAO <- NA
		  y$SAstateOfProc <- 'UNK'
		  y$SApres <- 'Unknown'
		  y$SAstateOfProc <- 'Unknown'
		  y$SAspecState <- 'Unknown'
		  y$SAtotalWtLive <- 0
          y$SAsampWtLive <- 0
          y$SAtotalWtMes <- 0
          y$SAsampWtMes <- 0
          y$SAnumTotal <- ifelse(y$SAunitType=="Individuals", 0, y$SAnumTotal)
          y$SAnumSamp <- ifelse(y$SAunitType=="Individuals", 0, y$SAnumSamp)
          y$SAselProb <- 1
          y$SAincProb <- 1
		  y$SAid <- min(x$SAid) - 0.1 # maintain a count
          y$SAseqNum <- min(x$SAseqNum) - 0.001 # maintain a count
          y$SAunitName <- min(x$SAid) - 0.001 # maintain a count
          y$SAsex <- 'U'
          y$SAlowHierarchy <- "D"
          y$SAsamp <- "N"
          x <- rbind(y, x)
          x
        } else {
          x
        }
    }
    x <- x[order(x$SAid, decreasing = F), ]
    # cleans tmpKey
	x$tmpKey<-NULL
	x
  })
  x[["SA"]] <- data.table::setDT(do.call("rbind", ls2))


  # delete aux var
  x[["SA"]]$SSuseCalcZero<-NULL
  x[["SA"]]$SLspeclistName<-NULL
  x[["SA"]]$SLid<-NULL
  # Ensure key is set on SA
  setkey(x[["SA"]],SAid)
}
  x
}
