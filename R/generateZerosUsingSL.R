#' Generate zeros in samples using Species List information
#'
#' examples for now see
#' https://github.com/ices-eg/WK_RDBES/tree/master/WKRDB-EST2/chairs/Nuno
#'
#' @param x RDBES data frame
#'
#' @return RDBES data frame where SA was complemented with species looked for
#' (sensu in sampling objectives) but not registered in sample
#' @export
#'


generateZerosUsingSL <- function(x) {

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(x, verbose = FALSE)

  if (!(nrow(x[["SA"]]) >= 1 && nrow(x[["SL"]]) >= 1)) stop("no SA and/or SL")

  # Take a copy of SA since we'll change some column data types and
  # we don't want to update the original version
  tmpSA <- data.table::copy(x[["SA"]])
  tmpSL <- x[["SL"]]
  # Now convert some columns from int to numeric
  colsToConvertToNumeric <- c("SAid", "SAseqNum")
  tmpSA[, (colsToConvertToNumeric) := lapply(.SD, as.double),
        .SDcols = colsToConvertToNumeric]


  ls1 <- split(tmpSA, tmpSA$SSid)
  ls2 <- lapply(ls1, function(x) {
    for (sppCode in tmpSL$SLsppCode) {
      if (sppCode %in% tmpSL$SLsppCode) { # sppCode is not in list
        if (!sppCode %in% x$SAspeCode) {
          # duplicates SA row
          y <- x[1, ]
          y$SAspeCode <- sppCode
          y$SAtotalWtLive <- 0
          y$SAsampWtLive <- 0
          y$SAtotalWtMes <- 0
          y$SAsampWtMes <- 0
          y$SAid <- min(x$SAid) - 0.001 # maintain a count
          y$SAseqNum <- min(x$SAseqNum) - 0.001 # maintain a count
          y$SAunitName <- min(x$SAid) - 0.001 # maintain a count
          y$SAsex <- NA
          y$SAlowHierarchy <- "D"
          y$SAsamp <- "N"
          x <- rbind(y, x)
          x
        } else {
          x
        }
      }
    }
    x <- x[order(x$SAid, decreasing = F), ]
    x
  })
  x[["SA"]] <- data.table::setDT(do.call("rbind", ls2))
  x
}
