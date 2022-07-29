#' Generate vector of selection or inclusion probabilities
#' (version of fuction by John Ball)
#'
#' @param x RDBES data frame
#' @param probType  Value == "selection" | "inclusion" ,
# for selection or inclusion probabilities respectively
#'
#' @return A vector or probabilities
#' @export
#'
#' @examples
#' \dontrun{
#' generateProbs(x=y, probType="inclusion")
#' }
generateProbs <- function(x, probType) {

  # Only allow "inclusion" or "selection" probType at the moment
  if (!probType %in% c("inclusion","selection")){
    stop(paste0("Unallowed value supplied for 'probType': ",probType))
  }

  #a <- unique(x[grepl("selectMeth", names(x)) == T])
  #a <- as.character(unique(a[grepl("selectMethCluster", names(a)) == F]))
  # The above lines were giving an error "...Recycling of logical i is no longer allowed..."
  methColNames <- grep("^..selectMeth$", names(x), value = TRUE)
  a <- as.character(unique(x[ ,..methColNames ]))


  if (sum(is.na(a)) > 0) stop("cannot proceed: NAs in SSselectMeth")
  if (length(a) > 1) stop("two different selection methods")

  #vecSmallN <- x[grepl("numSamp", names(x)) == T]
  #vecSmallN <- vecSmallN[grepl("SampCluster", names(vecSmallN)) == F]
  #vecBigN <- x[grepl("numTotal", names(x)) == T]
  #vecBigN <- vecBigN[grepl("TotalCluster", names(vecBigN)) == F]
  # The above lines were giving an error "...Recycling of logical i is no longer allowed..."
  numSampColNames <- grep("^..numSamp$", names(x), value = TRUE)
  vecSmallN <- x[[numSampColNames]]
  numTotalColNames <- grep("^..numTotal$", names(x), value = TRUE)
  vecBigN <- x[[numTotalColNames]]

  vecProb <- NA

  if (probType == "selection") {
    #vecProb <- a[grepl("selProbUnit", names(a)) == T] # not defined
    vecProbColNames <- grep("^..selProb$", names(x), value = TRUE)
    vecProb <- x[[vecProbColNames]]

    print(a)
    if (a %in% c("SRSWR", "SRSWOR")) {
      if (a == "SRSWR") vecProb <- 1 / vecBigN
      if (a == "SRSWOR") stop("depends on order")
    }
    if (a %in% c("UPSWR", "UPSWOR")) {
      if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
      vecProb <- vecProb
    }
    x[[vecProbColNames]] <- vecProb
  }


  if (probType == "inclusion") {
    #vecProb <- a[grepl("incProbUnit", names(a)) == T]
    vecProbColNames <- grep("^..incProb$", names(x), value = TRUE)
    vecProb <- x[[vecProbColNames]]

    if (length(a) > 1) {
      stop("two different selection methods")
    } else {
      print(a)
      if (a %in% c("SRSWR", "SRSWOR")) {
        if (sum(is.na(vecBigN)) > 0) stop("cannot proceed: NAs in total")
        if (sum(is.na(vecSmallN)) > 0) stop("cannot proceed: NAs in sampled")
        if (a == "SRSWR") vecProb <- 1 - (1 - 1 / vecBigN)^vecSmallN
        if (a == "SRSWOR") vecProb <- vecSmallN / vecBigN
      }
      if (a %in% c("UPSWR", "UPSWOR")) {
        if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
        vecProb <- vecProb
      }
    }
    x[[vecProbColNames]] <- vecProb
  }

  #vecProb
  x
}
