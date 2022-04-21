#' Multiple Count Estimator for Population Total
#'
#' @param y numeric variable to be estimated
#' @param sampled numeric total number of units sampled
#' @param total numeric total number of units int the population
#' @param method character selection method code e.g SRSWOR
#'
#' @return list of 5 elements including the population mean, total
#' (and their variance) and the I order inclusion probabilities
#' @export
#'
#' @examples
#' estimMC(c(3, 4, 4, 5), c(4, 4, 4, 4), c(8, 8, 8, 8))
estimMC <- function(y, sampled, total, method = "SRSWOR") {
  implementedMethods <- c(
    "^SRSWR$",
    "^SRSWOR$",
    "^CENSUS$"
  )
  if (!any(sapply(implementedMethods, grepl, method))) {
    stop(paste("Sampling method ", method, "not implemented!"))
  }

  n <- length(y)
  if (!all(sapply(c(y, sampled, total), is.numeric))) {
    stop("y, sampled and total must, be numeric vectors!")
  }

  if (any(length(sampled) != n, length(total) != n)) {
    stop("y, sampled and total must, be vectors of same length!")
  }
  #TODO include also quasi random
  if(grepl("^SRS", method) || grepl("^CENSUS$", method)){

    enk <- sampled / total
    enl <- t(enk)
  }

  #TODO Add Unequal probability here
  if(grepl("^UPS", method)){
    stop()
  }

  if (grepl("WOR$", method)) {
      enkl <- enk %*% t((sampled - 1) / (total - 1))
      #selecting one in the sample twice is not possible in WOR
      diag(enkl) <- enk
  }

  if (grepl("WR$", method)) {
    enkl <- (n * (n - 1)) * (enk/total %*% enl/sampled)
  }

  if (method == "CENSUS") {
    enkl <- enk %*% t((sampled - 1) / (total - 1))
  }

    est.total <- sum(y %/% enk)
    partY <- (y / enk) %*% (y / enl)
    partUp <- enkl - (enk %*% enl)
    var.total <- sum((partUp / enkl) * partY)

    est.mean <- est.total / mean(total)
    var.mean <- var.total / sum(total)
    PI <- enk %*% enl




  return(list(
    est.total = est.total,
    est.mean = est.mean,
    var.total = var.total,
    var.mean = var.mean,
    PI = PI
  ))
}

