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

  if(grepl("^SRS", method)){

    enk <- sampled / total
    enl <- t(enk)

    if (method == "SRSWOR") {
      enkl <- matrix((sampled / (sampled - 1))  * (total / (total - 1)),
                     nrow = n, ncol = n)
    }

    if (method == "SRSWR") {
      enkl <- (sampled %*% t(sampled - 1)) * enk %*% enl
    }

    total.est <- sum(y/enk)
    partY <- (y/enk) %*% (t(y)/enl)
    partUp <- enkl - (enk %*% enl)
    var.total.biased <- sum(partUp * partY)
    var.total <- sum((partUp/enkl)*partY)

    mean.est <- sum(y / enk / total)
    var.mean <- sum(var.total / (total^2))
    PI <- enk %*% enl
  }

  # TODO Is CENSUS necessary?  This is just a simplification of SRSWOR at n=N
  if (method == "CENSUS") {
    total.est <- sum(y)
    var.total <- 0
    mean.est <- sum(y) / n
    var.mean <- 0
    var.total.unbiased <- 0
    PI <- matrix(1, nrow = n, ncol = n)
  }

  return(list(
    est.total = total.est,
    est.mean = mean.est,
    var.total = var.total,
    #var.total.unbiased <- var.total.unbiased,
    var.mean = var.mean,
    PI = PI
  ))
}
