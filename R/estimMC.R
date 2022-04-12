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
  if (any(grepl(method, implementedMethods))) {
    pk <- sampled / total
    PI <- matrix(sampled / total * (sampled - 1) / (total - 1),
      nrow = length(pk),
      ncol = length(pk)
    )
    diag(PI) <- pk
    pl <- t(pk)
    A <- (PI - (pk %*% pl)) / (PI * (pk %*% pl)) * (y %*% t(y))

    total.est <- sum(y / pk)
    var.total <- sum(A)

    mean.est <- sum(y / pk / total)
    var.mean <- sum(A / total)
  }
  # #TODO Is this SRSWR necessary?  This is just a simplification of SRSWOR
  # if(method == "SRSWR") {
  #   pk <- 1/total
  #   mean.est <-  (1/n) * sum(y/pk/total)
  #   total.est <- (1/n) * sum(y/pk)
  #   A <- (y/pk - total.est)^2
  #
  #   var.total <- (1/(n-1))*sum(A/total)
  #   var.mean <- (1/(n-1))*sum(A/(total^2))
  #   pk<-pk*n
  # }
  # #TODO Is CENSUS necessary?  This is just a simplification of SRSWOR at n=N
  # if(method == "CENSUS") {
  #   total.est <- sum(y)
  #   var.total <- 0
  #   mean.est <- sum(y)/n
  #   var.mean <- 0
  #   pk <- rep(1,n)
  #
  # }

  return(list(
    est.total = total.est,
    est.mean = mean.est,
    var.total = var.total,
    var.mean = var.mean,
    PI = PI
  ))
}
