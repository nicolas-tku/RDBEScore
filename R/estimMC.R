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
  # TODO include also quasi random
  if (grepl("^SRS", method) || grepl("^CENSUS$", method)) {
    enk <- sampled / total
    enl <- t(enk)
    if (grepl("WOR$", method) || grepl("^CENSUS$", method)) {
      enkl <- enk %*% t((sampled - 1) / (total - 1))
      diag(enkl) <- enk
    }

    if (grepl("WR$", method)) {
      enkl <- (enk %*% t((sampled - 1) / total))
    }
  }

  # TODO Add Unequal probability here
  if (grepl("^UPS", method)) {
    stop("TODO")
  }

  est.total <- sum(y / enk)
  est.mean <- est.total / mean(total)

  partY <- ((y / enk) %*% (y / enl))
  var.mat <- ((enkl - (enk %*% enl)) / enkl) * partY
  #the diagonal can be set here as well see below
  # var.diag <- ((1-enk)/enk) * (y^2/enk)
  # diag(var.mat) <- var.diag
  var.total <- sum(var.mat)



  # TODO the code in "WR$" does not produce the expected value
  # temporarily falling back to simplification until fixed
  if (method == "SRSWR") {
    varSRSWR <- function(y, n, N) {
      meanY <- sum(y / (n / N))
      S2 <- (y * N - meanY)^2
      mean(1 / (n * (n - 1))) * sum(S2)
    }
    var.total <- varSRSWR(y, sampled, total)

    enkl <- enk %*% enl
  }

  var.mean <- var.total / sum(total) * mean(sampled)
  PI <- enkl


  return(list(
    est.total = est.total,
    est.mean = est.mean,
    var.total = ifelse(n < 2, NaN, var.total),
    var.mean = ifelse(n < 2, NaN, var.mean),
    PI = PI
  ))
}
