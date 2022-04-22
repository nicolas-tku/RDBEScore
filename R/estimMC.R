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
  }

  # TODO Add Unequal probability here
  if (grepl("^UPS", method)) {
    stop()
  }

  if (grepl("WOR$", method) || grepl("^CENSUS$", method)) {
    enkl <- enk %*% t((sampled - 1) / (total - 1))
    # selecting single element in the sample twice is not possible in WOR
    diag(enkl) <- enk
  }

  if (grepl("WR$", method)) {
    # from https://math.stackexchange.com/questions/2037468
    pk <- enk * ((total - 1) / total)^ (sampled - 1)
    enkl <- (sampled * pk %*% t(sampled * pk)) * (sampled - 1) * sampled
    # enkl <-  ((enk %*% enl) * ((sampled - 1)*sampled))
  }

  est.total <- sum(y / enk)
  est.mean <- est.total / mean(total)

  partY <- (y / enk) %*% (y / enl)
  partUp <- enkl - (enk %*% enl)
  var.total <- sum((partUp / enkl) * partY)

  # TODO the code in "WR$" does not produce the expected value
  # temporarily falling back to simplification until fixed
  if (method == "SRSWR") {
    varSRSWR <- function(y, n, N) {
      meanY <- mean(y)
      S2 <- 1 / (n - 1) * (sum((y - meanY)^2))
      N^2 * (S2 / n)
    }
    var.total <- varSRSWR(y, mean(sampled), mean(total))
    # from https://math.stackexchange.com/questions/2037468
    enkl <- pk %*% t(pk)
    never_sampled <- ((total - 1) / total)^sampled
    diag(enkl) <- 1 - (never_sampled + pk)
  }

  var.mean <- var.total / sum(total) * mean(sampled)
  PI <- enkl


  return(list(
    est.total = est.total,
    est.mean = est.mean,
    var.total = var.total,
    var.mean = var.mean,
    PI = PI
  ))
}
