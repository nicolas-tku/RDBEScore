#' Multiple Count Estimator for Population Total
#'
#' @param y numeric variable to be estimated
#' @param sampled numeric total number of units sampled
#' @param total numeric total number of units int the population
#' @param method character selection method code e.g SRSWOR
#'
#' @return list of 7 elements including the population mean, total
#' (and their variance), the algorithm name used and the I order
#' inclusion probabilities
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

  # Generalized Horvitz-Thompson estimator
  est.algorithm <- "Generalized Horvitz-Thompson aka Mutiple-Count"
  est.total <- sum(y / enk)
  est.mean <- est.total / mean(total)

  # Sen-Yates-Grundy estimate of variance
  var.algorithm <- "Sen-Yates-Grundy"
  partY <- (y / enk) %*% t(rep(1,n)) - t((y / enk) %*% t(rep(1,n)) )
  diag(partY) <- NA
  partY <- partY^2
  var.mat <- 0.5 * (((enk %*% enl) - enkl) / enkl)
  diag(var.mat) <- NA
  var.mat <- var.mat * partY
  var.total <- sum(var.mat, na.rm = T)
  var.mean <- var.total / sum(total) * mean(sampled)

  PI <- enkl


  return(list(
    est.total = est.total,
    est.mean = est.mean,
    est.algorithm = est.algorithm,
    var.total = ifelse(n < 2, NaN, var.total),
    var.mean = ifelse(n < 2, NaN, var.mean),
    var.algorithm = var.algorithm,
    PI = PI
  ))
}
