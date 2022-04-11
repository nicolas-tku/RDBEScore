#' Multiple Count Estimator for Population Total
#'
#' @param y variable to be estimated
#' @param sampled total number of units sampled
#' @param total total number of units int the population
#' @param method selection method code e.g SRSWOR
#'
#' @return list of 2 the population total and its variance
#' @export
#'
#' @examples
#' estimMC(c(3,4,4,5),c(4,4,4,4),c(8,8,8,8))
estimMC<-function(y, sampled, total, method="SRSWOR"){

  n <- length(y)

  if(any(length(sampled) != n, length(total) != n)){
    stop("y, sampled and total must, be vectors of same length!")
  }
  if(method == "SRSWOR"){
    pk <- sampled/total
    PI<-matrix(sampled/total*(sampled-1)/(total-1),
               nrow = length(pk),
               ncol = length(pk))
    diag(PI)<-pk
    A <- ( PI -  ( pk %*% t(pk)))/( PI * ( pk %*% t(pk))) * ( y %*% t(y))
    var <- sum(A)
    return(list(total=sum(y/pk),var=var))

  }
  if(method == "SRSWR"){

  }

  stop(paste("The implemented methods do not include", method))



}

