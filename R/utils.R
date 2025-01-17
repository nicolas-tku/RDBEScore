# This is a file to store general internal utility functions


#' as.integer.or.dbl
#'
#' This function checks if any value in a vector is above 2e+09, and if so runs
#' `round(as.double())` on it. If not it uses `as.integer()` instead. This is to
#' get around the 32-bit limitation in R that integers cannot be larger than
#' around 2e+09, in which case `as.integer` would return an `NA`.
#'
#' @param x vector to be coerced to integers or doubles
#'
#' @return a vector of integers or doubles
#' @importFrom stats na.omit
#' @keywords internal

as.integer.or.dbl <- function(x){

  # we apply as.numeric in case it is a character vector
  # we apply as.omit because that causes an error
  if(any(as.numeric(na.omit(x)) > 2e+09)) out <- round(as.double(x)) else
    out <- as.integer(x)

  return(out)
}

# Deal with "no visible binding for global variable.." warnings in R CMD CHECK
globalVariables(c("mapColNamesFieldR", "mapColNamesFieldR", "SAid",
                  "rdbesEstimObj", "..targetProbColumns",
                  "..targetProbColumns2", "finalInclusionProb_to_su1",
                  "targetValue", "su1unitName", "su1inclusionProb",
                  "..target_prob_columns2", "su1selectionProb",
                  "..varsNeeded", "%>%", "parentTableID", "est.total",
                  "recType", "parentTableStratum", "stratumName",
                  "parentIDandStratum", "studyVariable", "..myColNames",
                  "..methColNames", "tblName", "all_of"))



