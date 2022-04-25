#' Generate Multi-Level Estimations
#'
#' Generates mean and variance estimations for a list of related data tables.
#' The input data tables is expected to follow the structure of RDBESrawObject.
#' The mean an variance estimators are calculated for the target variable.
#'
#' @param tbls a named list of data tables that follow the structure of
#' RDBESrawObject
#' @inheritParams estimate
#' @inheritDotParams estimate
#'
#' @return a list of values for the estimation result that has all of the
#' estimation data on uppre tables included
#' @export
#'
#' @examples
#' hd <- generateTestTbls(LETTERS[1:5], selMeth = "SRSWOR", propSamp = 0.5)
#' startFrom <- list(parentIdCol = "Aid", ids = c(1))
#' doMultiStageEstimation(tbls = hd, startFrom, target = "Dy")
doMultiStageEstimation <- function(tbls, estimRes, target, ...) {
  if (length(tbls) == 0) {
    return(NULL)
  }
  tbl <- tbls[[1]]
  tbls[[1]] <- NULL
  if (target %in% colnames(tbl)) {
    return(estimate(tbl, estimRes, target))
  }
  estimRes <- estimate(tbl, estimRes, target, ...)
  doMultiStageEstimation(tbls, estimRes, target, ...)
}


#' Gather Estimates for Data Table
#'
#' Gets the mean and variance estimates for a single table aggregating all
#' of the parent children within the table.
#'
#' @param tbl data table to use for estimation
#' @param estimRes a list of 2 values. First, named **parentIdCol**, indicates
#' the column to use for sub-setting the data  and the second is a vector of
#' values used for sub-setting.
#' @param target string, the value to be estimated
#' @param stratums named list, vector of stratum names to use at each level.
#' Defaults to NULL, meaning using all strums.
#' @param typeOfMeasure  string, for lower hierarchy several measure types can
#' be used (e.g. age, length, weight). The type of interest can be specified.
#' Defaults to NULL meaning all will be used.
#'
#' @return a list of estimations at each level
estimate <- function(tbl, estimRes, target,
                     stratums = NULL,
                     typeOfMeasure = NULL) {
  parentIdCol <- estimRes$parentIdCol
  ids <- estimRes$ids
  tblName <- unique(tbl[[colnames(tbl)[grepl("recType$", colnames(tbl))]]])
  if (length(tblName) > 1) {
    stop("Mixed table")
  }
  cols <- icesRDBES:::getColnames(tblName)
  missingCols <- setdiff(cols, colnames(tbl))
  isTarget <- length(setdiff(target, colnames(tbl))) == 0
  data <- tbl[get(parentIdCol) %in% ids, ]
  data$dummy <- 1
  estim <- NA
  if (length(missingCols) == 0) {
    if (isTarget) {
      cols <- c(cols, y = target)
    }
    if (!isTarget) {
      cols <- c(cols, y = "dummy")
    }
    if (!is.numeric(data[, get(cols["y"])])) {
      warning("Target is not numeric! converting!")
      data[, cols["y"] := as.numeric(data[, get(cols["y"])])]
    }

    # TODO type of measure should also be selected
    # NB for sample/test data is all the same "Age"

    stratified <- unique(data[, get(cols["stratifiedCol"])])
    if (length(stratified) != 1) {
      stop("Can't handle mixed Y & N stratification")
    }
    # is unstratified this should be "U" for all data and we have 1 stratum
    dataStratums <- unique(data[, get(cols["stratumCol"])])
    if (!is.null(stratums)) {
      levelStratums <- stratums[[cols["stratumCol"]]]
      if (!is.null(levelStratums)) {
        dataStratums <- levelStratums
      }
    }

    estims <- lapply(dataStratums, estimStratum,
      ids = ids,
      data = data,
      cols = cols,
      parentIdCol = parentIdCol
    )

    estim <- aggLevelEstimations(estims)
    if (!is.null(estimRes$prevStage)) {
      lastEstim <- getLastEstimRes(estimRes)
      estim$est.total <- estim$est.total * lastEstim$est.total
    }
  }
  return(list(
    parentIdCol = cols["idCol"],
    ids = data[, get(cols["idCol"])],
    tbl = tblName,
    estim = estim, prevStage = estimRes
  ))
}

estimStratum <- function(stratum, data, cols, parentIdCol, ids) {
  data <- data[get(cols["stratumCol"]) %in% stratum, ]
  estims <- lapply(ids, getEstim,
    data = data,
    cols = cols,
    parentIdCol = parentIdCol
  )
  aggLevelEstimations(estims)
}

aggLevelEstimations <- function(estims) {
  # TODO the big question is how to aggregate these estimations into one
  # especially PI
  return(list(
    est.total = mean(sapply(estims, function(x) {
      x$est.total
    })),
    est.mean = mean(sapply(estims, function(x) {
      x$est.mean
    })),
    var.total = mean(sapply(estims, function(x) {
      x$var.total
    })),
    var.mean = mean(sapply(estims, function(x) {
      x$var.mean
    })),
    # sum the PI, keep structure
    PI = Reduce(`+`, lapply(estims, function(x) {
      x$PI
    }))
  ))
}

getEstim <- function(id, data, cols, parentIdCol) {
  data <- data[get(parentIdCol) == id, ]
  y <- data[, get(cols["y"])]
  sampled <- data[, get(cols["numSampledCol"])]
  total <- data[, get(cols["numTotalCol"])]

  selMethod <- unique(data[, get(cols["selMethodCol"])])
  if (length(selMethod) != 1) {
    stop(paste(
      "Can't deal with multiple selection methods.", tblName,
      "has methods:", paste0(selMethod, collapse = ", ")
    ))
  }
  estim <- estimMC(y, sampled, total, selMethod)
  # handle NaNs if n == 1 TODO is this the correct way? should it be zero
  if (is.nan(estim$var.total)) estim$var.total <- estim$var.mean <- 1
  # TODO handle at one level CENSUS sampling
  # so that multiplication would'nt yield 0
  # if(estim$var.total == 0) estim$var.total <- estim$var.mean <- 1
  estim
}

getLastEstimRes <- function(estimRes) {
  if (is.null(estimRes$prevStage)) stop("no estim found")
  if (is.list(estimRes$estim)) {
    return(estimRes$estim)
  }
  getLastEstimRes(estimRes$prevStage)
}
