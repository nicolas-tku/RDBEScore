#' Generate probabilities missing from RDBES Data
#'
#' Wrapper to generate probabilities. The wrapper calls
#' runChecksOnSelectionAndProbs which main tests need to be passed before
#' probabilities can be calculated. The it calls generateProbs for
#' each sample in each sampling level of the hierarchy.
#'
#' @param x - RDBES raw object
#' @param probType - string. Can be set to "selection" (only selection
#' probabilities are calculated), "inclusion" (only inclusion probabilities are
#' calculated) or "both" (both types of probabilities are calculated)
#' @param overwrite - if TRUE will overwrite probabilities already existing for
#' SRSWR and SRSWOR
#' @param runInitialProbChecks - if TRUE runs runChecksOnSelectionAndProbs
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return a list of all the RDBES data tables with probabilites calculated
#'
#' @export
#'
#' @seealso \code{\link{runChecksOnSelectionAndProbs}}
#' \code{\link{generateProbs}}
#'
#' examples for now see
#' https://github.com/ices-eg/WK_RDBES/tree/master/WKRDB-EST2/chairs/Nuno
#'

applyGenerateProbs <- function(x, probType, overwrite,
                               runInitialProbChecks = TRUE,
                               strict = TRUE) {

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(x, verbose = FALSE, strict = strict)

  if (runInitialProbChecks) {
    print("========start runChecksOnSelectionAndProbs=======")
    runChecksOnSelectionAndProbs(x)
    print("========end runChecksOnSelectionAndProbs=======")
  }


  print("========start generateProbs=======")

  if (length(unique(x[["DE"]]$DEhierarchy)) > 1) stop(">1 hierarchy in data:
                                                    not yet developed")

  targetTables <- getTablesInRDBESHierarchy(x[["DE"]]$DEhierarchy[1],
                                            includeTablesNotInSampHier = FALSE)
  targetTables <- targetTables[targetTables != "DE"]
  # Code doesn't handle lower hierachy A or B yet
  targetTables <- targetTables[targetTables != "FM"]
  parentId <- paste0(targetTables, "id")
  targetTables <- targetTables[targetTables != "SD"]
  parentId <- parentId[parentId != "BVid"]

  # aspects needing development
  if (any(!is.na(x[["SA"]]$SAparentID))) stop("multiple sub-sampling present
                                                in SA: not yet developed")
  if (nrow(x[["SA"]]) >= 1 && any(x[["SA"]]$SAlowHierarchy %in% c("A", "B"))) {
    stop("lower hierarchy A and B present: not yet developed")
  }


  for (i in targetTables) {
    print(i)

    # Only process if the table has rows
    if (nrow(x[[i]]) > 0 ){

      # following code will be worth setting in data.table
      ls1 <- split(x[[i]], x[[i]][[eval(noquote(parentId[targetTables == i]))]])
      ls2 <- lapply(ls1, function(x, ...) {
        # aspects needing development
        if (length(unique(x$stratumName)) > 1 | any(x$stratification == "Y")) {
          stop("stratification present: not yet developed")
        }
        if (length(unique(x$clusterName)) > 1 | any(x$clustering == "Y")) {
          stop("clustering present: not yet developed")
        }
        print(paste0(
          parentId[targetTables == i], ": ",
          x[[parentId[targetTables == i]]][1]
        ))
        x <- generateProbs(x, probType)
        x
      })
      x[[i]] <- data.table::setDT(do.call("rbind", ls2))

    }
  }

  print("========end generateProbs=======")
  x
}
