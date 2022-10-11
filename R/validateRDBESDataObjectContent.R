#' check RDBES Raw Object Content
#'  Private function to do some basic checks on
#' the content of the RDBESDataObject (e.g. all required field names are
#' present).  Function is only used by checkRDBESDataObject and should only
#' be passed a list of non-null objects
#'
#' @param objectToCheck - RDBESDataObject i.e. a list of data.tables
#'
#' @return list with  first element as the object and the second the warnings
#'
validateRDBESDataObjectContent <- function(objectToCheck) {
  validRDBESDataObject <- TRUE
  warningText <- NA

  # For each entry see if they have the required field names
  badEntries <- objectToCheck[!
  sapply(objectToCheck, function(x) {
    returnValue <- FALSE
    # Assume the first field name accurately gives us the table name
    tableName <- substring(names(x)[1], 1, 2)
    requiredColumnNames <-
      icesRDBES::mapColNamesFieldR[
        icesRDBES::mapColNamesFieldR$Table.Prefix == tableName,
        ]
    requiredColumnNames[is.na(requiredColumnNames$R.Name), "R.Name"] <-
      requiredColumnNames[is.na(requiredColumnNames$R.Name), "Field.Name"]
    requiredColumnNames <- requiredColumnNames$R.Name
    # Are all the required names present?
    if (all(requiredColumnNames %in% names(x))) {
      returnValue <- TRUE
    } else {
      missingColumnNames <-
        requiredColumnNames[!requiredColumnNames %in% names(x)]
      print(paste("The following required columns are missing from table",
            tableName,":",
            paste(missingColumnNames,collapse = ",")))
    }
    returnValue
  })]

  # CHECK 6 Check if there are any entries which have invalid field names
  if (length(badEntries) > 0) {
    validRDBESDataObject <- FALSE
    warningText <-
      paste("objectToCheck contains the following tables which don't ",
        "contain all required fields: ",
        paste(names(badEntries), collapse = ","),
        sep = ""
      )
  } else { # 1

    # CHECK 7 Check if any tables have duplicate rows
    tablesWithDupes <- objectToCheck[
      sapply(objectToCheck, function(x) {
        any(duplicated(x))
      })
    ]

    if (length(tablesWithDupes) > 0) {
      validRDBESDataObject <- FALSE
      warningText <-
        paste("objectToCheck contains the following tables which have ",
          "duplicate rows: ",
          paste(names(tablesWithDupes), collapse = ","),
          sep = ""
        )
    } else { # 2

      badEntries <- objectToCheck[
        sapply(objectToCheck, function(x) {
          returnValue <- FALSE
          # Assume the first field name accurately gives us the table name
          tableName <- substring(names(x)[1], 1, 2)
          idFieldName <- paste0(tableName, "id")
          returnValue <- any(duplicated(x[, idFieldName, with=FALSE]))
          returnValue
        })
      ]

      # CHECK 8 See if the XXid fields contain duplicates
      if (length(badEntries) > 0) {
        validRDBESDataObject <- FALSE
        warningText <-
          paste("objectToCheck contains the following tables which have ",
            "duplicated values in their XXid field: ",
            paste(names(badEntries), collapse = ","),
            sep = ""
          )
      }
    } # 2
  } # 1

  # Return our outcome as a list
  list("validRDBESDataObject" = validRDBESDataObject, "warningText" = warningText)
}
