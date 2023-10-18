#' Convert List of Data Frames to a RDBES Data Object
#'
#' This function converts a list of data frames into an object of class
#' `RDBESDataObject`.
#'
#' @param myList A list of data tables. Each element of the list should be a
#'   data frame or NULL.
#' @param castToCorrectDataTypes A logical value indicating whether to cast the
#'   columns to the correct data types. Default is TRUE.
#' @param strict (Optional) This function validates the RDBESDataObject it
#'   creates - should the validation be strict? The default is TRUE.
#'
#' @return An RDBESDataObject with each element being a data table from the
#'   input list.
#'
#' @keywords internal
#' @md
#'
#' @details The function converts all tables to `data.table`. `NULL` tables are
#'   left as `NULL`.
#'
#'   If `castToCorrectDataTypes = TRUE`, it ensures all columns are of the
#'   correct data type using `setRDBESDataObjectDataTypes`.
#'
#'   Column names are not (at present) checked, so they should be the offical
#'   RDBES 'R names' from the model documentation.
#'
#'   The function then sets a key on each table using the 'XXid' column as the
#'   key, where 'XX' is the name of that table, and replaces all empty strings
#'   with `NA`.
#'
#'   It then uses the `newRDBESDataObject` function to create a new
#'   `RDBESDataObject`.
#'
#'   Finally, it validates the RDBESDataObject using
#'   `RDBEScore::validateRDBESDataObject` and returns it.

importRDBESDataDFS <- function(myList,
                               castToCorrectDataTypes = TRUE,
                               strict = TRUE,
                               ...){

  dt <- RDBEScore::newRDBESDataObject(DE = makeDT(myList[["DE"]]),
                                      SD = makeDT(myList[["SD"]]),
                                      VS = makeDT(myList[["VS"]]),
                                      FT = makeDT(myList[["FT"]]),
                                      FO = makeDT(myList[["FO"]]),
                                      TE = makeDT(myList[["TE"]]),
                                      LO = makeDT(myList[["LO"]]),
                                      OS = makeDT(myList[["OS"]]),
                                      LE = makeDT(myList[["LE"]]),
                                      SS = makeDT(myList[["SS"]]),
                                      SA = makeDT(myList[["SA"]]),
                                      FM = makeDT(myList[["FM"]]),
                                      BV = makeDT(myList[["BV"]]),
                                      VD = makeDT(myList[["VD"]]),
                                      SL = makeDT(myList[["SL"]]),
                                      CL = makeDT(myList[["CL"]]),
                                      CE = makeDT(myList[["CE"]]))

  # Ensure all the columns are the correct data type
  if(castToCorrectDataTypes) dt <- RDBEScore:::setRDBESDataObjectDataTypes(dt)

  # Set a key on any data tables in myList - use the XXid column as the key
  for(aTable in names(dt)){
    #skip redundant tables
    # aTable = "DE"
    if(is.null(dt[[aTable]])){
      next
    } else {
      # SET KEY
      data.table::setkeyv(dt[[aTable]], paste0(aTable,"id")) # essentially orders rows by id column?
      # SET NAMES - skipped for now
      oldNames <- colnames(dt[[aTable]])
      # oldNames <- oldNames[c(2:64,1)]

      rNames <- RDBEScore:::convert.col.names(table = aTable, new.names = "R.name")
      #names(rNames) <- mapColNamesFieldR$Field.Name[mapColNamesFieldR$Table.Prefix == aTable]
      data.table::setnames(dt[[aTable]], old = oldNames, new = rNames, skip_absent = TRUE)
      names(dt[[aTable]])
      # SET all empty strings to NA
      dt[[aTable]][dt[[aTable]]==""] <- NA
    }
  }

  #check the data
  validateRDBESDataObject(dt,
                          checkDataTypes = castToCorrectDataTypes,
                          strict = strict,
                          ...)

  return(dt)
}
