#' Convert List of Data Frames to a  RDBES Data Object
#'
#' This function converts a list of data frames into an RDBESDataObject.
#'
#' @param myList A list of data tables. Each element of the list should be a
#'   data frame or NULL.
#' @param castToCorrectDataTypes A logical value indicating whether to cast the
#'   columns to the correct data types. Default is TRUE.
#' @param strict (Optional) This function validates the RDBESDataObject it
#' creates - should the validation be strict? The default is TRUE.
#'
#' @return An RDBESDataObject with each element being a data table from the
#'   input list.
#'
#' @details The function first defines a helper function, `makeDT`, which
#' converts its input into a data table using `data.table::as.data.table`. If
#' the input is NULL, it returns NULL.
#'
#' It then uses the `RDBEScore::newRDBESDataObject` function to create a new
#' RDBESDataObject, with each element being the result of applying `makeDT` to
#' the corresponding element of `myList`.
#'
#' The function then iterates over each element of `dt`. If an element is a data
#' table, it sets a key on it using the 'XXid' column as the key, where 'XX' is
#' the name of the data table. It also renames the columns according to
#' `RDBEScore::mapColNamesFieldR$R.Name`, and replaces all empty strings with
#' NA.
#'
#' If `castToCorrectDataTypes` is TRUE, it then ensures all columns are of the
#' correct data type using `RDBEScore:::setRDBESDataObjectDataTypes`.
#'
#' Finally, it validates the RDBESDataObject using
#' `RDBEScore::validateRDBESDataObject` and returns it.

# myList = H5list
# castToCorrectDataTypes = TRUE

importRDBESDataDFS <- function(myList,
                               castToCorrectDataTypes = TRUE,
                               strict = TRUE
                               ){

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


  # Set a key on any data tables in myList - use the XXid column as the key
  for(aTable in names(dt)){
    #skip redundant tables
    # aTable = "DE"
    if(is.null(dt[[aTable]])){
      next
    } else {
      data.table::setkeyv(dt[[aTable]], paste0(aTable,"id")) # essentially orders rows by id column?
      # Set R names
      #oldNames <- colnames(dt[[aTable]])
      rNames <- convert.col.names(table = aTable, new.names = "R.name")
      #names(rNames) <- RDBEScore::mapColNamesFieldR$Field.Name[RDBEScore::mapColNamesFieldR$Table.Prefix == aTable]
      data.table::setnames(dt[[aTable]], rNames, skip_absent = T)
      #set all empty strings to NA
      dt[[aTable]][dt[[aTable]]==""] <- NA
    }
  }

  if (castToCorrectDataTypes){
    # Ensure all the columns are the correct data type
   dt <- RDBEScore:::setRDBESDataObjectDataTypes(dt)
  }

  #check the data
  RDBEScore::validateRDBESDataObject(dt,
                                     checkDataTypes = castToCorrectDataTypes,
                                     strict = strict)

  return(dt)
}
