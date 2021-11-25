handlePrefixInVarNames <- function(x, type) {
  
  if (type=="remove"){
    for (i in names(x)) {
    print(i)
    if (!is.null(x[[i]])) {
      targetNames <- !grepl(names(x[[i]]), pat = paste0(i, "id"))
      colnames(x[[i]])[targetNames] <- gsub(i, "", names(x[[i]])[targetNames])
    }
  }
  }
  
   if (type=="add"){
   browser()
    for (i in names(x)) {
    print(i)
    if (!is.null(x[[i]])) {
      targetNames <- !grepl(names(x[[i]]), pat = paste0(i, "id"))
      colnames(x[[i]])[targetNames] <- gsub(i, "", names(x[[i]])[targetNames])
    }
  }
  } 
   x
}