list_of_dfs <- createRDBESDataObject("tests/testthat/h1_v_1_19_18/H1_2023_10_16.zip")
list_of_dfs <- list_of_dfs[!(sapply(list_of_dfs, is.null))]
list_of_dfs <- lapply(list_of_dfs, as.data.frame)

