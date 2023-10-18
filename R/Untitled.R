if(any(!(sapply(names(list_of_dfs),
             function(z) z %in% c("DE", "SD", "VS", "FT", "FO", "TE", "LO", "OS", "LE",
                                  "SS", "SA", "FM", "BV", "VD", "SL", "CL", "CE"))))) stop("ERROR")
