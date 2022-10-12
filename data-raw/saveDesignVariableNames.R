# Save the design variable names as a vector in the package data

designVariables <- c(
  "stratification", "stratumName", "clustering",
  "clusterName", "sampler", "numTotal", "numSamp",
  "selProb", "incProb", "selectMeth", "unitName",
  "selectMethCluster", "numTotalClusters",
  "numSampClusters", "selProbCluster",
  "incProbCluster", "samp", "noSampReason"
)

#outFile <- "data\\designVariables.RData"

# Save the data
usethis::use_data(designVariables)
