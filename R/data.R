#' The tables required for each RDBES hierarchy.
#'
#' A data frame containing the tables required for each RDBES hierachy
#'
#' @format A data frame containing the tables required for each RDBES hierachy.
#' \describe{
#'   \item{hierarchy}{the hierachy this applies to H1 to H13}
#'   \item{table}{the 2-letter table name}
#'   \item{lowerHierarchy}{is this a lower hierarchy table?}
#'   \item{optional}{is this table optional within the hierarchy?}
#'   \item{samplingUnit}{is this table a sampling unit within the hierarchy?}
#'   \item{sortOrder}{the table sort order within the hiaerarchy}
#' }
#' @source \url{https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles}
"tablesInRDBESHierarchies"
#' A dataset containing test RDBES data for H1 in the DBErawObj structure
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data frame}
#'   \item{SD}{the Sampling Details data frame}
#'   \item{VS}{the Vessel Selection data frame}
#'   \item{FT}{the Fishing Trip data frame}
#'   \item{FO}{the Fishing Operation data frame}
#'   \item{SS}{the Species Selection data frame}
#'   \item{SA}{the Sample data frame}
#'   \item{FM}{the Frequency Measure data frame}
#'   \item{BV}{the Biological Variable data frame}
#'   \item{VD}{the Vessel Details data frame}
#'   \item{SL}{the Species List data frame}
#'   ...
#' }
#' @source \url{https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles}
"h1DBErawObj"
#' A dataset containing test RDBES data for H1
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data frame}
#'   \item{SD}{the Sampling Details data frame}
#'   \item{VS}{the Vessel Selection data frame}
#'   \item{FT}{the Fishing Trip data frame}
#'   \item{FO}{the Fishing Operation data frame}
#'   \item{SS}{the Species Selection data frame}
#'   \item{SA}{the Sample data frame}
#'   \item{FM}{the Frequency Measure data frame}
#'   \item{BV}{the Biological Variable data frame}
#'   \item{VD}{the Vessel Details data frame}
#'   \item{SL}{the Species List data frame}
#'   ...
#' }
#' @source \url{https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles}
"h1Example"
#' A dataset containing the mapping from database column names
#' to R field names
#'
#' @format A data frame containing database field names and their equivalent
#' R field name:
#' \describe{
#'   \item{Table.Prefix}{The two letter prefix of the relevent RDBES table}
#'   \item{Field.Name}{The database field names}
#'   \item{R.Name}{The equivalent R field name}
#'   \item{RDataType}{The equivalent R data type (e.g. "integer", "character" etc)}
#'   \item{Type}{The Data type in the RDBES documentation (e.g. "Decimal", etc)}
#'   ...
#' }
#' @source \url{https://sboxrdbes.ices.dk}
"mapColNamesFieldR"
#' One quarter of sample data from swedish shrimp landings of the
#' SWE_OTB_CRU_32-69_0_0 fishery
#'
#' A dataset of rdbesEstimObj type containing simplified haul-level
#' samples (rows) of shrimp landings (targetValue, in kg) observed onboard using
#' H1 of RDBES with UPWOR on vessels. Data is provided for developing/testing
#' purposes only.
#'
#' @format A data frame with 10 rows and 95 variables:
#' \describe{
#'   \item{DEsamplingScheme}{Sampling Scheme}
#'   \item{DEyear}{Year of data collection}
#'   \item{DEstratumName}{Fishery code}
#'   \item{DEhierarchyCorrect}{Design Variable of RDBES. More details in RDBES documentation}
#'   \item{DEhierarchy}{Design Variable of RDBES. More details in RDBES documentation}
#'   \item{DEsampled}{Design Variable of RDBES. More details in RDBES documentation}
#'   \item{DEreasonNotSampled}{Design Variable of RDBES. More details in RDBES documentation}
#'   \item{SDcountry}{Country that collected the data}
#'   \item{SDinstitution}{Institution that collected the data}
#'   \item{su1, su2, su3, su4, su5}{sampling units of RDBES. More details in RDBES documentation}
#'   \item{XXXnumberSampled, ...}{Design Variables of RDBES. More details in RDBES documentation}
#'   \item{targetValue}{estimate of weight landed in each haul (in kg)}
#' }
#' @source Nuno Prista @ SLU Aqua, 2022
"shrimps"
#' One quarter of sample data from swedish shrimp catches of the
#' SWE_OTB_CRU_32-69_0_0 fishery
#'
#' A dataset of rdbesEstimObj type containing simplified haul-level
#' samples (rows) of shrimp catches (targetValue, in kg) observed onboard using
#' H1 of RDBES with UPWOR on vessels. Catches are divided into three strata (91, 92, 93_94)
#' that correspond to sorting sieves used onboard. Data is provided for developing/testing
#' purposes only.
#'
#' @format A data frame with 10 rows and 95 variables:
#' \describe{
#'   \item{DEsamplingScheme}{Sampling Scheme}
#'   \item{DEyear}{Year of data collection}
#'   \item{DEstratumName}{Fishery code}
#'   \item{DEhierarchyCorrect}{Design Variable of RDBES. More details in RDBES documentation}
#'   \item{DEhierarchy}{Design Variable of RDBES. More details in RDBES documentation}
#'   \item{DEsampled}{Design Variable of RDBES. More details in RDBES documentation}
#'   \item{DEreasonNotSampled}{Design Variable of RDBES. More details in RDBES documentation}
#'   \item{SDcountry}{Country that collected the data}
#'   \item{SDinstitution}{Institution that collected the data}
#'   \item{su1, su2, su3, su4, su5}{sampling units of RDBES. More details in RDBES documentation}
#'   \item{XXXnumberSampled, ...}{Design Variables of RDBES. More details in RDBES documentation}
#'   \item{su5stratumName}{sieve fraction}
#'   \item{targetValue}{estimate of weight fraction in each haul (in kg)}
#' }
#' @source Nuno Prista @ SLU Aqua, 2022
"shrimpsStrat"
#' A dataset containing the RDBES "design variable" names
#'
#' @format A vector containing the short R names of the RDBES design variables
#' (without any 2 letter table prefixes)
#' R field name:
#' \describe{
#'   \item{designVariables}{The design variable names}
#' }
#' @source \url{https://sboxrdbes.ices.dk}
"designVariables"
