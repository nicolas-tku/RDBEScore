
# Get the list of aphiaids from the ICES vocabulary
#codeTypes <- icesVocab::getCodeTypeList()
icesSpecWoRMS <- icesVocab::getCodeList(code_type = "SpecWoRMS")

# Add a date field
icesSpecWoRMS$DateDownloaded <- Sys.Date()

# Save the data
usethis::use_data(icesSpecWoRMS, overwrite = TRUE)

