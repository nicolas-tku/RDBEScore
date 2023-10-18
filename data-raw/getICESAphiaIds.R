
# Get the list of aphiaids from the ICES vocabulary
#codeTypes <- icesVocab::getCodeTypeList()
icesSpecWoRMS <- icesVocab::getCodeList(code_type = "SpecWoRMS")

# Save the data
usethis::use_data(icesSpecWoRMS, overwrite = TRUE)

