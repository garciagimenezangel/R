
# Main settings (normally, the only ones that should change among configuration files for different species)
species = "Osmia bicornis"
otherNames = c("Osmia.bicornis", "Osmia_ bicornis", "Osmia_bicornis")  # other names of the species in the OBServ database. See "OBservData\Thesaurus_Pollinators\organism_guild_META_STUDIES.csv"
excludeNames = c("Osmia", "Osmia_18", "Osmia_19", "Osmia_sp", "Osmia sp", "Osmia sp.", "Osmia sp1", "Osmia ssp.") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = FALSE   # TRUE: gbif df already calculated and saved
observReady     = FALSE  # TRUE: observ df already calculated and saved
locatReady      = FALSE  # TRUE: locations df already calculated and saved
featuresReady   = FALSE  # TRUE: features df already calculated and saved
useRasters      = FALSE  # TRUE: use collection of rasters to extract features 
