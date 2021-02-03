
# Main settings (normally, the only ones that should change among configuration files for different species)
species = "Andrena fulva"
otherNames = c("Andrena_ fulva", "Andrena_fulva", "Andrena.fulva", "Andrena_ cf. fulva")  # other names of the species in the OBServ database. See "OBservData\Thesaurus_Pollinators\organism_guild_META_STUDIES.csv"
excludeNames = c("Andrena", "Andrena sp", "Andrena sp.", "Andrena sp. ", "Andrena sp. 4", "Andrenidae", "Small Andrena sp.", "Large Andrena sp.", "Andrena ssp.", "Andrena sp1") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = FALSE   # TRUE: gbif df already calculated and saved
observReady     = FALSE  # TRUE: observ df already calculated and saved
locatReady      = FALSE  # TRUE: locations df already calculated and saved
featuresReady   = FALSE  # TRUE: features df already calculated and saved
useRasters      = FALSE  # TRUE: use collection of rasters to extract features 
