
# Main settings (normally, the only ones that should change among configuration files for different species)
species = "Bombus pascuorum"
otherNames = c("bombus.pascuorum", "Bombus.pascuorum", "Bombus_ pascuorum", "Bombus_pascuorum", "B. pascuorum", "B_pascuorum", "B..pascuorum")  # other names of the species in the OBServ database. See "OBservData\Thesaurus_Pollinators\organism_guild_META_STUDIES.csv"
excludeNames = c("Bombus Sp.", "Bombus sp", "Bombus sp.", "Bombus spp", "Bombus", "Bombus_6", "Bombus_7", "Bombus_spp.", "Bombus_sp") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = FALSE   # TRUE: gbif df already calculated and saved
observReady     = FALSE  # TRUE: observ df already calculated and saved
locatReady      = FALSE  # TRUE: locations df already calculated and saved
featuresReady   = FALSE  # TRUE: features df already calculated and saved
useRasters      = FALSE  # TRUE: use collection of rasters to extract features 

