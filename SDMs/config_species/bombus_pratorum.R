
# Main settings (normally, the only ones that should change among configuration files for different species)
species = "Bombus pratorum"
otherNames = c("bombus.pratorum", "Bombus.pratorum", "Bombus_ pratorum", 
               "Bombus_pratorum", "B. pratorum", "B_pratorum", "B..pratorum")  # other names of the species in the OBServ database. See "OBservData\Thesaurus_Pollinators\organism_guild_META_STUDIES.csv"
excludeNames = c("Bombus Sp.", "Bombus sp", "Bombus sp.", "Bombus spp", "Bombus", "Bombus_6", "Bombus_7", "Bombus_spp.", "Bombus_sp", "Bombus spp. unknown or unidentified") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = TRUE   # TRUE: gbif df already calculated and saved
observReady     = TRUE  # TRUE: observ df already calculated and saved
locatReady      = FALSE  # TRUE: locations df already calculated and saved


