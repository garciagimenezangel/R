
# Main settings (normally, the only ones that should change among configuration files for different species)
species = "Bombus hypnorum"
otherNames = c("bombus.hypnorum", "Bombus.hypnorum", "Bombus_ hypnorum", "Bombus_hypnorum", "B. hypnorum", "B_hypnorum", "B..hypnorum")  # other names of the species in the OBServ database. See "OBservData\Thesaurus_Pollinators\organism_guild_META_STUDIES.csv"
excludeNames = c("Bombus Sp.", "Bombus sp", "Bombus sp.", "Bombus spp", "Bombus", "Bombus_6", "Bombus_7", "Bombus_spp.", "Bombus_sp") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = FALSE   # TRUE: gbif df already calculated and saved
observReady     = FALSE  # TRUE: observ df already calculated and saved
locatReady      = FALSE  # TRUE: locations df already calculated and saved


