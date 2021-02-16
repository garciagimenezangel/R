
# Main settings (normally, the only ones that should change among configuration files for different species)
species = "Bombus lapidarius"
otherNames = c("bombus.lapidarius", "Bombus.lapidarius", "Bombus_ lapidarius", "Bombus_lapidarius", "B. lapidarius", "B_lapidarius", "B..lapidarius")  # other names of the species in the OBServ database. See "OBservData\Thesaurus_Pollinators\organism_guild_META_STUDIES.csv"
excludeNames = c("Bombus Sp.", "Bombus sp", "Bombus sp.", "Bombus spp", "Bombus", "Bombus_6", "Bombus_7", "Bombus_spp.") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = FALSE   # TRUE: gbif df already calculated and saved
observReady     = FALSE  # TRUE: observ df already calculated and saved
locatReady      = FALSE  # TRUE: locations df already calculated and saved


