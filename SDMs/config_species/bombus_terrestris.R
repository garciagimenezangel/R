
# Main settings 
species = "Bombus terrestris"
otherNames = c("B. terrestris", "Bomus terrestris", "Bombus terrestris-complex","Bombus terrestris or lucorum", "Bombus terrestris/lucorum", "bombus.terrestris", "Bombus.terrestris", "Bombus_ terrestris", "Bombus_ terrestris-complex", "Bombus_ terrestris/lucorum","Bombus_terrestris","Bombus_terrestris_aggregate", "Bombus_terrestris-aggregate", "Bombus lucorum/terrestris","Bombus_lucorum_terrestris")  # other names of the species in the OBServ database. See "OBservData\Thesaurus_Pollinators\organism_guild_META_STUDIES.csv". Take combinations with "lucorum" as valid too (apparently, these two species are difficult to distinguish).
excludeNames = c("Bombus Sp.", "Bombus sp", "Bombus sp.", "Bombus spp", "Bombus", "Bombus lucorum", "Bombus.lucorum", "Bombus_6", "Bombus_7", "Bombus_lucorum", "Bombus_spp.", "Bombus lucorum agg") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = FALSE   # TRUE: gbif df already calculated and saved
observReady     = FALSE  # TRUE: observ df already calculated and saved
locatReady      = FALSE  # TRUE: locations df already calculated and saved


