
# Main settings (normally, the only ones that should change among configuration files for different species)
species = "Andrena nitida"
otherNames = c("Andrena_ nitida", "Andrena_nitida", "Andrena.nitida")  # other names of the species in the OBServ database. See "OBservData\Thesaurus_Pollinators\organism_guild_META_STUDIES.csv"
yrFrom  = 1988
yrTo    = 2100
excludeNames = c("Andrena", "Andrena sp", "Andrena sp.", "Andrena sp. ", "Andrena sp. 4", "Andrenidae", "Small Andrena sp.", "Large Andrena sp.", "Andrena ssp.", "Andrena sp1", "Andrena_2_brittaindataset", "Andrena_3_brittaindataset", "Andrena_4_brittaindataset", "Andrena_sp.", "Large Andrena ap.", "Large Andrena sp.", "Small Andrena sp.", "Andrena Sp.", "Andrena sp2", "Andrena sp3", "Andrena sp4", "Andrena sp5", "Andrena sp6", "Andrena sp7") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = FALSE   # TRUE: gbif df already calculated and saved
observReady     = FALSE  # TRUE: observ df already calculated and saved
locatReady      = FALSE  # TRUE: locations df already calculated and saved


