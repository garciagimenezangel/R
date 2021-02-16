
# Main settings (normally, the only ones that should change among configuration files for different species)
species = "Andrena minutula"
otherNames = c("Andrena_ minutula", "Andrena_minutula", "Andrena.minutula")  # other names of the species in the OBServ database. See "OBservData\Thesaurus_Pollinators\organism_guild_META_STUDIES.csv"
excludeNames = c("Andrena", "Andrena sp", "Andrena sp.", "Andrena sp. ", "Andrena sp. 4", "Andrenidae", "Small Andrena sp.", "Large Andrena sp.", "Andrena ssp.", "Andrena sp1", "Andrena_ cf. minutula", "Andrena_2_brittaindataset", "Andrena_3_brittaindataset", "Andrena_4_brittaindataset", "Andrena_minutula_aggregate", "Andrena_sp.", "Large Andrena ap.", "Large Andrena sp.", "Small Andrena sp.", "Andrena_minutula-aggregate", "Andrena sp. aff. minutula", "Andrena Sp.", "Andrena sp2", "Andrena sp3", "Andrena sp4", "Andrena sp5", "Andrena sp6", "Andrena sp7", "Andrena minutula-aggregate") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = FALSE   # TRUE: gbif df already calculated and saved
observReady     = FALSE  # TRUE: observ df already calculated and saved
locatReady      = FALSE  # TRUE: locations df already calculated and saved


