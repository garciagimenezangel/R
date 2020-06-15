
# Main settings 
species = "Bombus terrestris"
otherNames = c("B. terrestris", "Bomus terrestris", "Bombus terrestris-complex","Bombus terrestris or lucorum", "Bombus terrestris/lucorum", "bombus.terrestris", "Bombus.terrestris", "Bombus_ terrestris", "Bombus_ terrestris-complex", "Bombus_ terrestris/lucorum","Bombus_terrestris","Bombus_terrestris_aggregate", "Bombus_terrestris-aggregate", "Bombus lucorum/terrestris","Bombus_lucorum_terrestris")  # other names of the species in the OBServ database. See "OBservData\Thesaurus_Pollinators\organism_guild_META_STUDIES.csv". Take combinations with "lucorum" as valid too (apparently, these two species are difficult to distinguish).
yrFrom  = 1988
yrTo    = 2100
excludeNames = c("Bombus Sp.", "Bombus sp", "Bombus sp.", "Bombus spp", "Bombus", "Bombus lucorum", "Bombus.lucorum", "Bombus_6", "Bombus_7", "Bombus_lucorum", "Bombus_spp.", "Bombus lucorum agg") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = TRUE   # TRUE: gbif df already calculated and saved
observReady     = TRUE  # TRUE: observ df already calculated and saved
locatReady      = TRUE  # TRUE: locations df already calculated and saved
featuresReady   = FALSE  # TRUE: features df already calculated and saved
useRasters      = FALSE  # TRUE: use collection of rasters to extract features 

# Directories
observDir  = "C:/Users/angel.gimenez/Documents/REPOSITORIES/OBservData/Datasets_storage"
sdmDir     = "C:/Users/angel.gimenez/Documents/DATA/OBServ/SDMs/"
speciesDir = paste0(gsub(" ","_",species), "/")
dataDir    = paste0(sdmDir, speciesDir);  
dir.create(file.path(sdmDir, speciesDir), showWarnings = FALSE)
featDir    = paste0(dataDir, "features/");
histoDir   = paste0(dataDir, "histograms/");
rastersDir = paste0(sdmDir, "rasters/");

# Other settings
csvGbif         = "gbifData.csv"
csvObservInsect = "observInsectData.csv"
csvObservField  = "observFieldData.csv"
csvLocations    = "locations.csv"
csvFeatures     = "features.csv"
timeSpan        = paste0("year=",stringr::str_c(yrFrom),",",stringr::str_c(yrTo))
coordsDigits    = 4
minCoordDig     = 3
removePatterns  = c("first_","_mean","_monthly_mean", "histogram_")
###############################
