# 
# The configuration files in this folder must contain the following options:

# Main settings (normally, the only ones that should change among configuration files for different species)
species = "Bombus pascuorum"
otherNames = c("bombus.pascuorum", "Bombus.pascuorum", "Bombus_ pascuorum", "Bombus_pascuorum", "B. pascuorum", "B_pascuorum", "B..pascuorum")  # other names of the species in the OBServ database. See "OBservData\Thesaurus_Pollinators\organism_guild_META_STUDIES.csv"
yrFrom  = 1988
yrTo    = 2100
excludeNames = c("Bombus Sp.", "Bombus sp", "Bombus sp.", "Bombus spp", "Bombus", "Bombus_6", "Bombus_7", "Bombus_spp.", "Bombus_sp") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = FALSE   # TRUE: gbif df already calculated and saved
observReady     = FALSE  # TRUE: observ df already calculated and saved
locatReady      = FALSE  # TRUE: locations df already calculated and saved
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
