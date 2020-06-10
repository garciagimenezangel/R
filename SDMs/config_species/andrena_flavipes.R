###############################
# Main settings
species = "Andrena flavipes"
otherNames = c("Andrena.flavipes","Andrena_flavipes","Andrena_ flavipes")  # other names of the species in the OBServ database
yrFrom  = 1988
yrTo    = 2020
#excludeNames = c("Andrena") # any pollinator name that contains any of these strings is not considered as a candidate absence point
excludeNames = c("Andrena", "Andrena sp", "Andrena sp.", "Andrena sp. ", "Andrena sp. 4", "Andreninae", "Small Andrena sp.", "Large Andrena sp.") # any pollinator name equal to these strings is not considered as a candidate absence point
gbifReady       = TRUE   # TRUE: gbif df already calculated and saved
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







