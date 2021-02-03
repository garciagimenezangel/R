
# Years
yrFrom  = 1985
yrTo    = 2100

# Directories
observDir  = "C:/Users/angel/git/OBservData/Final_Data/"
sdmDir     = "C:/Users/angel/DATA/SDMs/v2-Feb2021/"
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
