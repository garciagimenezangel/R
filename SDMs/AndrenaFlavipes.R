
###############
rm(list=ls())
setwd("C:/Users/angel.gimenez/Documents/REPOSITORIES/R/SDMs/")
source("lib/dataFunctions.R")
library(maptools)
library(dplyr)
library(raster)
library(RWeka)

###############################
# Main settings
species = "Andrena flavipes"
otherNames = c("Andrena.flavipes","Andrena_flavipes","Andrena_ flavipes")  # other names of the species in the OBServ database
yrFrom  = 1988
yrTo    = 2020
#excludeNames = c("Andrena") # any pollinator name that contains any of these strings is not considered as a candidate absence point
excludeNames = c("Andrena", "Andrena sp", "Andrena sp.", "Andrena sp. ", "Andrena sp. 4", "Andreninae", "Small Andrena sp.", "Large Andrena sp.") # any pollinator name equal to these strings is not considered as a candidate absence point

# Directories
observDir  = "C:/Users/angel.gimenez/Documents/REPOSITORIES/OBservData/Datasets_storage"
sdmDir     = "C:/Users/angel.gimenez/Documents/DATA/OBServ/SDMs/"
speciesDir = paste0(gsub(" ","_",species), "/")
dataDir    = paste0(sdmDir, speciesDir);  
dir.create(file.path(sdmDir, speciesDir), showWarnings = FALSE)
featDir    = paste0(dataDir, "features/");
histoDir   = paste0(dataDir, "histograms/");
rastersDir = paste0(sdmDir, "rasters/");
refRastDir = paste0(rastersDir, "reference rasters/");

# Other settings
csvGbif         = "gbifData.csv"
csvObservInsect = "observInsectData.csv"
csvObservField  = "observFieldData.csv"
csvLocations    = "locations.csv"
csvFeatures     = "features.csv"
gbifReady       = TRUE
observReady     = TRUE
locatReady      = TRUE
featuresReady   = FALSE
useRasters      = FALSE
timeSpan        = paste0("year=",stringr::str_c(yrFrom),",",stringr::str_c(yrTo))
coords_digits   = 4
removePatterns  = c("first_","_mean","_monthly_mean", "histogram_")
###############################


###############################
# 1. Join GBIF and OBServ data to get a table of locations with presence and pseudo-absence data
# 2. Clean data (CoordinateCleaner)
# Load GBIF data: presence-only
if (gbifReady) {
  dfGbif = read.csv(file = paste0(dataDir,csvGbif), header = TRUE)
} else {
  # Download and save GBIF data
  dfGbif = downloadGbifData(species, timeSpan)
  write.csv(dfGbif, paste0(dataDir, csvGbif), row.names=FALSE)
}

# Load OBServ data: presence and pseudo-absences (OBServ locations where the species is not observed)
if (observReady) {
  dfOBServInsectSampling = read.csv(file = paste0(dataDir, csvObservInsect), header = TRUE)
  dfOBServFieldData      = read.csv(file = paste0(dataDir, csvObservField), header = TRUE)
} else {
  # Insect sampling
  dfOBServInsectSampling = getOBServInsectSampling(observDir)
  write.csv(dfOBServInsectSampling, paste0(dataDir, csvObservInsect), row.names=FALSE)

  # Field data
  dfOBServFieldData = getOBServFieldData(observDir)
  write.csv(dfOBServFieldData, paste0(dataDir, csvObservField), row.names=FALSE)
}

# Bind presence and pseudo-absence data
if (locatReady) {
  dfLocations = read.csv(file = paste0(dataDir,csvLocations), header = TRUE)
} else {
  # Presence data: column "presence" == 1
  dfPresence = getPresenceData(species, dfOBServInsectSampling, dfOBServFieldData, dfGbif, otherNames)
  
  # Pseudo-absence data: column "presence" == 0
  dfAbsence = getAbsenceData(dfPresence, dfOBServInsectSampling, dfOBServFieldData, excludeNames)
  
  # Bind presence and pseudo-absence locations
  selectedCols = c("presence","pollinator","lon","lat","sampling_year","source")
  dfLocations = rbind(dfPresence[selectedCols], dfAbsence[selectedCols])
  
  # Remove NA and duplicated locations
  dfLocations = removeNAandDupLocations(dfLocations, "lon", "lat", selectedCols, coords_digits)
  
  # Further cleaning
  rownames(dfLocations)<-1:nrow(dfLocations)
  clean <- clean_coordinates(x = dfLocations, lon = "lon", lat = "lat", species="pollinator",
                             tests = c("capitals", "centroids", "equal", "gbif", "institutions", "seas", "outliers", "zeros"))
  dfLocations = dfLocations[clean$.summary,]
  write.csv(dfLocations, paste0(dataDir, csvLocations), row.names=FALSE)
}
###############################


# # Plot
# data(wrld_simpl)
# plot(wrld_simpl, xlim=c(-10,30), ylim=c(30,70), axes=TRUE, col="light yellow")
# # restore the box around the map
# box()
# # plot points
# points(dfLocations$lon, dfLocations$lat, col='orange', pch=20, cex=0.75)
# # plot points again to add a border, for better visibility
# points(dfLocations$lon, dfLocations$lat, col='red', cex=0.75)


###############################
# 3. Get environmental variables at the locations. 
# Candidates not taken into account (to date 2020/05/11):
# Growing days, Habitat complexity, Distance to semi-natural habitat: ver definición de SNH en http://www.fao.org/3/x0596e/x0596e01f.htm,
# Intensity of agriculture... (alguna métrica explícita), Parámetro cantidad de bordes en parches de SNH (más luz, mejores en teoría), RESOLVE Ecoregions 2017,
# Sentinel-5P OFFL NO2: Offline Nitrogen Dioxide? As a proxy of anthropogenic activities...(search tag pollution GEE->aerosols, CO,..) (ref: https://www.unenvironment.org/news-and-stories/story/celebrating-greatest-all-pollinators-bees)

# 2 possibilities: 
# i)  get data from rasters in the study region (large portion of Europe), exported in GEE 
# ii) get data extracted at the locations with GEE, stored in the folders 'features' and 'histograms' (dfFeatures)
if (useRasters) {
  # 3.1) RASTER STACK
  # Make a stack of rasters, which must be prepared in advanced.
  #   - GEE gives tiles that must be merged->(otherScripts/mergeRasters.R)
  #   - Rasters must be resampled to the same extent and resolution
  fsRasters = list.files(rastersDir, full.names = TRUE, pattern = ".tif", recursive = FALSE)
  fsRefRast = list.files(refRastDir, full.names = TRUE, pattern = ".tif", recursive = FALSE)
  rasters = lapply(fsRasters, function(x){raster(x)})
  refRast = lapply(fsRefRast, function(x){raster(x)})
  rasters = lapply(rasters, function(x) { projectRaster(x, refRast[[1]], method='bilinear', alignOnly = TRUE) })
  rasters = list.append(refRast, rasters)
  rStack  = stack(rasters)
  
} else {
  # 3.2) FEATURES
  if (featuresReady) {
    dfFeatures = read.csv(file = paste0(dataDir, csvFeatures), header = TRUE)
  } else {
    dfFeatures = getFeatures(featDir, removePatterns)
    
    # For data with medium-high resolution, such as CORINE (100m), pixel size is shorter than the typical distance of 
    # flight of most pollinators. Therefore, it is better to take into account the vicinity of the records' locations.
    # For CORINE, we do that using histograms where the number of each LC type is registered, within a buffer area
    # corresponding to the typical distance of flight of the species, using IT span and Greenleaf et al. (2007). Then,
    # the next code snippet transform those histograms into usable features of percentage of major LC types
    dfHistos = getHistos(histoDir, removePatterns)
    
    # Get features from CORINE histograms
    dfCORINE = dfHistos %>% select(c("CORINE","longitude","latitude"))
    dfLCperc = do.call(rbind, apply(dfCORINE, 1, getCorinePercentages))
    
    # Merge features
    dfFeatures = merge(dfFeatures, dfLCperc, by=c("longitude","latitude"))
    dfFeatures = dfFeatures[complete.cases(dfFeatures),]
    
    # Add locations
    names(dfFeatures)[names(dfFeatures) == "longitude"] <- "lon"
    names(dfFeatures)[names(dfFeatures) == "latitude"]  <- "lat"
    dfLocations$lon = round(dfLocations$lon, digits=coords_digits)
    dfLocations$lat = round(dfLocations$lat, digits=coords_digits)
    dfFeatures = merge(dfLocations, dfFeatures, by=c("lon","lat"))
    write.csv(dfFeatures, paste0(dataDir, csvFeatures), row.names=FALSE)
  }
  
}

# Save data to arff files
dfDataModel = dfFeatures %>% select(-c("lon","lat","pollinator","sampling_year")) 
dfDataModel$presence = as.factor(dfDataModel$presence)
dfDataModel$b0_soilText = as.factor(dfDataModel$b0_soilText)
dfDataModel$b10_soilText = as.factor(dfDataModel$b10_soilText)
dfDataModel$b30_soilText = as.factor(dfDataModel$b30_soilText)
dfDataModel$soil_taxonomy = as.factor(dfDataModel$soil_taxonomy)
dfDataModel$landforms = as.factor(dfDataModel$landforms)

dfContinuous = dfDataModel %>% select(-c("b0_soilText","b10_soilText","b30_soilText","landforms","soil_taxonomy"))
dfContinuousNorm <- Normalize(~., data = dfContinuous) # RWeka normalization, range [0,1]

# Save to .arff to use in WEKA
write.arff(dfContinuousNorm, file = paste0(dataDir, "continuousDataNorm.arff"))
write.arff(dfDataModel, file = paste0(dataDir, "fullData.arff"))
###############################



###############################
# (skip, for now, PCA or any other selection of the environmental variables)
# 4. Apply models with different approaches
# Bioclim
presvals = dfContinuousNorm[dfContinuousNorm$presence == 1,-c(1)]
absvals  = dfContinuousNorm[dfContinuousNorm$presence == 0,-c(1)]
presvals
bc <- bioclim(presvals)

# GLM
group <- kfold(dfContinuousNorm, 5)
train <- dfContinuousNorm[group != 1, ]
test  <- dfContinuousNorm[group == 1, ]
formula <- as.formula(paste("presence ~", paste(names(dfContinuousNorm)[-1],collapse="+")))
model = glm(formula, data = train, family = binomial("logit"), maxit = 100)
predict <- predict(model, test, type = 'response')

# BayesGLM
library(arm)
group <- kfold(dfContinuousNorm, 5)
train <- dfContinuousNorm[group != 1, ]
test  <- dfContinuousNorm[group == 1, ]
formula <- as.formula(paste("presence ~", paste(names(dfContinuousNorm)[-1],collapse="+")))
model = bayesglm(formula, data = train, family="binomial")
predict <- predict(model, test, type = 'response')

# confusion matrix
table_mat <- table(test$presence, predict > 0.8)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]# false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

prec <- precision(table_mat)
prec
rec <- recall(table_mat)
rec

library(ROCR)
ROCRpred <- prediction(predict, test$presence)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))
###############################

###############################
# 5. Model evaluation
###############################

###############################
# 6. Derive landscape suitability from values given by the applied model
###############################

###############################
# 7. Calculate pollination service from the landscape suitability index.
###############################

###############################
# 8. Habitat preferences (https://bartomeuslab.com/2016/05/24/preferring-a-preference-index-ii-null-models/)
###############################










