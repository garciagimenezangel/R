
###############
rm(list=ls())
setwd("C:/Users/angel.gimenez/Documents/REPOSITORIES/R/SDMs/")
source("lib/dataFunctions.R")
library(maptools)
library(dplyr)
library(raster)
library(RWeka)
library(rlist)

###############################
# Load species configuration
source("config_species/andrena_flavipes.R")

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
  # Some site_id's = NA. Replace by ""
  df$site_id = as.character(df$site_id)
  df$site_id[is.na(df$site_id)] = "noID"
  dfOBServFieldData = clean(dfOBServFieldData, yrFrom, yrTo, minCoordDig, lon="longitude", lat="latitude", species="")
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
  
  # Further cleaning
  dfLocations = clean(dfLocations, yrFrom, yrTo, minCoordDig)
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
# Growing days, Habitat complexity, Distance to semi-natural habitat: ver definici�n de SNH en http://www.fao.org/3/x0596e/x0596e01f.htm,
# Intensity of agriculture... (alguna m�trica expl�cita), Par�metro cantidad de bordes en parches de SNH (m�s luz, mejores en teor�a), RESOLVE Ecoregions 2017,
# Sentinel-5P OFFL NO2: Offline Nitrogen Dioxide? As a proxy of anthropogenic activities...(search tag pollution GEE->aerosols, CO,..) (ref: https://www.unenvironment.org/news-and-stories/story/celebrating-greatest-all-pollinators-bees)
# For data with medium-high resolution, such as CORINE (100m), pixel size is shorter than the typical distance of flight of most pollinators. Therefore, it is better to take into account the vicinity of the records' locations. For CORINE, we do that using histograms where the number of each LC type is registered, within a buffer area corresponding to the typical distance of flight of the species, using IT span and Greenleaf et al. (2007). Then, transform those histograms into usable features of percentage of major LC types

# 2 possibilities: 
# i)  get data from ALIGNED rasters (same dimensions and resolution) in th ROI, exported in GEE 
# ii) get data extracted at the locations with GEE, stored in the folders 'features' and 'histograms'
if (useRasters) {
  fsRastersCommon = list.files(paste0(rastersDir,"common_aligned"), full.names = TRUE, pattern = ".tif", recursive = FALSE)
  fsRastersAndrenaFlavipes = list.files(paste0(rastersDir,"Andrena flavipes"), full.names = TRUE, pattern = ".tif", recursive = FALSE)
  fsRasters = list.append(fsRastersAndrenaFlavipes, fsRastersCommon)
  rasters = lapply(fsRasters, function(x){raster(x)})
  predictors  = stack(rasters)
  pres = dfLocations[dfLocations$presence == 1,c("lon","lat")]
  abs  = dfLocations[dfLocations$presence == 0,c("lon","lat")]
  presvals <- extract(predictors, pres)
  absvals  <- extract(predictors, abs)
  
} else {
  # 3.2) FEATURES
  if (featuresReady) {
    dfFeatures = read.csv(file = paste0(dataDir, csvFeatures), header = TRUE)
  } else {
    # Get features and histograms from csv's created in GEE
    dfFeatures = getFeatures(featDir, removePatterns)
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
    dfLocations$lon = round(dfLocations$lon, digits=coordsDigits)
    dfLocations$lat = round(dfLocations$lat, digits=coordsDigits)
    dfFeatures = merge(dfLocations, dfFeatures, by=c("lon","lat"))
    write.csv(dfFeatures, paste0(dataDir, csvFeatures), row.names=FALSE)
  }
}

# Save data to arff files
dfDataModel = dfFeatures %>% select(-c("lon","lat","pollinator","sampling_year","source")) 
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

# Test prediction with logistic model
library(randomForest)
pres = dfFeatures[dfFeatures$presence == 1,c("lon","lat")]
abs  = dfFeatures[dfFeatures$presence == 0,c("lon","lat")]
presvals <- extract(predictors, pres)
absvals  <- extract(predictors, abs)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdataNorm <- Normalize(~., data = sdmdata) # RWeka normalization, range [0,1]
formula <- as.formula(paste("pb ~", paste(names(sdmdataNorm)[-1],collapse="+")))
model = glm(formula, data = sdmdataNorm, family = binomial("logit"), maxit = 100)  
model = randomForest(formula, data = sdmdataNorm)  
p <- predict(predictors, model)
plot(p)

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










