
#############################################################################################
# main.R: Join GBIF and OBServ data to get a table of locations with presence and pseudo-absence data
# INPUT: configuration file for species (e.g. "config_species/andrena_flavipes.R")
# OUTPUT: tables dfGbif, dfOBServInsectSampling, dfOBServFieldData, dfLocations
#############################################################################################

###############################
# Load libraries and set WD
rm(list=ls())
setwd("C:/Users/angel.gimenez/Documents/REPOSITORIES/R/SDMs/")
source("lib/dataFunctions.R")
library(maptools)
library(dplyr)
library(raster)
library(RWeka)
library(rlist)

# Load species configuration
source("config_species/bombus_terrestris.R")

###############################
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
  dfOBServFieldData$site_id = as.character(df$site_id)
  dfOBServFieldData$site_id[is.na(dfOBServFieldData$site_id)] = "noID"
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





