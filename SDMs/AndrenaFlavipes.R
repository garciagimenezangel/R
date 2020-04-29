
###############
rm(list=ls())
library(dismo)
library(dplyr)
library(CoordinateCleaner)
library(maptools)
library(rgeos)
library(sp)

###############################
# Main settings
species = "Andrena flavipes"
otherNames = c("Andrena.flavipes","Andrena_flavipes","Andrena_ flavipes")  # other names of the species in the OBServ database
yrFrom  = 1989
yrTo    = 2020
excludeInAbsenceSelection = c("Andrena") # any pollinator name that contains any of these strings is not considered as a candidate absence point
# excludeInAbsenceSelection = c("Andrena", "Andrena sp", "Andrena sp.", "Andrena sp. ", "Andrena sp. 4", "Andreninae", "Small Andrena sp.", "Large Andrena sp.") # any pollinator name equal to these strings is not considered as a candidate absence point

# Directories
observDir  = "C:/Users/angel.gimenez/Documents/REPOSITORIES/OBservData/Datasets_storage" 
sdmDir     = "C:/Users/angel.gimenez/Documents/DATA/OBServ/SDMs/"
speciesDir = paste0(gsub(" ","_",species), "/")
dataDir    = paste0(sdmDir, speciesDir);  
dir.create(file.path(sdmDir, speciesDir), showWarnings = FALSE)
                   
# Other settings
csvGbif         = "gbifData.csv"
csvObservInsect = "observInsectData.csv"
csvObservField  = "observFieldData.csv"
csvLocations    = "locations.csv"
csvCleanLoc     = "locationsCleaned.csv"
csvFeatures     = "features.csv"
gbifReady       = TRUE
observReady     = TRUE
locatReady      = TRUE
cleanReady      = FALSE
featuresReady   = FALSE
timeSpan        = paste0("year=",stringr::str_c(yrFrom),",",stringr::str_c(yrTo))
coords_digits   = 4

###############################
# 1. Join GBIF and OBServ data to get a table of locations with presence and pseudo-absence data

# Load GBIF data: presence-only
if (gbifReady) {
  dfGbif = read.csv(file = paste0(dataDir,csvGbif), header = TRUE)
} else {
  # Download and save GBIF data
  pars = c(timeSpan)  # use dismo syntax for query
  splitted = strsplit(species, " ")
  genus = splitted[[1]][1]
  sp = splitted[[1]][2]
  dfGbif = gbif(genus, sp, args=pars, geo=TRUE)
  write.csv(dfGbif, paste0(dataDir, csvGbif), row.names=FALSE)
}

# Load OBServ data: presence and pseudo-absences (OBServ locations where the species is not observed)
if (observReady) {
  dfInsectSampling = read.csv(file = paste0(dataDir, csvObservInsect), header = TRUE)
  dfFieldData      = read.csv(file = paste0(dataDir, csvObservField), header = TRUE)
} else {
  observFiles = list.files(observDir, full.names = TRUE)
  
  # Insect sampling
  indInsectSampling = lapply(observFiles, function(x){grepl("insect_sampling",x)})
  indInsectSampling = unlist(indInsectSampling)
  fsInsectSampling  = observFiles[indInsectSampling]
  dfInsectSampling  = do.call(rbind, lapply(fsInsectSampling, function(file){ read.csv(file = file, header = TRUE) }))
  dfInsectSampling  = dfInsectSampling[, names(dfInsectSampling) %in% c("study_id", "site_id", "pollinator","abundance")]
  dfInsectSampling  = dfInsectSampling[complete.cases(dfInsectSampling),]
  names(dfInsectSampling)[names(dfInsectSampling) == "abundance"] <- "abundance_species"
  write.csv(dfInsectSampling, paste0(dataDir, csvObservInsect), row.names=FALSE)

  # Field data
  indFieldData = lapply(observFiles, function(x){grepl("field_level_data",x)})
  indFieldData = unlist(indFieldData)
  fsFieldData  = observFiles[indFieldData]
  dfFieldData  = do.call(rbind, lapply(fsFieldData, function(file){ read.csv(file = file, header = TRUE) }))
  dfFieldData  = dfFieldData[dfFieldData$sampling_year >= yrFrom & dfFieldData$sampling_year <= yrTo ,]
  dfFieldData  = dfFieldData[, names(dfFieldData) %in% c("study_id", "site_id", "latitude","longitude", "abundance", "sampling_year")]
  dfFieldData  = dfFieldData[complete.cases(dfFieldData),]
  names(dfFieldData)[names(dfFieldData) == "abundance"] <- "abundance_total"
  write.csv(dfFieldData, paste0(dataDir, csvObservField), row.names=FALSE)
}

# Bind presence and pseudo-absence data
if (locatReady) {
  dfLocations = read.csv(file = paste0(dataDir,csvLocations), header = TRUE)
} else {
  
  ###########
  # Presence data: column "presence" == 1
  # Select subset data from OBServ and GBIF data
  dfInsectPresence = dfInsectSampling[dfInsectSampling$pollinator %in% c(species, otherNames) ,]
  dfObservPresence = merge(dfInsectPresence, dfFieldData, by=c("study_id", "site_id"))
  dfGbifPresence   = dfGbif[c("lat","lon","year")]
  
  # Manipulate names
  names(dfObservPresence)[names(dfObservPresence) == "longitude"] <- "lon"
  names(dfObservPresence)[names(dfObservPresence) == "latitude"]  <- "lat"
  names(dfGbifPresence)[names(dfGbifPresence) == "year"]  <- "sampling_year"

  # Bind GBIF and OBServ
  dfPresence       = plyr::rbind.fill(dfObservPresence, dfGbifPresence)
  dfPresence["presence"]    = rep(1, nrow(dfPresence))
  dfPresence["pollinator"]  = rep(species, nrow(dfPresence)) # other_names -> species name
  
  ###########
  # Pseudo-absence data: column "presence" == 0
  # Exclude species that show pattern $excludeInAbsenceSelection
  i <- 1
  dfToExclude = data.frame(matrix(ncol = length(names(dfInsectSampling)), nrow = 0))
  for (i in seq(1,length(excludeInAbsenceSelection))) {
    dfToExclude = rbind(dfToExclude, dfInsectSampling[grepl(excludeInAbsenceSelection[i], dfInsectSampling$pollinator) ,])
    #dfToExclude = rbind(dfToExclude, dfInsectSampling[excludeInAbsenceSelection[i] == dfInsectSampling$pollinator, ])
  }
  dfInsectAbsence = anti_join(dfInsectSampling, dfToExclude)
  dfInsectAbsence = dfInsectAbsence[dfInsectAbsence$abundance_species > 0,]
  
  # Select those locations (study_id, site_id) not present in dfPresence
  dfInsectAbsence = anti_join(dfInsectAbsence, dfInsectPresence, by=c("study_id", "site_id"))
  
  # Join tables by study_id and site_id and save
  dfAbsence = merge(dfInsectAbsence, dfFieldData, by=c("study_id", "site_id"))
  names(dfAbsence)[names(dfAbsence) == "longitude"] <- "lon"
  names(dfAbsence)[names(dfAbsence) == "latitude"]  <- "lat"
  dfAbsence["presence"]  = rep(0, nrow(dfAbsence))
  
  # Take points within the convex hull of the presence points
  conHull   = convHull(dfPresence[c("lon","lat")])
  absPoints = SpatialPoints(cbind(dfAbsence$lon, dfAbsence$lat))
  inConHull = array()
  for(i in seq(1:length(absPoints))) {
    inConHull[i] = gContains(conHull@polygons, absPoints[i])
  }
  dfAbsence = dfAbsence[inConHull,]
  
  # Remove NA and duplicated locations
  dfAbsence = dfAbsence[!is.na(dfAbsence$lat) & !is.na(dfAbsence$lon),]
  dfAbsence = dfAbsence[!duplicated(dfAbsence[c("lon","lat")]),]
  
  # Bind presence and pseudo-absence locations
  selectedCols = c("presence","pollinator","lon","lat","sampling_year")
  dfLocations = rbind(dfPresence[selectedCols], dfAbsence[selectedCols])
  
  # Remove NA and duplicated locations
  dfLocations = dfLocations[!is.na(dfLocations$lat) & !is.na(dfLocations$lon),]
  dfLocations$round_lon = round(dfLocations$lon, digits=coords_digits)
  dfLocations$round_lat = round(dfLocations$lat, digits=coords_digits)
  dfLocations = dfLocations[!duplicated(dfLocations[c("round_lon","round_lat")]),]
  dfLocations = dfLocations[selectedCols]
  write.csv(dfLocations, paste0(dataDir, csvLocations), row.names=FALSE)
}

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
# 2. Clean data (CoordinateCleaner)
if (cleanReady) {
  dfLocations = read.csv(file = paste0(dataDir, csvCleanLoc), header = TRUE)
} else {
  rownames(dfLocations)<-1:nrow(dfLocations)
  test <- clean_coordinates(x = dfLocations, lon = "lon", lat = "lat", species="pollinator",
                            tests = c("capitals", 
                                      "centroids",
                                      "equal", 
                                      "gbif", 
                                      "institutions", 
                                      "seas",
                                      "outliers",
                                      "zeros"))
  dfLocations = dfLocations[test$.summary,]
  write.csv(dfLocations, paste0(dataDir, csvCleanLoc), row.names=FALSE)
}

# 3. Sampling bias. Take absence points as locations in the OBServ datasets 
#   where the species has not bee observed (within the study region). In case this
#   doesn't work, we will need to take random background points and subsample
#   the presence locations.

# 4. Get environmental variables at the locations. Some might be ready off
#   the shelf (bioclim variables), but others will need to be calculated, 
#   such as the percentage of LC types within certain radius, which in turn 
#   can be computed according to the intertegular span of the insect.
#   The size of the surrounding area considered to be relevant for the insect
#   is proportional to its typical distance of flight. This distance can be 
#   derived from the inter-tegular (IT) span and "Greenleaf et al 2007 - Bee 
#   foraging ranges and their relationship to body size".
# Candidates:
# Growing days
# Habitat complexity
# Distance to semi-natural habitat: ver definición de SNH en http://www.fao.org/3/x0596e/x0596e01f.htm
# LC (porcentaje de los distintos tipos dentro de cada celda)
# Elevation
# Aspect
# Intensity of agriculture... (alguna métrica de ello)
# monthly average of minimum temperature
# monthly average of maximum temperature
# monthly average of mean temperature 
# annual precipitation
# isothermality (?)
# temperature seasonality (?)
# maximum temperature of warmest month
# minimum temperature of coldest month
# wettest month precipitation
# driest month precipitation
# Wind
# Soil composition (percentages of silt, sand and clay (loam?))
# Soil taxonomy
# Soil depth
# Soil temperature
# Soil moisture 
# Sentinel-5P OFFL NO2: Offline Nitrogen Dioxide? As a proxy of anthropogenic activities...(search tag pollution GEE->aerosols, CO,..) (ref: https://www.unenvironment.org/news-and-stories/story/celebrating-greatest-all-pollinators-bees)
# Vegetation Indices? NDVI or LAI
# Latitude
# Parámetro cantidad de bordes en parches de SNH (más luz, mejores en teoría)
# RESOLVE Ecoregions 2017
# potential evapotranspiration (ver label evapotranspiration en catálogos GEE)
# ...
# VER Bioclimatic Predictors from WorldClim:
#   https://worldclim.org/data/bioclim.html
if (featuresReady) {
  dfFeatures = read.csv(file = paste0(dataDir, csvFeatures), header = TRUE)
} else {
  featFiles = list.files(paste0(dataDir,"features"), full.names = TRUE);
  featNames = tools::file_path_sans_ext(list.files(paste0(dataDir,"features"), full.names = FALSE));
  for (i in seq(featFiles)) {
    dfFeature = read.csv(file = featFiles[i], header = TRUE)
    
    # Round coordinates to the specified level of precision
    dfFeature$longitude = round(dfFeature$longitude, digits=coords_digits)
    dfFeature$latitude = round(dfFeature$latitude, digits=coords_digits)
    
    # Drop not useful columns (inherited from the export in GEE)
    dfFeature = dfFeature %>% select(-c("system.index", ".geo"))
    
    # Remove duplicated locations 
    dfFeature = dfFeature[!duplicated(dfFeature[c("longitude","latitude")]),]

    # Rename feature column
    names
    names(dfFeature)[! names(dfFeature) %in% c("longitude","latitude","sampling_year")] <- featNames[i] 
        
    # Left join
    if (i==1) dfFeatures = dfFeature else dfFeatures = merge(dfFeature, dfFeatures, all.x = TRUE)
  }
  
  # TODO: HACER LA PARTE DE PORCENTAJES LANDCOVER. COLUMNA HISTOGRAM, PARSE TO DICTIONARY, CONVERTIR A LOS 15 LC TYPES GENERALES (TOMAR
  # PRIMEROS DOS DÍGITOS), Y CREAR UN FEATURE POR CADA UNO DE ELLOS
  
  write.csv(dfFeatures, paste0(dataDir, csvFeatures), row.names=FALSE)
}



# 5. PCA of the environmental variables (skip for the moment, try with all)

# 6. Apply model: maxent or logistic regression (or both and compare). Consider normalizing continuous variables. 

# 7. Derive landscape suitability from values given by the applied model

# 8. Calculate pollination service from the landscape suitability index.
 
# 9. Habitat preferences (https://bartomeuslab.com/2016/05/24/preferring-a-preference-index-ii-null-models/)
# 



