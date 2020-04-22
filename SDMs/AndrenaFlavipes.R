
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
yrFrom  = 1990
yrTo    = 2020
excludeInAbsenceSelection = c("Andrena") # any pollinator name that contains any of these strings is not considered as a candidate absence point
# excludeInAbsenceSelection = c("Andrena", "Andrena sp", "Andrena sp.", "Andrena sp. ", "Andrena sp. 4", "Andreninae", "Small Andrena sp.", "Large Andrena sp.") # any pollinator name equal to these strings is not considered as a candidate absence point

# Directories
observDir  = "C:/Users/angel.gimenez/Documents/REPOSITORIES/OBservData/Datasets_storage" 
sdmDir     = "C:/Users/angel.gimenez/Documents/DATA/OBServ/SDMs/"
speciesDir = paste(gsub(" ","_",species), "/", sep="")
dataDir    = paste(sdmDir, speciesDir, sep="");  
dir.create(file.path(sdmDir, speciesDir), showWarnings = FALSE)
                   
# Other settings
csvGbif         = "gbifData.csv"
csvObservInsect = "observInsectData.csv"
csvObservField  = "observFieldData.csv"
gbifReady    = TRUE
observReady  = TRUE
timeSpan     = paste("year=",stringr::str_c(yrFrom), sep="") %>% paste(",", sep="") %>% paste(stringr::str_c(yrTo), sep="")


###############################
# 1. Join GBIF and OBServ data to get a table of locations with presence and pseudo-absence data

# Load GBIF data: presence-only
if (gbifReady) {
  dfGbif = read.csv(file = paste(dataDir,csvGbif, sep=""), header = TRUE)
} else {
  # Download and save GBIF data
  pars = c(timeSpan)  # use dismo syntax for query
  splitted = strsplit(species, " ")
  genus = splitted[[1]][1]
  sp = splitted[[1]][2]
  dfGbif = gbif(genus, sp, args=pars, geo=TRUE)
  write.csv(dfGbif, paste(dataDir, csvGbif, sep=""), row.names=FALSE)
}

# Load OBServ data: presence and pseudo-absences (OBServ locations where the species is not observed)
if (observReady) {
  dfInsectSampling = read.csv(file = paste(dataDir, csvObservInsect, sep=""), header = TRUE)
  dfFieldData      = read.csv(file = paste(dataDir, csvObservField, sep=""), header = TRUE)
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
  write.csv(dfInsectSampling, paste(dataDir, csvObservInsect, sep=""), row.names=FALSE)

  # Field data
  indFieldData = lapply(observFiles, function(x){grepl("field_level_data",x)})
  indFieldData = unlist(indFieldData)
  fsFieldData  = observFiles[indFieldData]
  dfFieldData  = do.call(rbind, lapply(fsFieldData, function(file){ read.csv(file = file, header = TRUE) }))
  dfFieldData  = dfFieldData[dfFieldData$sampling_year >= yrFrom & dfFieldData$sampling_year <= yrTo ,]
  dfFieldData  = dfFieldData[, names(dfFieldData) %in% c("study_id", "site_id", "latitude","longitude", "abundance")]
  dfFieldData  = dfFieldData[complete.cases(dfFieldData),]
  names(dfFieldData)[names(dfFieldData) == "abundance"] <- "abundance_total"
  write.csv(dfFieldData, paste(dataDir, csvObservField, sep=""), row.names=FALSE)
  
}

# Presence data: column "presence" == 1
dfInsectPresence = dfInsectSampling[dfInsectSampling$pollinator %in% c(species, otherNames) ,]
dfInsectPresence["pollinator"]  = rep(species, nrow(dfInsectPresence)) # other_names -> species name
dfObservPresence = merge(dfInsectPresence, dfFieldData, by=c("study_id", "site_id"))
names(dfObservPresence)[names(dfObservPresence) == "longitude"] <- "lon"
names(dfObservPresence)[names(dfObservPresence) == "latitude"]  <- "lat"
dfGbifPresence   = dfGbif[c("lat","lon","country")]
dfGbifPresence["pollinator"]  = rep(species, nrow(dfGbifPresence))
dfPresence       = plyr::rbind.fill(dfObservPresence, dfGbifPresence)
dfPresence       = dfPresence[!is.na(dfPresence$lat) & !is.na(dfPresence$lon),]
dfPresence["presence"]  = rep(1, nrow(dfPresence))
# Remove duplicated locations
dfPresence = dfPresence[!duplicated(dfPresence[c("lon","lat")]),]

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
dfAbsence = dfAbsence[!is.na(dfAbsence$lat) & !is.na(dfAbsence$lon),]
dfAbsence["presence"]  = rep(0, nrow(dfAbsence))
# Take points within the convex hull of the presence points
conHull   = convHull(dfPresence[c("lon","lat")])
absPoints = SpatialPoints(cbind(dfAbsence$lon, dfAbsence$lat))
inConHull = array()
for(i in seq(1:length(absPoints))) {
  inConHull[i] = gContains(conHull@polygons, absPoints[i])
}
dfAbsence = dfAbsence[inConHull,]
# Remove duplicated locations
dfAbsence = dfAbsence[!duplicated(dfAbsence[c("lon","lat")]),]

# Bind presence and pseudo-absence locations
dfLocations = rbind(dfPresence[c("presence","pollinator","lon","lat")], dfAbsence[c("presence","pollinator","lon","lat")])


# Plot
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-10,30), ylim=c(30,70), axes=TRUE, col="light yellow")
# restore the box around the map
box()
# plot points
points(dfAbsence$lon, dfAbsence$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(dfAbsence$lon, dfAbsence$lat, col='red', cex=0.75)


###############################
# 2. Clean data (CoordinateCleaner)
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

# 5. PCA of the environmental variables

# 6. Apply model: maxent or logistic regression (or both and compare)

# 7. Derive landscape suitability from values given by the applied model

# 8. Calculate pollination service from the landscape suitability index.
 
# 9. Habitat preferences (https://bartomeuslab.com/2016/05/24/preferring-a-preference-index-ii-null-models/)
# 



