library(dplyr)
library(gstat) 

###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/Analysis/2020-12/"
GEEFolder  = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"

# Read dataset
dataFile = paste0(dataFolder, "geo_model_metrics_20-12-18.csv")
df_data  = read.csv(dataFile, header=T)


###############################
# Calculate slopes by category
###############################
# The function calculateSlope will aggregate those columns stored in the variable "columns" before the call to the function

# Proportion of seminatural area
columns = paste0("prop_",seminatural)
df_slopeSeminatural = df_data %>% group_by(longitude, latitude, province, region) %>%  do(data.frame(slope=calculateSlope(., columns)))
df_slopeSeminatural = df_slopeSeminatural[!is.na(df_slopeSeminatural$slope),]
boxplot(df_slopeSeminatural$slope ~ df_slopeSeminatural$region, range=100)
mean1000 = function(x){
  return(mean(x)*1000)
}
aggregate(df_slopeSeminatural[,"slope"], by=list(df_slopeSeminatural$region), FUN=mean1000)

# Pollination potential
columns = c("ZonasNaturales2018_man0_mod0")
df_pollPot = df_data %>% group_by(longitude, latitude, province, region) %>%  do(data.frame(slope=calculateSlope(., columns)))
df_pollPot = df_pollPot[!is.na(df_pollPot$slope),]
boxplot(df_pollPot$slope ~ df_pollPot$region, range=100)
mean1000 = function(x){
  return(mean(x)*1000)
}
aggregate(df_pollPot[,"slope"], by=list(df_pollPot$region), FUN=mean1000)

# Field size
columns = c("avgFieldSizeDiss")
df_slopeFieldSize = df_data %>% group_by(longitude, latitude, province, region) %>%  do(data.frame(slope=calculateSlope(., columns)))

#...


##########################################################################
# Add coordinates of the center of the segments, and associate with region
##########################################################################
# To avoid replication of calculations, get unique combinations of "D1_HUS" and "D2_NUM" and associate coordinates of the center of the segment (assume unique combinations have the same size of the segment, which in practice is not always true)
lookupCoord = calculateLookupCoord(df_data[,c("D1_HUS","D2_NUM","segArea")])
lookupCoord = lookupCoord %>% select(-c("segArea"))
# Add region associated to the coordinates
fileShp = "C:/Users/angel/DATA/Administrative areas/ESP_adm/ESP_adm2.shp"
map     = readOGR(fileShp)
lookupCoordRegions = do.call("rbind", apply(lookupCoord,1,calculateRegion))
# Some points lie on the sea. Use a buffer of the map to get the region
fileShp = "C:/Users/angel/DATA/Administrative areas/ESP_adm/ESP_adm2_buff.shp"
map     = readOGR(fileShp)
indNullRegions   = is.na(lookupCoordRegions$province)
nullRegions      = lookupCoordRegions[indNullRegions,]
nullCoordRegions = do.call("rbind", apply(nullRegions,1,calculateRegion))
lookupCoordRegions[indNullRegions,] = nullCoordRegions

# Save file
write.csv(lookupCoordRegions, file=paste0(dataFolder,"lookup_coordinates_regions.csv"),row.names=FALSE)

# Add coordinates and region by merging dataframe with lookup table
df_coords = merge(df_data, lookupCoordRegions, by=c("D1_HUS","D2_NUM"), all.x = TRUE)
write.csv(df_coords, file=paste0(dataFolder,"geo_metrics_20-12-18.csv"),row.names=FALSE)


#####################################
# Join data from pollination models
#####################################
df_coords = read.csv(paste0(dataFolder,"geo_metrics_20-12-18.csv"), header=TRUE)
df_modeldata = df_coords %>% rename_at(vars(c("x_center","y_center")), ~ c("longitude","latitude"))
df_modeldata[,c("latitude","longitude")] = as.data.frame(apply(df_modeldata[,c("latitude","longitude")], 2, as.numeric))
modelNames = c("ZonasNaturales2000_man0_mod0","ZonasNaturales2006_man0_mod0","ZonasNaturales2012_man0_mod0","ZonasNaturales2018_man0_mod0") 
for (modelName in modelNames) {
  df_modeldata[,modelName] = NA
  modelFile = paste0(GEEFolder, modelName, ".csv") 
  df_model  = read.csv(modelFile, header=T)
  df_model  = df_model %>% select(-c("system.index",".geo"))
  df_model  = df_model %>% rename_at(vars(c("first")), ~ c(modelName))
  digits = 5
  while(digits>2) { 
    indNotFound = is.na(df_modeldata[,modelName])  # some points are not found, decrease precision and try again
    df_aux = df_modeldata[indNotFound,c("longitude","latitude")]
    df_aux = addModelValues(df_aux, df_model, digits) 
    df_modeldata[indNotFound, modelName] = df_aux[,modelName]
    digits = digits-1
  }
}
write.csv(df_modeldata, file=paste0(dataFolder,"geo_model_metrics_20-12-18.csv"),row.names=FALSE)


#######################
# Calculate variograms
#######################
randomPt = sample_n(df_modeldata, 1)
dist_deg = 0.5
xmax = randomPt$longitude + dist_deg
xmin = randomPt$longitude - dist_deg
ymax = randomPt$latitude + dist_deg
ymin = randomPt$latitude - dist_deg

# Seminatural
neighbours = subset(df_modeldata, longitude>xmin & longitude<xmax & latitude>ymin & latitude<ymax)
prop_seminatural_cols  = paste0("prop_",seminatural)
neighbours["prop_seminatural"] = rowSums(neighbours[,prop_seminatural_cols])
coordinates(neighbours)= ~ longitude+latitude
variogramNeighbors = variogram(prop_seminatural~1, data=neighbours)
plot(variogramNeighbors)

# Cropfields
neighbours = subset(df_modeldata, longitude>xmin & longitude<xmax & latitude>ymin & latitude<ymax)
prop_agriLand_cols  = paste0("prop_",agriLand)
neighbours["prop_agriLand"] = rowSums(neighbours[,prop_agriLand_cols])
coordinates(neighbours)= ~ longitude+latitude
variogramNeighbors = variogram(prop_agriLand~1, data=neighbours)
plot(variogramNeighbors)

# Individual crop, for example maize
neighbours = subset(df_modeldata, longitude>xmin & longitude<xmax & latitude>ymin & latitude<ymax)
coordinates(neighbours)= ~ longitude+latitude
variogramNeighbors = variogram(prop_maize~1, data=neighbours)
plot(variogramNeighbors)

# Model
neighbours = subset(df_modeldata, longitude>xmin & longitude<xmax & latitude>ymin & latitude<ymax)
coordinates(neighbours)= ~ longitude+latitude
variogramNeighbors = variogram(ZonasNaturales2018_man0_mod0~1, data=neighbours)
plot(variogramNeighbors)


