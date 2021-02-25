library(dplyr)
library(gstat) 
rm(list=ls())
###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
GEEFolder  = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"

# Read datasets
dataFile     = paste0(dataFolder, "metrics_v2021-02-25.csv")
df_data      = read.csv(dataFile, header=T)
# modelFile    = paste0(dataFolder, "geo_model_20-12-18.csv")
# df_pollModel = read.csv(modelFile, header=T)


##########################################################################
# Add coordinates of the center of the segments, and associate with region
##########################################################################
# To avoid replication of calculations, get unique combinations of "D1_HUS" and "D2_NUM" and associate coordinates of the center of the segment (assume unique combinations have the same size of the segment, which in practice is not always true)
lookupCoord = calculateLookupCoord(df_data[,c("D1_HUS","D2_NUM","segArea")])
lookupCoord = lookupCoord %>% select(-c("segArea"))
# Add region associated to the coordinates
fileShp = "C:/Users/angel/DATA/Administrative areas/ESP_adm/ESP_adm2.shp"
map     = readOGR(fileShp, encoding='utf-8', use_iconv=TRUE)
lookupCoordRegions = do.call("rbind", apply(lookupCoord,1,calculateRegion))
# Some points lie on the sea. Use a buffer of the map to get the region
fileShp = "C:/Users/angel/DATA/Administrative areas/ESP_adm/ESP_adm2_buff.shp"
map     = readOGR(fileShp, encoding='utf-8', use_iconv=TRUE)
indNullRegions   = is.na(lookupCoordRegions$province)
nullRegions      = lookupCoordRegions[indNullRegions,]
nullCoordRegions = do.call("rbind", apply(nullRegions,1,calculateRegion))
lookupCoordRegions[indNullRegions,] = nullCoordRegions

# Read/Save file
write.csv(lookupCoordRegions, file=paste0(dataFolder,"intermediateProducts/lookup_coordinates_regions.csv"),row.names=FALSE)
lookupCoordRegions = read.csv(file=paste0(dataFolder,"intermediateProducts/lookup_coordinates_regions.csv"),header=T)

# Add coordinates and region by merging dataframe with lookup table
df_coords = merge(df_data, lookupCoordRegions, by=c("D1_HUS","D2_NUM"), all.x = TRUE)
write.csv(df_coords, file=paste0(dataFolder,"geo_metrics_20-12-18.csv"),row.names=FALSE)


#####################################
# Join data from pollination models
#####################################
# lookupCoordRegions = read.csv(paste0(dataFolder,"lookup_coordinates_regions.csv"), header=T)
modelName    = "ZonasNaturales_man0_mod0"
modelsByYear = c("ZonasNaturales2000_man0_mod0","ZonasNaturales2006_man0_mod0","ZonasNaturales2012_man0_mod0","ZonasNaturales2018_man0_mod0")
years        = c(2000,2006,2012,2018) # MUST correspond with modelNames
df_pollModel = data.frame(matrix(ncol = 8, nrow = 0))
colnames(df_pollModel) <- c(colnames(lookupCoordRegions), "YEA", modelName)
for (it in seq(1,length(years))) {
  # Read model values
  modelFile = paste0(GEEFolder, modelsByYear[it], ".csv") 
  df_model  = read.csv(modelFile, header=T)
  df_model  = df_model %>% dplyr::select(-c("system.index",".geo"))
  df_model  = df_model %>% rename_at(vars(c("first")), ~ c("modelValue"))
  # Define data frame to store the values of the model linked to segments (D1_HUS, D2_NUM)
  df_pollModelYear = lookupCoordRegions
  df_pollModelYear[,"YEA"]     = years[it]
  # Assign values to segments (D1_HUS, D2_NUM) using coordinates
  modelValues = apply(df_pollModelYear, 1, getModelValue)
  df_pollModelYear[,modelName] = modelValues
  df_pollModel = rbind(df_pollModel, df_pollModelYear)
}
write.csv(df_pollModel, file=paste0(dataFolder,"intermediateProducts/geo_model_21-02-25.csv"),row.names=FALSE)






