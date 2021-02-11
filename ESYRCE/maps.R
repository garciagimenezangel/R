library(dplyr)
library(gstat) 
library(sf)
rm(list=ls())
###########

# setwd("C:/Users/angel.gimenez/git/R/ESYRCE/")
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

# dataFolder = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/ESYRCE/"
# GEEFolder  = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"
dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
GEEFolder  = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"

# Read datasets
dataFile     = paste0(dataFolder, "geo_metrics_climate_intensif_pollService_20-12-18.csv")
df_data      = read.csv(dataFile, header=T)
modelFile    = paste0(dataFolder, "geo_model_20-12-18.csv")
df_pollModel = read.csv(modelFile, header=T)

# Regions
# fileShp = "C:/Users/angel.gimenez/DATA/Administrative areas/ESP_adm/ESP_adm2.shp"
fileShp = "C:/Users/angel/DATA/Administrative areas/ESP_adm/ESP_adm2.shp"

polys <- st_read(fileShp)
# Crop to peninsula and Baleares
box = c(xmin=-9.271338, xmax=4.308773, ymin=36.04188, ymax=43.76652)
polys = st_crop(polys, st_bbox(box)) 
polys$region = abbreviate(polys$NAME_1) # abbreviate names
polys$province = abbreviate(polys$NAME_2) # abbreviate names
df_data$region = abbreviate(df_data$region) # abbreviate names
df_data$province = abbreviate(df_data$province) # abbreviate names
df_pollModel$region = abbreviate(df_pollModel$region) # abbreviate names
df_pollModel$province = abbreviate(df_pollModel$province) # abbreviate names


#######
# MAPS 
#######
# FieldSize(sin disolver)
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_data[,c(baseCols,"avgFieldSize")]
df_metric$metric = df_metric[,"avgFieldSize"]
df_metric        = df_metric[df_metric$metric > 0,] # rule out plots with no fields (avg size=0)
# Mean
name = "Average \nField Size \n(ha)"
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)
# Slope
name = "Change in \nField Size \n(%/yr)"
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# FieldSize(dissolved)
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_data[,c(baseCols,"avgFieldSizeDiss")]
df_metric$metric = df_metric[,"avgFieldSizeDiss"]
df_metric        = df_metric[df_metric$metric > 0,] # rule out plots with no fields (avg size=0)
# Mean
name = "Average \nField Size \nDiss.(ha)"
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)
# Slope
name = "Change in \nField Size \nDiss.(%/yr)"
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)


# propAgri
name = "Average \nPercentage \nCropland"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_",agriLand)
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = rowSums(df_metric[,columns])
df_metric$metric = df_metric$metric*100 # convert to percentage
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)

name = "Change in \nCropland (%/yr)"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_",agriLand)
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = rowSums(df_metric[,columns])
df_metric$metric = df_metric$metric*100 # convert to percentage
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# seminatural
name = "Average \nPercentage \nSeminatural \nLand"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_",seminatural)
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = rowSums(df_metric[,columns])
df_metric$metric = df_metric$metric*100 # convert to percentage
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)

name = "Change in \nSeminatural \nLand (%/yr)"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_",seminatural)
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = rowSums(df_metric[,columns])
df_metric$metric = df_metric$metric*100 # convert to percentage
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# notAgri (artificial)
name = "Average \nPercentage \nArtificial \nLand"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_","notAgri")
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = df_metric[,columns]
df_metric$metric = df_metric$metric*100 # convert to percentage
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)

name = "Change in \nArtificial \nLand (%/yr)"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_","notAgri")
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = df_metric[,columns]
df_metric$metric = df_metric$metric*100 # convert to percentage
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# improductive
name = "Average \nPercentage \nImproductive \nLand"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_","improductive")
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = df_metric[,columns]
df_metric$metric = df_metric$metric*100 # convert to percentage
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)

name = "Change in \nImproductive \nLand (%/yr)"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_","improductive")
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = df_metric[,columns]
df_metric$metric = df_metric$metric*100 # convert to percentage
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# edgeDensityDiss 
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_data[,c(baseCols,"edgeDensityDiss")]
df_metric$metric = df_metric[,"edgeDensityDiss"]
# mean
name = "Average \nEdge Density \n(m/ha)"
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)
# slope
name = "Change in \nEdge Density \n(%/yr)"
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# edgeDensityFields 
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_data[,c(baseCols,"edgeDenFields")]
df_metric$metric = df_metric[,"edgeDenFields"]
# mean
name = "Average \nEdge Density \nFields (m/ha)"
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)
# slope
name = "Change in \nEdge Density \nFields (%/yr)"
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# heterogeneity
name = "Average \nCrop Diversity \n(#crops/ha)"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_data[,c(baseCols,"heterogeneity")]
df_metric$metric = df_metric[,"heterogeneity"]
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)

name = "Change in \nCrop Diversity \n(%/yr)"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_data[,c(baseCols,"heterogeneity")]
df_metric$metric = df_metric[,"heterogeneity"]
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# demand
name = "Average \nPollinators \nDemand"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_data[,c(baseCols,"demand")]
df_metric$metric = df_metric[,"demand"]
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)

name = "Change in \nPollinators' \nDemand \n(%/yr)"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_data[,c(baseCols,"demand")]
df_metric$metric = df_metric[,"demand"]
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# intensification
name = "Average \nIntensification \nScore"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_data[,c(baseCols,"intensification")]
df_metric$metric = rescaleVariable(df_metric[,"intensification"])
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = 0
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)

name = "Change in \nIntensification \nScore (%/yr)"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_data[,c(baseCols,"intensification")]
df_metric$metric = df_metric[,"intensification"]
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# abandoned
name = "Average \nPercentage \nAbandoned \nLand"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_",abandAgri)
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = rowSums(df_metric[,columns])
df_metric$metric = df_metric$metric*100 # convert to percentage
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)

name = "Change in \nAbandoned \nLand (%/yr)"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_",abandAgri)
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = rowSums(df_metric[,columns])
df_metric$metric = df_metric$metric*100 # convert to percentage
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAgg$slope = df_slopeAgg$slope *100 / df_meanAgg$mean # scale by the mean of the metric
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# Pollination score
name = "Average \nPollination \nScore"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_pollModel[,c(baseCols,"ZonasNaturales_man0_mod0")]
df_metric$metric = df_metric[,"ZonasNaturales_man0_mod0"]
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)

name = "Change in \nPollination \nScore \n(1/yr)"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
df_metric        = df_pollModel[,c(baseCols,"ZonasNaturales_man0_mod0")]
df_metric$metric = df_metric[,"ZonasNaturales_man0_mod0"]
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_pollModel[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# pollinators' independent/dependent crops
name = "Average \nPercentage \nPollinator \nDependent \nCrops"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_",pollDependent)
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = rowSums(df_metric[,columns])
df_metric$metric = df_metric$metric*100 # convert to percentage
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)

name = "Change in \nPollinator \nDependent \nCrops (%/yr)"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
columns          = paste0("prop_",pollDependent)
df_metric        = df_data[,c(baseCols,columns)]
df_metric$metric = rowSums(df_metric[,columns])
df_metric$metric = df_metric$metric*100 # convert to percentage
df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_slopeAgg      = merge(df_aggUnit, df_slope)
df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
sf_slopeAgg      = merge(polys,df_slopeAggMean)
ggplot(sf_slopeAgg) +
  geom_sf(data = sf_slopeAgg, aes(fill = slope))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)

# pollination service
name = "Average \nPollination \nService \nScore"
baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
demandThreshold  = 0.1 # set when calculating pollination service (pollService.R)
df_metric        = df_data[df_data$demand>demandThreshold,c(baseCols,"pollService")]
df_metric$metric = df_metric[,"pollService"]
df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_meanAgg       = merge(df_aggUnit, df_mean)
df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
sf_meanAgg       = merge(polys,df_meanAggMean)
midpoint         = mean(df_meanAggMean$mean)
ggplot(sf_meanAgg) +
  geom_sf(data = sf_meanAgg, aes(fill = mean))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)



########################
# Particular crops area
########################
df_crops = read.csv(paste0(dataFolder,"cropAreaByProvince.csv"), header=T)
years = seq(2001,2019)
crops = pollImportant[pollImportant %in% agriLand]
df_crops$area_crop = rowSums(df_crops[,paste0("area_",crops)]) # several columns
# df_crops$area_crop = df_crops[,paste0("area_",crops)] # one column
df_cropsYears = df_crops[df_crops$YEA %in% years,] %>% group_by(province) %>% summarise(sum_crop=sum(area_crop, na.rm = TRUE))
sf_metricYears = merge(polys,df_cropsYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = sum_crop))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

######################
# Field size analysis 
######################
# FieldSize(sin disolver)
# df_slope... and df_mean... calculated for field size
df_Jaen      = df_data[df_data$province == "Jaén",] # select negative slope
df_negSlope  = df_slope[df_slopeAgg$province == "Jaén" & df_slopeAgg$slope < 0,]
df_JaenNegSl = df_Jaen[df_Jaen$D2_NUM %in% df_negSlope$D2_NUM, prop_landcovertypes] # select data corresponding to negative slope in field size, and columns of land cover types
df_test = df_JaenNegSl
dominantLC   = colnames(df_test)[apply(df_JaenNegSl,1,which.max)]
qplot( abbreviate(gsub("prop_","",dominantLC)) ) 

df_sumLC = data.frame(as.list(colSums(df_test)))
colnames(df_sumLC) = abbreviate(gsub("prop_","",colnames(df_sumLC)))
df_sumLC <- as.data.frame(t(df_sumLC[,-1]))
colnames(df_sumLC) = "totalArea"
df_sumLC$landcover = rownames(df_sumLC)
df_sumLC = df_sumLC[df_sumLC$totalArea>100,]
(bp<-ggplot(data=df_sumLC, aes(x=landcover, y=totalArea)) +
    geom_bar(stat="identity"))

# get dominant land cover type



# r <- raster()
# extent(r)<-c(-9.271338, 4.308773, 36.04188, 43.76652)
# #reclasificacion de la variable para verla mejor en 3 categorias, incremento area (1), neutral (0), decrecimiento area
# df_slopesPoll$slope_recl<-df_slopesPoll$slope
# df_slopesPoll$slope_recl[df_slopesPoll$slope_recl<0]<--1
# df_slopesPoll$slope_recl[df_slopesPoll$slope_recl>0]<-1
# spr<-df_slopesPoll[,c("longitude","latitude","slope_recl")]
# head(spr)
# spr2<-as.data.frame(spr)
# coordinates(spr2) <- ~longitude+latitude
# r <- rasterize(spr2, r, 'slope_recl', fun=mean)
# plot(r, main="Poll score")




