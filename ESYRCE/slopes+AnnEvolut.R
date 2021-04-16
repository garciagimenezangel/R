library(dplyr)
library(gstat) 
library(sf)
library(ggplot2)
library(scales)
library(raster)
library(rasterVis)
library(wesanderson)
library(cowplot)
library(rlist)
rm(list=ls())
###########

# setwd("C:/Users/angel.gimenez/git/R/ESYRCE/")
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

# dataFolder = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/ESYRCE/"
dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
figuresFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/figures/"

# Read datasets
dataFile     = paste0(dataFolder, "metrics_v2021-02-25.csv")
df_data      = read.csv(dataFile, header=T)

############################
# Slopes 
############################
getSlope = function(columns, outcolname, isOneColumn = FALSE, minThresh=-1e100, maxThresh=1e100, columnThresh="") {
  baseCols           = c("D1_HUS", "D2_NUM", "province", "YEA")
  df_metric          = df_data[,c(baseCols,columns)]
  if(isOneColumn) {
    df_metric$metric = df_metric[,columns]
  } else {
    df_metric$metric = rowSums(df_metric[,columns])
  }
  if (columnThresh == "") { # use metric to set thresholds
    notValid = (df_metric$metric <= minThresh) | (df_metric$metric >= maxThresh)
    df_metric$metric[notValid] = NA
  } else { # use columnThresh to set thresholds
    notValid = (df_metric$columnThresh <= minThresh) | (df_metric$columnThresh >= maxThresh)
    df_metric$metric[notValid] = NA
  }  
  df_slope = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
  names(df_slope)[names(df_slope) == 'slope'] <- outcolname
  return(df_slope)
}

# LANDCOVER
# Cropland
cropland = getSlope(paste0("prop_",agriLand), "slope_cropland")
# seminatural
seminatural = getSlope(paste0("prop_",seminatural), "slope_seminatural")
# notAgri (artificial)
notAgri = getSlope("prop_notAgri", "slope_artificial", isOneColumn = TRUE)
# edgeDensityDiss 
edgeDensityDiss = getSlope("edgeDensityDiss", "slope_edgeDensityDiss", isOneColumn = TRUE)

# CROPLAND metrics
# FieldSize(sin disolver)
avgFieldSize = getSlope("avgFieldSize","slope_avgFieldSize", isOneColumn=TRUE, minThresh=0)
# Diversity
diversity = getSlope("cropsPerCropHa", "slope_cropsPerCropHa", isOneColumn = TRUE, minThresh=0)
# Intensification
intensification = getSlope("intensification", "slope_intensification", isOneColumn = TRUE)

# POLLINATORS metrics
# Threshold cropsPerCropHa>0 equals to the condition set in "mergeNewMetrics.R": cropArea > cropAreaThreshold (initially 0.5 hectares)
# Demand
demand = getSlope("demand","slope_demand", isOneColumn=TRUE, minThresh=0, columnThresh="cropsPerCropHa")
# Poll score
pollScore = getSlope("pollScore","slope_pollScore", isOneColumn=TRUE, minThresh=0, columnThresh="cropsPerCropHa")
# Poll service
pollService = getSlope("pollService2","slope_pollService2", isOneColumn=TRUE, minThresh=0, columnThresh="cropsPerCropHa")
# pollinators' independent/dependent crops
croplandDependent = getSlope(paste0("prop_",pollDependent),"slope_pollDependent")
croplandNotDepent = getSlope(paste0("prop_",pollNotDepent),"slope_pollNotDepent")

# YIELD
i=1
minYield=50 #minimum 50kg per ha
listSlopeYield = list()
for (crop in agriLand) {
  yieldCrop = paste0("yield_",crop)
  listSlopeYield[[i]] = getSlope(yieldCrop, paste0("slope_",yieldCrop), isOneColumn=TRUE, minThresh=minYield)
  i=i+1
}
yieldCrops = listSlopeYield %>% reduce(left_join, by = c("D1_HUS","D2_NUM"))
# write.csv(yieldCrops, file=paste0(dataFolder,"intermediateProducts/slopeYieldCrops.csv"),row.names=FALSE)
yieldCrops = read.csv(file=paste0(dataFolder,"intermediateProducts/slopeYieldCrops.csv"), header = T)

# MERGE EVERYTHING
listMetrics = list(cropland, seminatural, notAgri, edgeDensityDiss, avgFieldSize, diversity, intensification, demand, pollScore, pollService, croplandDependent, croplandNotDepent)
metrics     = listMetrics %>% reduce(left_join, by = c("D1_HUS","D2_NUM"))
metricsAll  = merge(yieldCrops, metrics, by = c("D1_HUS","D2_NUM"))
metricsAll  = Filter(function(x)!all(is.na(x)), metricsAll) # remove columns with all NA's
write.csv(metricsAll, file=paste0(dataFolder,"intermediateProducts/slopeMetrics.csv"),row.names=FALSE)

############################
# Annual evolution 
############################
# Demand
pl_dem<-ggplot(df_data, aes(YEA, demand)) +
  geom_point(aes(colour = factor(region))) +
  geom_smooth(method="lm", se=TRUE, aes(color=region))

# Pollination potential
pl_poll<-ggplot(df_pollModel, aes(YEA, ZonasNaturales_man0_mod0)) +
  geom_point(aes(colour = factor(region))) +
  geom_smooth(method="lm", se=TRUE, aes(color=region))


