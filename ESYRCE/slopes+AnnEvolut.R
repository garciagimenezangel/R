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
# Note: slope is scaled using the mean value over the years, so we save these mean values too
getSlopeAndMean = function(columns, 
                           outcolname, 
                           isOneColumn = FALSE, 
                           minThresh=-1e100, 
                           maxThresh=1e100, 
                           columnThresh="", 
                           ignoreSegmAllZeros = T,
                           ignoreSegmAnyZero  = F)
{
  baseCols           = c("D1_HUS", "D2_NUM", "province", "YEA")
  df_metric          = df_data[,c(baseCols,columns)]
  if(isOneColumn) {
    df_metric$metric = df_metric[,columns]
  } else {
    df_metric$metric = rowSums(df_metric[,columns])
  }
  if (columnThresh == "") { # use metric to set thresholds
    notValid = (df_metric$metric < minThresh) | (df_metric$metric > maxThresh)
    df_metric$metric[notValid] = NA
  } else { # use columnThresh to set thresholds
    notValid = (df_metric$columnThresh < minThresh) | (df_metric$columnThresh > maxThresh)
    df_metric$metric[notValid] = NA
  }  
  df_slope    = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
  df_mean     = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
  df_slopeMean= merge(df_slope,df_mean,by=c("D1_HUS", "D2_NUM"))

  if (ignoreSegmAllZeros) { # if all zeros over the years, set values to NA
    allZeros = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(areAllZeros(., "metric")))
    auxDf    = merge(df_slopeMean, allZeros)
    df_slopeMean$slope[auxDf$allZeros] = NA
    df_slopeMean$mean[auxDf$allZeros]  = NA
  }
  else if (ignoreSegmAnyZero) {
    anyZero = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(isAnyZero(., "metric")))
    auxDf   = merge(df_slopeMean, anyZero)
    df_slopeMean$slope[auxDf$anyZero] = NA
    df_slopeMean$mean[auxDf$anyZero]  = NA
  }
  names(df_slopeMean)[names(df_slopeMean) == 'slope'] <- paste0("slope_",outcolname)
  names(df_slopeMean)[names(df_slopeMean) == 'mean']  <- paste0("mean_",outcolname)
  return(df_slopeMean)
}

# LANDCOVER
# Cropland
df_cropland = getSlopeAndMean(paste0("prop_",agriLand), "cropland")
# seminatural
df_seminatural = getSlopeAndMean(paste0("prop_",seminatural), "seminatural")
# notAgri (artificial)
df_notAgri = getSlopeAndMean("prop_notAgri", "artificial", isOneColumn = TRUE)
# edgeDensityDiss 
df_edgeDensityDiss = getSlopeAndMean("edgeDensityDiss", "edgeDensityDiss", isOneColumn = TRUE, ignoreSegmAllZeros = F)
# seminatural types
df_seminatForest = getSlopeAndMean(paste0("prop_",seminatural_forest), "seminatForest")
df_seminatMeadow = getSlopeAndMean(paste0("prop_",seminatural_meadow), "seminatMeadow")
df_seminatShrub  = getSlopeAndMean(paste0("prop_",seminatural_shrub),  "seminatShrub")

# CROPLAND metrics
# FieldSize(sin disolver)
df_avgFieldSize = getSlopeAndMean("avgFieldSize","avgFieldSize", isOneColumn=TRUE, minThresh=0)
# Diversity
df_diversity = getSlopeAndMean("cropsPerCropHa", "cropsPerCropHa", isOneColumn = TRUE, minThresh=0)
# Intensification
df_intensification = getSlopeAndMean("intensification", "intensification", isOneColumn = TRUE)

# POLLINATORS metrics
# Threshold cropsPerCropHa>0 equals to the condition set in "mergeNewMetrics.R": cropArea > cropAreaThreshold (initially 0.5 hectares)
# Demand
df_demand = getSlopeAndMean("demand","demand", isOneColumn=TRUE, minThresh=0, columnThresh="cropsPerCropHa")
# Poll score
df_pollScore = getSlopeAndMean("pollScore","pollScore", isOneColumn=TRUE, minThresh=0, columnThresh="cropsPerCropHa")
# Poll service
df_pollService = getSlopeAndMean("pollService2","pollService2", isOneColumn=TRUE, minThresh=0, columnThresh="cropsPerCropHa")
# pollinators' independent/dependent crops
df_croplandDependent = getSlopeAndMean(paste0("prop_",pollDependent),"pollDependent")
df_croplandNotDepent = getSlopeAndMean(paste0("prop_",pollNotDepent),"pollNotDepent")

# YIELD
i=1
minYield=50 #minimum 50kg per ha
listSlopeYield = list()
for (crop in agriLand) {
  yieldCrop = paste0("yield_",crop)
  listSlopeYield[[i]] = getSlopeAndMean(yieldCrop, paste0("",yieldCrop), isOneColumn=TRUE, minThresh=minYield)
  i=i+1
}
df_yieldCrops = listSlopeYield %>% reduce(left_join, by = c("D1_HUS","D2_NUM"))
write.csv(df_yieldCrops, file=paste0(dataFolder,"intermediateProducts/slopeYieldCrops.csv"),row.names=FALSE)
#df_yieldCrops = read.csv(file=paste0(dataFolder,"intermediateProducts/slopeYieldCrops.csv"), header = T)

# MERGE EVERYTHING
listMetrics = list(df_cropland, df_seminatural, df_seminatForest, df_seminatMeadow, df_seminatShrub, df_notAgri, df_edgeDensityDiss, df_avgFieldSize, df_diversity, df_intensification, df_demand, df_pollScore, df_pollService, df_croplandDependent, df_croplandNotDepent)
df_metrics     = listMetrics %>% reduce(left_join, by = c("D1_HUS","D2_NUM","province"))
df_metricsAll  = merge(df_yieldCrops, df_metrics, by = c("D1_HUS","D2_NUM","province"))
df_metricsAll  = Filter(function(x)!all(is.na(x)), metricsAll) # remove columns with all NA's

# Add province 
df_province = df_data %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
df_metricsAll = merge(df_metricsAll, df_province, by=c("D1_HUS", "D2_NUM")) # add province

# SAVE
write.csv(df_metricsAll, file=paste0(dataFolder,"intermediateProducts/slopeMetrics.csv"),row.names=FALSE)
df_metricsAll = read.csv(file=paste0(dataFolder,"intermediateProducts/slopeMetrics.csv"), header=T)

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


