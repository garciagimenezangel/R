library(dplyr)
library(gstat) 
rm(list=ls())
###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/Analysis/2020-12/"
GEEFolder  = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"

# Read datasets
dataFile     = paste0(dataFolder, "geo_metrics_20-12-18.csv")
df_data      = read.csv(dataFile, header=T)
modelFile    = paste0(dataFolder, "geo_model_20-12-18.csv")
df_pollModel = read.csv(modelFile, header=T)

#######################
# Calculate variograms
#######################
randomPt = sample_n(df_data, 1.5)
dist_deg = 1
xmax = randomPt$longitude + dist_deg
xmin = randomPt$longitude - dist_deg
ymax = randomPt$latitude + dist_deg
ymin = randomPt$latitude - dist_deg

# Seminatural
neighbours = subset(df_data, longitude>xmin & longitude<xmax & latitude>ymin & latitude<ymax)
prop_seminatural_cols  = paste0("prop_",seminatural)
neighbours["prop_seminatural"] = rowSums(neighbours[,prop_seminatural_cols])
coordinates(neighbours)= ~ longitude+latitude
variogramNeighbours = variogram(prop_seminatural~1, data=neighbours)
plot(variogramNeighbours)

# Cropfields
neighbours = subset(df_data, longitude>xmin & longitude<xmax & latitude>ymin & latitude<ymax)
prop_agriLand_cols  = paste0("prop_",agriLand)
neighbours["prop_agriLand"] = rowSums(neighbours[,prop_agriLand_cols])
coordinates(neighbours)= ~ longitude+latitude
variogramNeighbours = variogram(prop_agriLand~1, data=neighbours)
plot(variogramNeighbours)

# Individual crop, for example maize
neighbours = subset(df_data, longitude>xmin & longitude<xmax & latitude>ymin & latitude<ymax)
coordinates(neighbours)= ~ longitude+latitude
variogramNeighbours = variogram(prop_maize~1, data=neighbours)
plot(variogramNeighbours)

# Model
neighbours = subset(df_data, longitude>xmin & longitude<xmax & latitude>ymin & latitude<ymax)
neighbours = neighbours[!is.na(neighbours$ZonasNaturales2018_man0_mod0),]
coordinates(neighbours)= ~ longitude+latitude
variogramNeighbours = variogram(ZonasNaturales2018_man0_mod0~1, data=neighbours)
plot(variogramNeighbours)


