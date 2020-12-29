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


###########
# RASTERS
###########
# Pollination score
r <- raster()
extent(r)<-c(-9.271338, 4.308773, 36.04188, 43.76652)
#reclasificacion de la variable para verla mejor en 3 categorias, incremento area (1), neutral (0), decrecimiento area
df_slopesPoll$slope_recl<-df_slopesPoll$slope
df_slopesPoll$slope_recl[df_slopesPoll$slope_recl<0]<--1
df_slopesPoll$slope_recl[df_slopesPoll$slope_recl>0]<-1
spr<-df_slopesPoll[,c("longitude","latitude","slope_recl")]
head(spr)
spr2<-as.data.frame(spr)
coordinates(spr2) <- ~longitude+latitude
r <- rasterize(spr2, r, 'slope_recl', fun=mean)
plot(r, main="Poll score")




