library(dplyr)
library(gstat) 
library(sf)
library(ggplot2)
library(scales)
library(raster)
library(rasterVis)
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
figuresFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/figures/"
GEEFolder  = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"

# Read datasets
dataFile     = paste0(dataFolder, "metrics_v2021-02.csv")
df_data      = read.csv(dataFile, header=T)
# modelFile    = paste0(dataFolder, "intermediateProducts/geo_model_20-12-18.csv")
# df_pollModel = read.csv(modelFile, header=T)

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

# General function
saveFigures = function(columns, title, units, isOneColumn = FALSE, showAsPercentage = FALSE, minThresh=-1e100, maxThresh=1e100, columnThresh="", squish=FALSE) {
  baseCols           = c("D1_HUS", "D2_NUM", "province", "YEA")
  df_metric          = df_data[,c(baseCols,columns)]
  if(isOneColumn) {
    df_metric$metric = df_metric[,columns]
  } else {
    df_metric$metric = rowSums(df_metric[,columns])
  }
  if (columnThresh == "") { # use metric to set thresholds
    df_metric = df_metric[df_metric$metric > minThresh,]
    df_metric = df_metric[df_metric$metric < maxThresh,]
  }
  else { # use columnThresh to set thresholds
    df_metric = df_metric[df_data[,columnThresh] > minThresh,]
    df_metric = df_metric[df_data[,columnThresh] < maxThresh,]
  }
  if (showAsPercentage) 
    df_metric$metric = df_metric$metric*100 # convert to percentage
  # Mean
  pngFile = paste0(figuresFolder,"avg",gsub(" ","",title),".png")
  name = paste0("Average \n", gsub("\\(","\n(",title), "\n(",units,")")
  name = gsub("- ","\n",name)
  df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
  df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
  df_meanAgg       = merge(df_aggUnit, df_mean)
  df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
  sf_meanAgg       = merge(polys,df_meanAggMean)
  midpoint         = mean(df_meanAggMean$mean)
  ggplot(sf_meanAgg) +
    geom_sf(data = sf_meanAgg, aes(fill = mean))+
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = midpoint, name=name)
  ggsave(pngFile, width = 7, height = 7, dpi = 150, units = "in", device='png')
  # Slope
  pngFile = paste0(figuresFolder,"slope",gsub(" ","",title),".png")
  name = paste0("Change in \n", gsub("\\(","\n(",title), "\n(%/yr)")
  name = gsub("- ","\n",name)
  df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
  df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
  df_slopeAgg      = merge(df_aggUnit, df_slope)
  df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
  df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
  sf_slopeAgg      = merge(polys,df_slopeAggMean)
  if (squish) {
    ggplot(sf_slopeAgg) +
      geom_sf(data = sf_slopeAgg, aes(fill = slope))+
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name, limits = c(-10, 10), oob = scales::squish)+
      ggsave(pngFile, width = 7, height = 7, dpi = 150, units = "in", device='png')
  } 
  else {
    ggplot(sf_slopeAgg) +
      geom_sf(data = sf_slopeAgg, aes(fill = slope))+
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name=name)+
      ggsave(pngFile, width = 7, height = 7, dpi = 150, units = "in", device='png')
  }
}

#############
# LANDCOVER
#############
# Cropland
saveFigures(paste0("prop_",agriLand), "Cropland", "%", showAsPercentage = TRUE)
# seminatural
saveFigures(paste0("prop_",seminatural), "Seminatural", "%", showAsPercentage = TRUE)
# notAgri (artificial)
saveFigures("prop_notAgri", "Artificial", "%", isOneColumn = TRUE, showAsPercentage = TRUE)
# edgeDensityDiss 
saveFigures("edgeDensityDiss", "Edge Density", "m/ha", isOneColumn = TRUE)

##########################
# Cropland metrics
##########################
# FieldSize(sin disolver)
saveFigures("avgFieldSize","Field Size","ha", isOneColumn=TRUE, minThresh=0)
saveFigures("avgFieldSize","Field Size (Large Fields)","ha", isOneColumn=TRUE, minThresh=5) #(area fields>5ha)
saveFigures("avgFieldSize","Field Size (Small Fields)","ha", isOneColumn=TRUE, minThresh=0, maxThresh=5) #(area fields<5ha)
# Diversity
saveFigures("cropsPerCropHa", "Diversity", "#crops/- crop ha", isOneColumn = TRUE, minThresh=0)
# saveFigures("cropsPerHa", "Diversity", "#crops/ha", isOneColumn = TRUE, minThresh=0)
# Intensification
saveFigures("intensification", "Intensification", "score", isOneColumn = TRUE)

##########################
# Pollinators metrics
##########################
# Threshold cropsPerCropHa>0 equals to the condition set in "mergeNewMetrics.R": cropArea > cropAreaThreshold (initially 0.5 hectares)
# Demand
saveFigures("demand","Pollinators - Demand","%", isOneColumn=TRUE, showAsPercentage = TRUE, minThresh=0, columnThresh="cropsPerCropHa")
# Poll score
saveFigures("pollScore","Pollinators - Model","score", isOneColumn=TRUE, minThresh=0, columnThresh="cropsPerCropHa")
# Poll service
saveFigures("pollService2","Pollinators - Service","score", isOneColumn=TRUE, minThresh=0, columnThresh="cropsPerCropHa",squish=TRUE)
# pollinators' independent/dependent crops
saveFigures(paste0("prop_",pollDependent),"Cropland - Pollinators - Dependent","%", showAsPercentage = TRUE)
saveFigures(paste0("prop_",pollNotDepent),"Cropland - Pollinators - Not Dependent","%", showAsPercentage = TRUE)

########################
# Pollination Service
########################
pServ = raster(paste0(figuresFolder,"pollinators/PollService.tif"))
qq    = quantile(pServ, probs = seq(0,1,0.1), names=FALSE)
png(paste0(figuresFolder,"pollinators/pollService.png"))
levelplot(pServ, par.settings=BuRdTheme(), at=qq, margin=F)
dev.off()

########################
# Pollinators score
########################
pScore = raster(paste0(figuresFolder,"pollinators/PollScore_ZonasNaturales2018_man0_mod0_Spain.tif"))
png(paste0(figuresFolder,"pollinators/pollScore.png"))
levelplot(pScore, par.settings=BuRdTheme(), margin=F)
dev.off()

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




