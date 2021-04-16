library(dplyr)
library(gstat) 
library(sf)
library(ggplot2)
library(scales)
library(raster)
library(rasterVis)
library(wesanderson)
library(cowplot)
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

getSlope = function(columns, isOneColumn = FALSE, minThresh=-1e100, maxThresh=1e100, columnThresh="") {
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
  } else { # use columnThresh to set thresholds
    df_metric = df_metric[df_data[,columnThresh] > minThresh,]
    df_metric = df_metric[df_data[,columnThresh] < maxThresh,]
  }  
  return(df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric"))))
}

# General function to save the figures
getFigures = function(columns, title, units, isOneColumn = FALSE, showAsPercentage = FALSE, 
                      minThresh=-1e100, maxThresh=1e100, columnThresh="", squish=FALSE, 
                      slopeMin=-5, slopeMax=5) {
  pal <- wes_palette("Zissou1", 100, type = "continuous")
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
  } else { # use columnThresh to set thresholds
    df_metric = df_metric[df_data[,columnThresh] > minThresh,]
    df_metric = df_metric[df_data[,columnThresh] < maxThresh,]
  }
  if (showAsPercentage) 
    df_metric$metric = df_metric$metric*100 # convert to percentage
  # Mean
  name = paste0("Average \n", gsub("\\(","\n(",title), "\n(",units,")")
  name = gsub("- ","\n",name)
  df_mean          = df_metric %>% group_by(D1_HUS, D2_NUM) %>% summarise(mean=mean(metric, na.rm = TRUE))
  df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
  df_meanAgg       = merge(df_aggUnit, df_mean)
  df_meanAggMean   = df_meanAgg %>% group_by(province) %>% summarise(mean=mean(mean, na.rm = TRUE))
  sf_meanAgg       = merge(polys,df_meanAggMean)
  midpoint         = mean(df_meanAggMean$mean)
  fMean = ggplot(sf_meanAgg) +
    geom_sf(data = sf_meanAgg, aes(fill = mean))+
    scale_fill_gradientn(colours = pal, name=name)
  
  # Slope
  name = paste0("Average \nSlope \n(%/yr)")
  name = gsub("- ","\n",name)
  df_slope         = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateSlopeOnecolumn(., "metric")))
  df_aggUnit       = df_data[,c("D1_HUS","D2_NUM","province")] %>% group_by(D1_HUS, D2_NUM) %>% summarise(province=first(province))
  df_slopeAgg      = merge(df_aggUnit, df_slope)
  df_slopeAgg$slope = df_slopeAgg$slope *100 / abs(df_meanAgg$mean) # scale by the mean of the metric
  df_slopeAggMean  = df_slopeAgg %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
  sf_slopeAgg      = merge(polys,df_slopeAggMean)
  if (squish) {
    fSlope = ggplot(sf_slopeAgg) +
      geom_sf(data = sf_slopeAgg, aes(fill = slope))+
      scale_fill_gradientn(colours = pal, name=name, limits = c(slopeMin, slopeMax), oob = scales::squish)
  } 
  else {
    fSlope = ggplot(sf_slopeAgg) +
      geom_sf(data = sf_slopeAgg, aes(fill = slope))+
      scale_fill_gradientn(colours = pal, name=name, limits = c(slopeMin, slopeMax))
  }
  return(list(fMean, fSlope))
}

# to save figure, run (example):
# columns = paste0("prop_",agriLand)
# title = "Cropland"
# units = "%"
# figure = getFigures(columns, title, units, showAsPercentage = TRUE)
# pngFile = paste0(figuresFolder,"slope",gsub(" ","",title),".png")
# figure[[2]]+ggsave(pngFile, width = 7, height = 7, dpi = 150, units = "in", device='png')

#############
# LANDCOVER
#############
# Cropland
cropland = getFigures(paste0("prop_",agriLand), "Cropland", "%", showAsPercentage = TRUE)
# seminatural
seminatural = getFigures(paste0("prop_",seminatural), "Seminatural", "%", showAsPercentage = TRUE)
# notAgri (artificial)
notAgri = getFigures("prop_notAgri", "Artificial", "%", isOneColumn = TRUE, showAsPercentage = TRUE)
# edgeDensityDiss 
edgeDensityDiss = getFigures("edgeDensityDiss", "Edge Density", "m/ha", isOneColumn = TRUE)

##########################
# Cropland metrics
##########################
# FieldSize(sin disolver)
avgFieldSize = getFigures("avgFieldSize","Field Size","ha", isOneColumn=TRUE, minThresh=0)
avgFieldSizeLarge = getFigures("avgFieldSize","Field Size (Large Fields)","ha", isOneColumn=TRUE, minThresh=5) #(area fields>5ha)
avgFieldSizeSmall = getFigures("avgFieldSize","Field Size (Small Fields)","ha", isOneColumn=TRUE, minThresh=0, maxThresh=5) #(area fields<5ha)
# Diversity
diversity = getFigures("cropsPerCropHa", "Diversity", "#crops/- crop ha", isOneColumn = TRUE, minThresh=0)
# diversity = getFigures("cropsPerHa", "Diversity", "#crops/ha", isOneColumn = TRUE, minThresh=0)
# Intensification
intensification = getFigures("intensification", "Intensification", "score", isOneColumn = TRUE)

##########################
# Pollinators metrics
##########################
# Threshold cropsPerCropHa>0 equals to the condition set in "mergeNewMetrics.R": cropArea > cropAreaThreshold (initially 0.5 hectares)
# Demand
demand = getFigures("demand","Pollinators - Demand","%", isOneColumn=TRUE, showAsPercentage = TRUE, minThresh=0, columnThresh="cropsPerCropHa")
# Poll score
pollScore = getFigures("pollScore","Pollinators - Model","score", isOneColumn=TRUE, minThresh=0, columnThresh="cropsPerCropHa")
# Poll service
pollService = getFigures("pollService2","Pollinators - Service","score", isOneColumn=TRUE, minThresh=0, columnThresh="cropsPerCropHa",squish=TRUE)
# pollinators' independent/dependent crops
croplandDependent = getFigures(paste0("prop_",pollDependent),"Cropland - Pollinators - Dependent","%", showAsPercentage = TRUE)
croplandNotDepent = getFigures(paste0("prop_",pollNotDepent),"Cropland - Pollinators - Not Dependent","%", showAsPercentage = TRUE)

#################
# Combined plots 
#################
# MAIN
diversity    = getFigures("cropsPerCropHa", "Diversity", "#crops/- crop ha", isOneColumn = TRUE, minThresh=0, slopeMin = -3, slopeMax = 3, squish = T)
avgFieldSize = getFigures("avgFieldSize","Field Size","ha", isOneColumn=TRUE, minThresh=0, slopeMin = -3, slopeMax = 3, squish = T)
cropland     = getFigures(paste0("prop_",agriLand), "Cropland", "%", showAsPercentage = TRUE, slopeMin = -3, slopeMax = 3, squish = T)
seminatPlot  = getFigures(paste0("prop_",seminatural), "Seminatural", "%", showAsPercentage = TRUE, slopeMin = -3, slopeMax = 3, squish = T)
pollService  = getFigures("pollService2","Pollinators - Service","score", isOneColumn=TRUE, columnThresh="cropsPerCropHa", slopeMin = -3, slopeMax = 3, squish = T)
pollScore    = getFigures("pollScore","Pollinators - Model","score", isOneColumn=TRUE, minThresh=0, columnThresh="cropsPerCropHa", slopeMin = -3, slopeMax = 3, squish = T)

prowMain <- cowplot::plot_grid( diversity[[2]]    + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                avgFieldSize[[2]] + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                cropland[[2]]     + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                seminatPlot[[2]]  + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                pollScore[[2]]    + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                pollService[[2]]  + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                    align = 'vh',
                    hjust = -1,
                    labels = c("Diversity", "Field size", "Cropland", "Seminatural", "Poll. Score", "Poll. Service"),
                    nrow = 3)
legend <- get_legend(
  diversity[[2]] + theme(legend.box.margin = margin(0, 0, 0, 12))
)
slopesSummary = plot_grid(prowMain, legend, rel_widths = c(3, .4))
# Save
pngFile=paste0(figuresFolder,"slopesSummary.png")
slopesSummary+ggsave(pngFile, width = 8, height = 8, dpi = 250, units = "in", device='png')

# SUPPLEMENTARY MATERIAL
# supplementary: crop dep sí/no, edge density, forest, meadow y pasture
croplandDependent = getFigures(paste0("prop_",pollDependent),"Cropland - Pollinators - Dependent","%", showAsPercentage = TRUE, slopeMin = -3, slopeMax = 3, squish = T)
croplandNotDepent = getFigures(paste0("prop_",pollNotDepent),"Cropland - Pollinators - Not Dependent","%", showAsPercentage = TRUE, slopeMin = -3, slopeMax = 3, squish = T)
edgeDensityDiss   = getFigures("edgeDensityDiss", "Edge Density", "m/ha", isOneColumn = TRUE, slopeMin = -3, slopeMax = 3, squish = T)
forestPlot        = getFigures(paste0("prop_",forested), "Forest", "%", showAsPercentage = TRUE, slopeMin = -3, slopeMax = 3, squish = T)
meadowPlot        = getFigures(paste0("prop_",c("naturalMeadow", "highMountainMeadow")), "Meadow", "%", showAsPercentage = TRUE, slopeMin = -3, slopeMax = 3, squish = T)
pasturePlot       = getFigures(paste0("prop_",c("pastureGrassland", "pastureShrub")), "Pasture", "%", showAsPercentage = TRUE, slopeMin = -3, slopeMax = 3, squish = T)

prowSupp <- cowplot::plot_grid( croplandDependent[[2]] + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                croplandNotDepent[[2]] + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                edgeDensityDiss[[2]]   + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                forestPlot[[2]]        + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                meadowPlot[[2]]        + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                pasturePlot[[2]]       + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                            align = 'vh',
                            hjust = 0,
                            labels = c("Cropland Poll. Dep.", "Cropland Poll. Not Dep.", "Edge Density", "Forest", "Meadow", "Pasture"),
                            nrow = 3)
legend <- get_legend(
  croplandDependent[[2]] + theme(legend.box.margin = margin(0, 0, 0, 12))
)
slopesSupplementary = plot_grid(prowSupp, legend, rel_widths = c(3, .4))
# Save
pngFile=paste0(figuresFolder,"slopesSupplementary.png")
slopesSupplementary+ggsave(pngFile, width = 8, height = 8, dpi = 250, units = "in", device='png')



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




