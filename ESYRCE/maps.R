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

# Functions
source("./functions.R")

# dataFolder = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/ESYRCE/"
dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
figuresFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/figures/"

# Read datasets (slopes)
dataFile     = paste0(dataFolder, "intermediateProducts/slopeMetrics.csv")
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

# General function to save the figures
getFigures = function(metric, 
                      title, 
                      scaleWithMean=TRUE,
                      squish=FALSE, 
                      slopeMin=-5, 
                      slopeMax=5) 
{
  # plot settings
  name = paste0("Average \nSlope \n(%/yr)")
  name = gsub("- ","\n",name)
  pal  = wes_palette("Zissou1", 100, type = "continuous")
  
  # prepare dataset metrics
  baseCols         = c("D1_HUS", "D2_NUM", "province")
  meanCol          = paste0("mean_",metric)
  slopeCol         = paste0("slope_",metric)
  df_metric        = df_data[,c(baseCols,slopeCol,meanCol)]
  names(df_metric) = c(baseCols, "slope", "mean")
  if(scaleWithMean) df_metric$slope = df_metric$slope * 100 / df_metric$mean

  # aggregate data
  df_slopeProv = df_metric %>% group_by(province) %>% summarise(slope=mean(slope, na.rm = TRUE))
  sf_slopeProv = merge(polys,df_slopeProv)
  if (squish) {
    fSlope = ggplot(sf_slopeProv) +
      geom_sf(data = sf_slopeProv, aes(fill = slope))+
      scale_fill_gradientn(colours = pal, name=name, limits = c(slopeMin, slopeMax), oob = scales::squish)
  } 
  else {
    fSlope = ggplot(sf_slopeProv) +
      geom_sf(data = sf_slopeProv, aes(fill = slope))+
      scale_fill_gradientn(colours = pal, name=name, limits = c(slopeMin, slopeMax))
  }
  return(fSlope)
}

# to save figure, run (example):
# columns = paste0("prop_",agriLand)
# title = "Cropland"
# units = "%"
# figure = getFigures(columns, title, units, showAsPercentage = TRUE)
# pngFile = paste0(figuresFolder,"slope",gsub(" ","",title),".png")
# figure[[2]]+ggsave(pngFile, width = 7, height = 7, dpi = 150, units = "in", device='png')

#################
# Combined plots 
#################
# MAIN
diversity    = getFigures("cropsPerCropHa", "Diversity"         , slopeMin = -2.5, slopeMax = 2.5, squish = T)
avgFieldSize = getFigures("avgFieldSize","Field Size"           , slopeMin = -2.5, slopeMax = 2.5, squish = T)
cropland     = getFigures("cropland", "Cropland"                , slopeMin = -2.5, slopeMax = 2.5, squish = T)
seminatPlot  = getFigures("seminatural", "Seminatural"          , slopeMin = -2.5, slopeMax = 2.5, squish = T)
pollService  = getFigures("pollService2","Pollinators - Service", slopeMin = -2.5, slopeMax = 2.5, squish = T)
pollScore    = getFigures("pollScore","Pollinators - Model"     , slopeMin = -2.5, slopeMax = 2.5, squish = T)

prowMain <- cowplot::plot_grid( diversity    + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                avgFieldSize + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                cropland     + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                seminatPlot  + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                pollScore    + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                pollService  + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                    align = 'vh',
                    hjust = -1,
                    labels = c("Diversity", "Field size", "Cropland", "Seminatural", "Poll. Score", "Poll. Service"),
                    nrow = 3)
legend <- get_legend(
  diversity + theme(legend.box.margin = margin(0, 0, 0, 12))
)
slopesSummary = plot_grid(prowMain, legend, rel_widths = c(3, .4))
# Save
pngFile=paste0(figuresFolder,"slopesSummary.png")
slopesSummary+ggsave(pngFile, width = 8, height = 8, dpi = 250, units = "in", device='png')

# SUPPLEMENTARY MATERIAL
# supplementary: crop dep sí/no, edge density, forest, meadow y pasture
croplandDependent = getFigures("pollDependent","Cropland - Pollinators - Dependent"    , slopeMin = -5, slopeMax = 5, squish = T)
croplandNotDepent = getFigures("pollNotDepent","Cropland - Pollinators - Not Dependent", slopeMin = -5, slopeMax = 5, squish = T)
edgeDensityDiss   = getFigures("edgeDensityDiss", "Edge Density"                       , slopeMin = -5, slopeMax = 5, squish = T)
forestPlot        = getFigures("seminatForest", "Forest"                               , slopeMin = -5, slopeMax = 5, squish = T)
meadowPlot        = getFigures("seminatMeadow", "Meadow"                               , slopeMin = -5, slopeMax = 5, squish = T)
pasturePlot       = getFigures("seminatShrub" , "Pasture and Shrub"                    , slopeMin = -5, slopeMax = 5, squish = T)

prowSupp <- cowplot::plot_grid( croplandDependent + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                croplandNotDepent + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                edgeDensityDiss   + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                forestPlot        + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                meadowPlot        + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                                pasturePlot       + theme(legend.position="none", axis.text = element_blank(), axis.ticks = element_blank()) + ggtitle(""),
                            align = 'vh',
                            hjust = 0,
                            labels = c("Cropland Poll. Dep.", "Cropland Poll. Not Dep.", "Edge Density", "Forest", "Meadow", "Pasture and Shrub"),
                            nrow = 3)
legend <- get_legend(
  croplandDependent + theme(legend.box.margin = margin(0, 0, 0, 12))
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




