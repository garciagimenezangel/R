library(dplyr)
library(gstat) 
library(sf)
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
dataFile     = paste0(dataFolder, "geo_metrics_climate_20-12-18.csv")
df_data      = read.csv(dataFile, header=T)
modelFile    = paste0(dataFolder, "geo_model_20-12-18.csv")
df_pollModel = read.csv(modelFile, header=T)


###########
# RASTERS
###########
# Regions
fileShp = "C:/Users/angel/DATA/Administrative areas/ESP_adm/ESP_adm2.shp"

polys <- st_read(fileShp)
# Crop to peninsula and Baleares
box = c(xmin=-9.271338, xmax=4.308773, ymin=36.04188, ymax=43.76652)
polys = st_crop(polys, st_bbox(box)) 
polys$region = abbreviate(polys$NAME_1) # abbreviate names
polys$province = abbreviate(polys$NAME_2) # abbreviate names

# Metrics
# avgFieldSize(sin disolver)
# df_avgFieldSize              = df_data %>% group_by(YEA, region) %>% summarise(avgFieldSize=mean(avgFieldSize, na.rm = TRUE))
df_avgFieldSize                = df_data %>% group_by(YEA, province) %>% summarise(avgFieldSize=mean(avgFieldSize, na.rm = TRUE))
# df_slopeAvgFieldSize         = df_avgFieldSize %>% group_by(region) %>% do(data.frame(calculateSlopeOnecolumn(., "avgFieldSize")))
df_slopeAvgFieldSize           = df_avgFieldSize %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "avgFieldSize")))
df_slopeAvgFieldSize$FieldSize = df_slopeAvgFieldSize$slope
sf_slopeAvgFieldSize           = merge(polys,df_slopeAvgFieldSize)
ggplot(sf_slopeAvgFieldSize) +
  geom_sf(data = sf_slopeAvgFieldSize, aes(fill = FieldSize))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# propSeminatural
baseCols                        = c("province", "region", "YEA")
columns                         = paste0("prop_",seminatural)
df_seminatural                  = df_data[,c(baseCols,columns)]
df_seminatural$semin            = rowSums(df_seminatural[,columns])
df_seminatural                  = df_seminatural %>% group_by(YEA, province) %>% summarise(avgSeminatural=mean(semin, na.rm = TRUE))
df_slopeSeminatural             = df_seminatural %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "avgSeminatural")))
df_slopeSeminatural$Seminatural = df_slopeSeminatural$slope
sf_slopeSeminatural             = merge(polys,df_slopeSeminatural)
ggplot(sf_slopeSeminatural) +
  geom_sf(data = sf_slopeSeminatural, aes(fill = Seminatural))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# edgeDenSemiDiss
df_edgeDenSeminat                             = df_data %>% group_by(YEA, province) %>% summarise(edgeDenSeminat=mean(edgeDenSemiDiss, na.rm = TRUE))
df_slopeEdgeDenSeminat                        = df_edgeDenSeminat %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "edgeDenSeminat")))
df_slopeEdgeDenSeminat$EdgeDensitySeminatural = df_slopeEdgeDenSeminat$slope
sf_slopeEdgeDenSeminat                        = merge(polys,df_slopeEdgeDenSeminat)
ggplot(sf_slopeEdgeDenSeminat) +
  geom_sf(data = sf_slopeEdgeDenSeminat, aes(fill = EdgeDensitySeminatural))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# avgSeminatSize
df_avgSeminatSize                      = df_data %>% group_by(YEA, province) %>% summarise(avgSeminatSize=mean(avgSeminatSize, na.rm = TRUE))
df_slopeAvgSeminatSize                 = df_avgSeminatSize %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "avgSeminatSize")))
df_slopeAvgSeminatSize$SeminaturalSize = df_slopeAvgSeminatSize$slope
sf_slopeAvgSeminatSize                 = merge(polys,df_slopeAvgSeminatSize)
ggplot(sf_slopeAvgSeminatSize) +
  geom_sf(data = sf_slopeAvgSeminatSize, aes(fill = SeminaturalSize))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# heterogeneity
df_diversity                    = df_data %>% group_by(YEA, province) %>% summarise(diversity=mean(heterogeneity, na.rm = TRUE))
df_slopeDiversity               = df_diversity %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "diversity")))
df_slopeDiversity$CropDiversity = df_slopeDiversity$slope
sf_slopeDiversity               = merge(polys,df_slopeDiversity)
ggplot(sf_slopeDiversity) +
  geom_sf(data = sf_slopeDiversity, aes(fill = CropDiversity))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)


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




