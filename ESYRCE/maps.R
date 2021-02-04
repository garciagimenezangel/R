library(dplyr)
library(gstat) 
library(sf)
# rm(list=ls())
###########

setwd("C:/Users/angel.gimenez/git/R/ESYRCE/")
# setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

dataFolder = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/ESYRCE/"
GEEFolder  = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"
# dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
# GEEFolder  = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"

# Read datasets
dataFile     = paste0(dataFolder, "geo_metrics_climate_20-12-18.csv")
df_data      = read.csv(dataFile, header=T)
modelFile    = paste0(dataFolder, "geo_model_20-12-18.csv")
df_pollModel = read.csv(modelFile, header=T)

# Regions
fileShp = "C:/Users/angel.gimenez/DATA/Administrative areas/ESP_adm/ESP_adm2.shp"

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


###############
# MAPS METRICS
###############
df_dataYears = df_data[df_data$YEA == 2003 | df_data$YEA == 2019,]
df_pollYears = df_pollModel[df_pollModel$YEA == 2000 | df_pollModel$YEA == 2018,]
# avgFieldSize(sin disolver)
df_metricYears = df_dataYears %>% group_by(YEA,province) %>% summarise(FieldSize=mean(avgFieldSize, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = FieldSize))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# propSeminatural: seminatural = c(forested, otherWoodyCrop, pasture)
baseCols                        = c("province", "region", "YEA")
columns                         = paste0("prop_",seminatural)
df_seminatural                  = df_dataYears[,c(baseCols,columns)]
df_seminatural$semin            = rowSums(df_seminatural[,columns])
df_metricYears = df_seminatural %>% group_by(YEA,province) %>% summarise(Seminatural=mean(semin, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = Seminatural))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# propAgri: agriLand  = c(cerealGrain, legumeGrain, tuber, industrial, fodder, vegetable, orchard, ornamental, citric, fruitNoCitric, vineyard, oliveTrees, nursery)
baseCols                        = c("province", "region", "YEA")
columns                         = paste0("prop_",agriLand)
df_agriLand                     = df_dataYears[,c(baseCols,columns)]
df_agriLand$agriLand            = rowSums(df_agriLand[,columns])
df_metricYears = df_agriLand %>% group_by(YEA,province) %>% summarise(AgriLand=mean(agriLand, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = AgriLand))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# forested: seminatural = c(forested, otherWoodyCrop, pasture)
baseCols                        = c("province", "region", "YEA")
columns                         = paste0("prop_",forested)
df_forested                     = df_dataYears[,c(baseCols,columns)]
df_forested$forested            = rowSums(df_forested[,columns])
df_metricYears = df_forested %>% group_by(YEA,province) %>% summarise(Forested=mean(forested, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = Forested))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# pasture: seminatural = c(forested, otherWoodyCrop, pasture)
baseCols                        = c("province", "region", "YEA")
columns                         = paste0("prop_",pasture)
df_pasture                      = df_dataYears[,c(baseCols,columns)]
df_pasture$pasture              = rowSums(df_pasture[,columns])
df_metricYears = df_pasture %>% group_by(YEA,province) %>% summarise(Pasture=mean(pasture, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = Pasture))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# notAgri
df_metricYears = df_dataYears %>% group_by(YEA,province) %>% summarise(NotAgri=mean(prop_notAgri, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = NotAgri))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# other ->c('fallow','emptyGreenh','posio','wasteland','spartizal','abandoned')
baseCols                        = c("province", "region", "YEA")
columns                         = paste0("prop_",other)
df_other                        = df_dataYears[,c(baseCols,columns)]
df_other$other                  = rowSums(df_other[,columns])
df_metricYears = df_other %>% group_by(YEA,province) %>% summarise(Other=mean(other, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = Other))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# pollImportant 
baseCols                        = c("province", "region", "YEA")
columns                         = paste0("prop_",pollImportant)
df_other                        = df_dataYears[,c(baseCols,columns)]
df_other$pollDepCrops           = rowSums(df_other[,columns])
df_metricYears = df_other %>% group_by(YEA,province) %>% summarise(PollDepCrops=mean(pollDepCrops, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = PollDepCrops))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# pollNotImportant 
baseCols                        = c("province", "region", "YEA")
columns                         = paste0("prop_",pollNotImport)
df_other                        = df_dataYears[,c(baseCols,columns)]
df_other$pollIndepCrops         = rowSums(df_other[,columns])
df_metricYears = df_other %>% group_by(YEA,province) %>% summarise(PollIndepCrops=mean(pollIndepCrops, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = PollIndepCrops))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# pollImportant/pollNotImportant
baseCols                        = c("province", "region", "YEA")
columns1                        = paste0("prop_",pollNotImport)
columns2                        = paste0("prop_",pollImportant)
df_other                        = df_dataYears[,c(baseCols,columns1,columns2)]
df_other$pollIndepCrops         = rowSums(df_other[,columns1])
df_other$pollDepCrops           = rowSums(df_other[,columns2])
df_metricYears = df_other %>% group_by(YEA,province) %>% summarise(PollIndepCrops=mean(pollIndepCrops, na.rm = TRUE), PollDepCrops=mean(pollDepCrops, na.rm = TRUE))
df_metricYears$ratioIndepDepCrops = df_metricYears$PollIndepCrops/df_metricYears$PollDepCrops
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = ratioIndepDepCrops))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# abandoned
baseCols                        = c("province", "region", "YEA")
columns                         = c("prop_abandoned")
df_other                        = df_dataYears[,c(baseCols,columns)]
df_metricYears = df_other %>% group_by(YEA,province) %>% summarise(Abandoned=mean(prop_abandoned, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = Abandoned))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# edgeDenSemiDiss
df_metricYears = df_dataYears %>% group_by(YEA,province) %>% summarise(EdgeDensitySeminatural=mean(edgeDenSemiDiss, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = EdgeDensitySeminatural))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# avgSeminatSize
df_metricYears = df_dataYears %>% group_by(YEA,province) %>% summarise(SeminaturalSize=mean(avgSeminatSize, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = SeminaturalSize))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# heterogeneity(diversity)
df_metricYears = df_dataYears %>% group_by(YEA,province) %>% summarise(Diversity=mean(heterogeneity, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = Diversity))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# demand
df_metricYears = df_dataYears %>% group_by(YEA,province) %>% summarise(Demand=mean(demand, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = Demand))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# intensification
df_metricYears = df_dataYears %>% group_by(YEA,province) %>% summarise(Intensif=mean(intensification, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = Intensif))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
# pollScore
df_metricYears = df_pollYears %>% group_by(YEA,province) %>% summarise(PollinatorScore=mean(ZonasNaturales_man0_mod0, na.rm = TRUE))
sf_metricYears = merge(polys,df_metricYears)
ggplot(sf_metricYears) +
  geom_sf(data = sf_metricYears, aes(fill = PollinatorScore))+
  facet_wrap(~YEA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# Particular crops area
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


###############
# MAPS SLOPES
###############
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

# propAgri
baseCols                        = c("province", "region", "YEA")
columns                         = paste0("prop_",agriLand)
df_agriLand                     = df_data[,c(baseCols,columns)]
df_agriLand$agriLand            = rowSums(df_agriLand[,columns])
df_agriLand                     = df_agriLand %>% group_by(YEA, province) %>% summarise(agriLand=mean(agriLand, na.rm = TRUE))
df_slopeAgriLand                = df_agriLand %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "agriLand")))
df_slopeAgriLand$AgriLand       = df_slopeAgriLand$slope
sf_slopeAgriLand                = merge(polys,df_slopeAgriLand)
ggplot(sf_slopeAgriLand) +
  geom_sf(data = sf_slopeAgriLand, aes(fill = AgriLand))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# propOther
baseCols             = c("province", "region", "YEA")
columns              = paste0("prop_",other)
df_metric            = df_data[,c(baseCols,columns)]
df_metric$other      = rowSums(df_metric[,columns])
df_metric            = df_metric %>% group_by(YEA, province) %>% summarise(propOther=mean(other, na.rm = TRUE))
df_slopeMetric       = df_metric %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "propOther")))
df_slopeMetric$Other = df_slopeMetric$slope
sf_slopeMetric       = merge(polys,df_slopeMetric)
ggplot(sf_slopeMetric) +
  geom_sf(data = sf_slopeMetric, aes(fill = Other))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# forested
baseCols             = c("province", "region", "YEA")
columns              = paste0("prop_",forested)
df_metric            = df_data[,c(baseCols,columns)]
df_metric$forested   = rowSums(df_metric[,columns])
df_metric            = df_metric %>% group_by(YEA, province) %>% summarise(propForested=mean(forested, na.rm = TRUE))
df_slopeMetric       = df_metric %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "propForested")))
df_slopeMetric$Forested = df_slopeMetric$slope
sf_slopeMetric       = merge(polys,df_slopeMetric)
ggplot(sf_slopeMetric) +
  geom_sf(data = sf_slopeMetric, aes(fill = Forested))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# pasture
baseCols             = c("province", "region", "YEA")
columns              = paste0("prop_",pasture)
df_metric            = df_data[,c(baseCols,columns)]
df_metric$pasture    = rowSums(df_metric[,columns])
df_metric            = df_metric %>% group_by(YEA, province) %>% summarise(propPasture=mean(pasture, na.rm = TRUE))
df_slopeMetric       = df_metric %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "propPasture")))
df_slopeMetric$Pasture = df_slopeMetric$slope
sf_slopeMetric       = merge(polys,df_slopeMetric)
ggplot(sf_slopeMetric) +
  geom_sf(data = sf_slopeMetric, aes(fill = Pasture))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# industrial
baseCols             = c("province", "region", "YEA")
columns              = paste0("prop_",industrial)
df_metric            = df_data[,c(baseCols,columns)]
df_metric$industrial    = rowSums(df_metric[,columns])
df_metric            = df_metric %>% group_by(YEA, province) %>% summarise(propIndustrial=mean(industrial, na.rm = TRUE))
df_slopeMetric       = df_metric %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "propIndustrial")))
df_slopeMetric$Industrial = df_slopeMetric$slope
sf_slopeMetric       = merge(polys,df_slopeMetric)
ggplot(sf_slopeMetric) +
  geom_sf(data = sf_slopeMetric, aes(fill = Industrial))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# pollImportant
baseCols             = c("province", "region", "YEA")
columns              = paste0("prop_",pollImportant)
df_metric            = df_data[,c(baseCols,columns)]
df_metric$pollDepCrop= rowSums(df_metric[,columns])
df_metric            = df_metric %>% group_by(YEA, province) %>% summarise(pollDepCrop=mean(pollDepCrop, na.rm = TRUE))
df_slopeMetric       = df_metric %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "pollDepCrop")))
df_slopeMetric$PollDepCrop = df_slopeMetric$slope
sf_slopeMetric       = merge(polys,df_slopeMetric)
ggplot(sf_slopeMetric) +
  geom_sf(data = sf_slopeMetric, aes(fill = PollDepCrop))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# pollNotImportant
baseCols             = c("province", "region", "YEA")
columns              = paste0("prop_",pollNotImport)
df_metric            = df_data[,c(baseCols,columns)]
df_metric$pollIndepCrops = rowSums(df_metric[,columns])
df_metric            = df_metric %>% group_by(YEA, province) %>% summarise(pollIndepCrops=mean(pollIndepCrops, na.rm = TRUE))
df_slopeMetric       = df_metric %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "pollIndepCrops")))
df_slopeMetric$PollIndepCrops = df_slopeMetric$slope
sf_slopeMetric       = merge(polys,df_slopeMetric)
ggplot(sf_slopeMetric) +
  geom_sf(data = sf_slopeMetric, aes(fill = PollIndepCrops))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# pollNotImportant/pollImportant
baseCols             = c("province", "region", "YEA")
columns1             = paste0("prop_",pollImportant)
columns2             = paste0("prop_",pollNotImport)
df_metric            = df_data[,c(baseCols,c(columns1,columns2))]
df_metric$pollDepCrops  = rowSums(df_metric[,columns1])
df_metric$pollIndepCrops= rowSums(df_metric[,columns2])
df_metric            = df_metric %>% group_by(YEA, province) %>% summarise(pollDepCrops=mean(pollDepCrops, na.rm = TRUE),pollIndepCrops=mean(pollIndepCrops, na.rm = TRUE))
df_metric$ratioIndepDepCrops = df_metric$pollIndepCrops / df_metric$pollDepCrops
df_slopeMetric       = df_metric %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "ratioIndepDepCrops")))
df_slopeMetric$RatioIndepDepCrops = df_slopeMetric$slope
sf_slopeMetric       = merge(polys,df_slopeMetric)
ggplot(sf_slopeMetric) +
  geom_sf(data = sf_slopeMetric, aes(fill = RatioIndepDepCrops))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

  
# Agri with unknown dependence on pollinators
baseCols             = c("province", "region", "YEA")
columns              = paste0("prop_",agriLand[!(agriLand %in% c(pollImportant,pollNotImport)) ])
df_metric            = df_data[,c(baseCols,columns)]
df_metric$unknown    = rowSums(df_metric[,columns])
df_metric            = df_metric %>% group_by(YEA, province) %>% summarise(unknown=mean(unknown, na.rm = TRUE))
df_slopeMetric       = df_metric %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "unknown")))
df_slopeMetric$Unknown = df_slopeMetric$slope
sf_slopeMetric       = merge(polys,df_slopeMetric)
ggplot(sf_slopeMetric) +
  geom_sf(data = sf_slopeMetric, aes(fill = Unknown))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# propNotAgri
df_metric                      = df_data %>% group_by(YEA, province) %>% summarise(propNotAgri=mean(prop_notAgri, na.rm = TRUE))
df_slopeMetric                 = df_metric %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "propNotAgri")))
df_slopeMetric$NotAgricultural = df_slopeMetric$slope
sf_slopeMetric                 = merge(polys,df_slopeMetric)
ggplot(sf_slopeMetric) +
  geom_sf(data = sf_slopeMetric, aes(fill = NotAgricultural))+
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

# abandoned
df_abandoned                    = df_data %>% group_by(YEA, province) %>% summarise(abandoned=mean(prop_abandoned, na.rm = TRUE))
df_slopeAbandoned               = df_abandoned %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "abandoned")))
df_slopeAbandoned$Abandoned     = df_slopeAbandoned$slope
sf_slopeAbandoned               = merge(polys,df_slopeAbandoned)
ggplot(sf_slopeAbandoned) +
  geom_sf(data = sf_slopeAbandoned, aes(fill = Abandoned))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# heterogeneity
df_diversity                    = df_data %>% group_by(YEA, province) %>% summarise(diversity=mean(heterogeneity, na.rm = TRUE))
df_slopeDiversity               = df_diversity %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "diversity")))
df_slopeDiversity$CropDiversity = df_slopeDiversity$slope
sf_slopeDiversity               = merge(polys,df_slopeDiversity)
ggplot(sf_slopeDiversity) +
  geom_sf(data = sf_slopeDiversity, aes(fill = CropDiversity))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# demand
df_metric             = df_data %>% group_by(YEA, province) %>% summarise(demand=mean(demand, na.rm = TRUE))
df_slopeMetric        = df_metric %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "demand")))
df_slopeMetric$Demand = df_slopeMetric$slope
sf_slopeMetric        = merge(polys,df_slopeMetric)
ggplot(sf_slopeMetric) +
  geom_sf(data = sf_slopeMetric, aes(fill = Demand))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# intensification
df_metric               = df_data %>% group_by(YEA, province) %>% summarise(intensif=mean(intensification, na.rm = TRUE))
df_slopeMetric          = df_metric %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "intensif")))
df_slopeMetric$Intensif = df_slopeMetric$slope
sf_slopeMetric          = merge(polys,df_slopeMetric)
ggplot(sf_slopeMetric) +
  geom_sf(data = sf_slopeMetric, aes(fill = Intensif))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)

# Pollination score
df_pollScore                      = df_pollModel %>% group_by(YEA, province) %>% summarise(pollScore=mean(ZonasNaturales_man0_mod0, na.rm = TRUE))
df_slopePollScore                 = df_pollScore %>% group_by(province) %>% do(data.frame(calculateSlopeOnecolumn(., "pollScore")))
df_slopePollScore$PollinatorScore = df_slopePollScore$slope
sf_slopePollScore                 = merge(polys,df_slopePollScore)
ggplot(sf_slopePollScore) +
  geom_sf(data = sf_slopePollScore, aes(fill = PollinatorScore))+
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)



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




