library(dplyr)
library(gstat) 
library(raster)
###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

# Read datasets
dataFolder   = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
dataFile     = paste0(dataFolder, "geo_metrics_climate_20-12-18.csv")
df_data      = read.csv(dataFile, header=T)
df_data$region   = abbreviate(df_data$region)
df_data$province = abbreviate(df_data$province)
modelFile    = paste0(dataFolder, "geo_model_20-12-18.csv")
df_pollModel = read.csv(modelFile, header=T)


##########################
# INTENSIFICATION METRICS
##########################
# crop cover
columns     = paste0("prop_",agriLand)
df_aux      = df_data[,columns]
cropCover   = rowSums(df_aux)
stCropCover = scale(cropCover)
hist(stCropCover)
summary(stCropCover)
# crop field size 
columns     = "avgFieldSize"
fieldSize   = df_data[,columns]
stFieldSize = scale(fieldSize)
hist(stFieldSize)
summary(stFieldSize)
# crop diversity
columns     = "heterogeneity"
diversity   = -df_data[,columns] # change sign to go along with intensification
stDiversity = scale(diversity)
hist(stDiversity)
summary(stDiversity)
# INTENSIFICATION
intensification = stCropCover + stFieldSize + stDiversity
hist(intensification)
summary(intensification)

# Save as a new column:
df_data$intensification = intensification
write.csv(df_data, file=paste0(dataFolder,"geo_metrics_climate_intensif_20-12-18.csv"),row.names=FALSE)

#############################################################################
# YIELD vs INTENSIFICATION (grouped by level of pollinators' dependence) 
#############################################################################
# 1. Temporal "correction"
# From Deguines 2014, one could account for the (hypothetical) increase of the yield through time, by 
# means of a regression, using the residuals + predicted yield at the center of the time interval.
# However, from what I can see in the crops that I've tested (see previous lines of codee), 
# there is no evident trend upwards with time...
# Should we do the temporal "correction" of the hypothetical increase of the yield in time? 

# 2. Z-scores (by region and province)
zScoreNames  = c("crop","YEA","region","yield","sd")
zScoreRegion = data.frame(matrix(ncol=length(zScoreNames),nrow=0, dimnames=list(NULL, zScoreNames)))
# Mean by region and year
for (crop in agriLand) {
  thresholdYield      = 50 # take values of yields (kg/ha) larger than this threshold
  cropCol             = paste0("yield_",crop)
  useCols             = c("YEA", "region", cropCol)
  indYield            = !is.na(df_data[,cropCol]) & df_data[,cropCol] > thresholdYield
  df_yield            = df_data[indYield, useCols]
  df_crop_meanSd      = df_yield %>% group_by(YEA, region) %>% summarise_at(.vars = cropCol, .funs = c(mean,sd))
  df_crop_meanSd      = df_crop_meanSd %>% rename(yield=fn1, sd=fn2) %>% drop_na()
  df_crop_meanSd$crop = crop
  zScoreRegion        = rbind(zScoreRegion, df_crop_meanSd)
}
zScoreNames  = c("crop","YEA","province","yield","sd")
zScoreProv   = data.frame(matrix(ncol=length(zScoreNames),nrow=0, dimnames=list(NULL, zScoreNames)))
# Mean by province and year
for (crop in agriLand) {
  thresholdYield      = 50 # take values of yields (kg/ha) larger than this threshold
  cropCol             = paste0("yield_",crop)
  useCols             = c("YEA", "province", cropCol)
  indYield            = !is.na(df_data[,cropCol]) & df_data[,cropCol] > thresholdYield
  df_yield            = df_data[indYield, useCols]
  df_crop_meanSd      = df_yield %>% group_by(YEA, province) %>% summarise_at(.vars = cropCol, .funs = c(mean,sd))
  df_crop_meanSd      = df_crop_meanSd %>% rename(yield=fn1, sd=fn2) %>% drop_na()
  df_crop_meanSd$crop = crop
  zScoreProv          = rbind(zScoreProv, df_crop_meanSd)
}
# Yield variability
zScoreRegion$yield_variab = zScoreRegion$sd*1000 / zScoreRegion$yield
zScoreProv$yield_variab   = zScoreProv$sd*1000 / zScoreProv$yield
# Z-score
# Region
meansZScore = zScoreRegion %>% 
  group_by(crop, YEA) %>% 
  summarise(mean_yield = mean(yield), 
            mean_variab = mean(yield_variab), 
            sd_yield = sd(yield), 
            sd_variab = sd(yield_variab)) 
zScoreRegion = merge(zScoreRegion, meansZScore, all.x=TRUE) %>% drop_na()
zScoreRegion$z_meanYield   = (zScoreRegion$yield - zScoreRegion$mean_yield) / zScoreRegion$sd_yield
zScoreRegion$z_yieldVariab = (zScoreRegion$yield_variab - zScoreRegion$mean_variab) / zScoreRegion$sd_variab
# Province
meansZScore = zScoreProv %>% 
  group_by(crop, YEA) %>% 
  summarise(mean_yield = mean(yield), 
            mean_variab = mean(yield_variab), 
            sd_yield = sd(yield), 
            sd_variab = sd(yield_variab)) 
zScoreProv = merge(zScoreProv, meansZScore, all.x=TRUE) %>% drop_na()
zScoreProv$z_meanYield   = (zScoreProv$yield - zScoreProv$mean_yield) / zScoreProv$sd_yield
zScoreProv$z_yieldVariab = (zScoreProv$yield_variab - zScoreProv$mean_variab) / zScoreProv$sd_variab

# climate?
# Water deficit
watDef= df_data[,c("YEA","water_def")] %>% drop_na()
zWatDef = watDef %>% group_by(YEA) %>% summarize(mean=mean(water_def))
zWatDef$zscore = scale(zWatDef$mean)
aa = zScore %>% group_by(YEA) %>% summarize(median=median(z_meanYield))
bb = merge(aa,zWatDef)
plot(bb$median, bb$zscore)
# Total precipitation
prcp= df_data[,c("YEA","total_prec")] %>% drop_na()
zprcp = prcp %>% group_by(YEA) %>% summarize(mean=mean(total_prec))
zprcp$zscore = scale(zprcp$mean)
aa = zScore %>% group_by(YEA) %>% summarize(median=median(z_meanYield))
bb = merge(aa,zprcp)
plot(bb$median, bb$zscore)
# tmin
tmin= df_data[,c("YEA","tmin")] %>% drop_na()
ztmin = tmin %>% group_by(YEA) %>% summarize(mean=mean(tmin))
ztmin$zscore = scale(ztmin$mean)
aa = zScore %>% group_by(YEA) %>% summarize(median=median(z_meanYield))
bb = merge(aa,ztmin)
plot(bb$median, bb$zscore)
# tmax
tmax= df_data[,c("YEA","tmax")] %>% drop_na()
ztmax = tmax %>% group_by(YEA) %>% summarize(mean=mean(tmax))
ztmax$zscore = scale(ztmax$mean)
aa = zScore %>% group_by(YEA) %>% summarize(median=median(z_meanYield))
bb = merge(aa,ztmax)
plot(bb$median, bb$zscore)


# 3. Intensification metrics: avgFieldSize(sin disolver), propSeminatural/edgeDensitySeminat, avgSeminatSize, heterogeneity
intensif1          = scale(df_data$avgFieldSize) # avgFieldSize
propSeminat        = rowSums(df_data[,paste0("prop_",seminatural)])
intensif2          = - scale( propSeminat/df_data$edgeDenSemiDiss ) # propSeminatural/edgeDensitySeminat
intensif3          = - scale(df_data$avgSeminatSize) # avgSeminatSize
intensif4          = - scale(df_data$heterogeneity)  # heterogeneity
propIrrigated      = rowSums(df_data[,irrigated])
intensif5          = scale(df_data$irrigated)
df_data$intenIndex = (intensif1 + intensif2 +intensif3 + intensif4) / 4
df_data$intenIndex = intensif5
# Plot evolution
# REGION
df_intenIndex      = df_data %>% group_by(YEA, region) %>% summarise(intenIndex=mean(intenIndex, na.rm = TRUE))
boxplot(df_intenIndex$intenIndex ~ df_intenIndex$YEA)
(pl_intens<-ggplot(df_intenIndex, aes(YEA, intenIndex)) +
  geom_point(aes(colour = factor(region)))+ 
  geom_smooth(method="lm", se=TRUE, aes(color=region)))
# PROVINCE
df_intenIndex      = df_data %>% group_by(YEA, province) %>% summarise(intenIndex=mean(intenIndex, na.rm = TRUE))
boxplot(df_intenIndex$intenIndex ~ df_intenIndex$YEA)
(pl_intens<-ggplot(df_intenIndex, aes(YEA, intenIndex)) +
    geom_point(aes(colour = factor(province)))+ 
    geom_smooth(method="lm", se=TRUE, aes(color=province)))

# 4. Pollination index
# Region
df_pollIndex = df_pollModel %>% group_by(YEA, region) %>% summarise(pollIndex=mean(ZonasNaturales_man0_mod0, na.rm = TRUE))
boxplot(df_pollIndex$pollIndex ~ df_pollIndex$YEA)
(pl_poll<-ggplot(df_pollIndex, aes(YEA, pollIndex)) +
    geom_point(aes(colour = factor(region)))+ 
    geom_smooth(method="lm", se=TRUE, aes(color=region)))
# Province
df_pollIndex = df_pollModel %>% group_by(YEA, province) %>% summarise(pollIndex=mean(ZonasNaturales_man0_mod0, na.rm = TRUE))
boxplot(df_pollIndex$pollIndex ~ df_pollIndex$YEA)
(pl_poll<-ggplot(df_pollIndex, aes(YEA, pollIndex)) +
    geom_point(aes(colour = factor(province)))+ 
    geom_smooth(method="lm", se=TRUE, aes(color=province)))

# Z-score
zScore=zScoreRegion
zScoreIntensif = merge(zScore,df_intenIndex,all.x=TRUE)
(pl_zScore<-ggplot(zScoreIntensif, aes(intenIndex, z_meanYield)) +
    geom_point()+ 
    geom_smooth(method="lm", se=TRUE))
reg=lm(z_meanYield~intenIndex, zScoreIntensif)
summary(reg)$r.squared
# Z-score by pollinators' dependence
# NO INCREASE
zScoreNoIncrease = zScore[zScore$crop %in% noIncrease,]
zScoreNoIncrease = merge(zScoreNoIncrease,df_intenIndex,all.x=TRUE)
(pl_intens<-ggplot(zScoreNoIncrease, aes(intenIndex, z_meanYield)) +
    geom_point()+ 
    geom_smooth(method="lm", se=TRUE))
reg=lm(z_meanYield~intenIndex, zScoreNoIncrease)
summary(reg)$r.squared

# ESSENTIAL
zScoreEssential = zScore[zScore$crop %in% essential,]
zScoreEssential = merge(zScoreEssential,df_intenIndex,all.x=TRUE)
(pl_intens<-ggplot(zScoreEssential, aes(intenIndex, z_meanYield)) +
    geom_point()+ 
    geom_smooth(method="lm", se=TRUE))
reg=lm(z_meanYield~intenIndex, zScoreEssential)
summary(reg)$r.squared

# INCREASE
zScoreIncrease = zScore[zScore$crop %in% increase,]
zScoreIncrease = merge(zScoreIncrease,df_intenIndex,all.x=TRUE)
(pl_intens<-ggplot(zScoreIncrease, aes(intenIndex, z_meanYield)) +
    geom_point()+ 
    geom_smooth(method="lm", se=TRUE))
reg=lm(z_meanYield~intenIndex, zScoreIncrease)
summary(reg)$r.squared

# GREAT
zScoreGreat = zScore[zScore$crop %in% great,]
zScoreGreat = merge(zScoreGreat,df_intenIndex,all.x=TRUE)
(pl_intens<-ggplot(zScoreGreat, aes(intenIndex, z_meanYield)) +
    geom_point()+ 
    geom_smooth(method="lm", se=TRUE))
reg=lm(z_meanYield~intenIndex, zScoreGreat)
summary(reg)$r.squared

# MODEST
zScoreModest = zScore[zScore$crop %in% modest,]
zScoreModest = merge(zScoreModest,df_intenIndex,all.x=TRUE)
(pl_intens<-ggplot(zScoreModest, aes(intenIndex, z_meanYield)) +
    geom_point()+ 
    geom_smooth(method="lm", se=TRUE))
reg=lm(z_meanYield~intenIndex, zScoreModest)
summary(reg)$r.squared

# LITTLE
zScoreLittle = zScore[zScore$crop %in% little,]
zScoreLittle = merge(zScoreLittle,df_intenIndex,all.x=TRUE)
(pl_intens<-ggplot(zScoreLittle, aes(intenIndex, z_meanYield)) +
    geom_point()+ 
    geom_smooth(method="lm", se=TRUE))
reg=lm(z_meanYield~intenIndex, zScoreLittle)
summary(reg)$r.squared

# IMPORTANT
zScoreImportant = zScore[zScore$crop %in% pollImportant,]
zScoreImportant = merge(zScoreImportant,df_intenIndex,all.x=TRUE)
(pl_intens<-ggplot(zScoreImportant, aes(intenIndex, z_meanYield)) +
    geom_point()+ 
    geom_smooth(method="lm", se=TRUE))
reg=lm(z_meanYield~intenIndex, zScoreImportant)
summary(reg)$r.squared

# NOT IMPORTANT
zScoreNotImport = zScore[zScore$crop %in% pollNotImport,]
zScoreNotImport = merge(zScoreNotImport,df_intenIndex,all.x=TRUE)
(pl_intens<-ggplot(zScoreNotImport, aes(intenIndex, z_meanYield)) +
    geom_point()+ 
    geom_smooth(method="lm", se=TRUE))
reg=lm(z_meanYield~intenIndex, zScoreNotImport)
summary(reg)$r.squared

