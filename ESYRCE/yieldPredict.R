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
dataFolder   = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/Analysis/2020-12/"
dataFile     = paste0(dataFolder, "geo_metrics_climate_20-12-18.csv")
df_data      = read.csv(dataFile, header=T)
df_data$region   = abbreviate(df_data$region)
df_data$province = abbreviate(df_data$province)
modelFile    = paste0(dataFolder, "geo_model_20-12-18.csv")
df_pollModel = read.csv(modelFile, header=T)


###############
# Explore data
###############
# One plot for each province/region
crop     = "hardWheat"
yieldCol = paste0("yield_",crop)
useCols  = c("D1_HUS", "D2_NUM", "province", "region", "YEA", yieldCol)
indYield = !is.na(df_data[,yieldCol]) & df_data[,yieldCol] != 0
df_yield = df_data[indYield, useCols]
listGroupVals = df_yield %>% group_by(D1_HUS, D2_NUM, province, region) %>% group_split(.)
stPlot = paste0("ggplot(NULL, aes(YEA,",yieldCol,"))")
for (i in seq(1,length(listGroupVals))){ if(nrow(listGroupVals[[i]])>1) { stPlot = paste(stPlot, "+ geom_line(data = listGroupVals[[",as.character(i),"]])") } }
stPlot = paste(stPlot, "+ facet_wrap(~province, nrow=2)", "+ ylim(0, 10000)")
eval(parse(text=stPlot))

# Plot everything together by region? Difficult to see anything
pl_crop<-ggplot(df_yield, aes(YEA, yield_hardWheat)) +
  geom_point(aes(colour = factor(region)))+ 
  geom_smooth(method="lm", se=TRUE, aes(color=region)) + 
  ylim(0, 10000)
pl_crop

# By year
boxplot(df_yield[,yieldCol] ~ df_yield$YEA, range=100, ylim=c(-100, 10000))


#############################################################################
# YIELD vs INTENSIFICATION (grouped by level of pollinators' dependence) 
#############################################################################
# 1. Temporal "correction"
# From Deguines 2014, one could account for the (hypothetical) increase of the yield through time, by 
# means of a regression, using the residuals + predicted yield at the center of the time interval.
# However, from what I can see in the crops that I've tested (see previous lines of codee), 
# there is no evident trend upwards with time...
# Should we do the temporal "correction" of the hypothetical increase of the yield in time? 

# 2. Z-scores (test by region and by province)
# REGION
zScoreNames = c("crop","YEA","region","yield","sd")
zScore = data.frame(matrix(ncol=length(zScoreNames),nrow=0, dimnames=list(NULL, zScoreNames)))
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
  zScore              = rbind(zScore, df_crop_meanSd)
}
# Yield variability
zScore$yield_variab = zScore$sd*1000 / zScore$yield
# Z-score
meansZScore = zScore %>% 
  group_by(crop, YEA) %>% 
  summarise(mean_yield = mean(yield), 
            mean_variab = mean(yield_variab), 
            sd_yield = sd(yield), 
            sd_variab = sd(yield_variab)) 
zScore = merge(zScore, meansZScore, all.x=TRUE) %>% drop_na()
zScore$z_meanYield   = (zScore$yield - zScore$mean_yield) / zScore$sd_yield
zScore$z_yieldVariab = (zScore$yield_variab - zScore$mean_variab) / zScore$sd_variab

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
df_data$intenIndex = (intensif1 + intensif2 +intensif3 + intensif4) / 4
df_intenIndex      = df_data %>% group_by(YEA, region) %>% summarise(mean=mean(intenIndex, na.rm = TRUE))
# Plot evolution
boxplot(df_intenIndex$mean ~ df_intenIndex$YEA)
(pl_intens<-ggplot(df_intenIndex, aes(YEA, mean)) +
  geom_point(aes(colour = factor(region)))+ 
  geom_smooth(method="lm", se=TRUE, aes(color=region)))


#########
# Models
#########
# Model (Ainhoa): Production ~ Climatic (including interanual variability, total_prec + min_temp + max_temp) * 
#Landscape configuration (avgFieldSize + FALTAN: edge density, average size natural area) * 
#Landscape composition (heterogeneity + seminatural) * 
#time (* management) + cuadraticos [medidas repetidas] 

# Select useful columns from df_data
stackCols = paste0("yield_",agriLand)
commCols  = c("D1_HUS", "D2_NUM", "province", "region", "YEA")
metrics   = c("avgFieldSizeDiss", "avgSeminatSizeDiss", "heterogeneity", "edgeDenSeminat", "edgeDensity")
useCols   = c(commCols, metrics, stackCols)
df_yield  = df_data[, useCols]

# Create new columns with new parameters
df_yield$seminatural = rowSums(df_data[,paste0("prop_",seminatural)])

# Add columns to use as parameters
rowSums(data[,columns])

# Stack data to use crops as random factors
stackCols = paste0("yield_",agriLand)
commCols  = c("D1_HUS", "D2_NUM", "province", "region", "YEA")
metrics   = c("avgFieldSizeDiss", "avgSeminatSizeDiss", "heterogeneity", "edgeDenSeminat", "edgeDensity")
useCols   = c(commCols, metrics, stackCols)
df_yield  = df_data[, useCols]
df_stack  = data.frame(df_yield[,c(commCols, metrics)], stack(df_yield[,columns]))
df_stack  = na.omit(df_stack) #remove all NA observations for yield
df_stack$site<-paste(df_stack$D1_HUS, df_stack$D2_NUM) #unique identifier for each site combination of HUS and NUM

df_rape = subset(df_stack, df_stack$ind=="yield_rapeseed")
df_rape_norm = df_rape
df_rape_norm[,c(metrics,"values")] = lapply(df_rape[,c(metrics,"values")], function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/1))

library(lme4)
m1<-lme(values~(heterogeneity + YEA + edgeDensity), 
        random = ~ 1 | site,
        correlation = corCAR1(form = ~ YEA | site), data=df_rape_norm) #include site as random and correlation structure for year
plot(m1)
qqnorm(resid(m1))
qqline(resid(m1))

