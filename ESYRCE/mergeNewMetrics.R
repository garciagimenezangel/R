library(dplyr)
library(gstat) 
rm(list=ls())
###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

# Read datasets
dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
currentMetrics = paste0(dataFolder, "geo_metrics_climate_intensif_pollService_20-12-18.csv")
df_currMetrics = read.csv(currentMetrics, header=T)
newMetrics     = paste0(dataFolder, "intermediateProducts/newMetrics_2021-02.csv")
df_newMetrics  = read.csv(newMetrics, header=T)

# Left join
df_merged = merge(df_newMetrics, df_currMetrics, by=c("D1_HUS","D2_NUM","YEA"), all.x=TRUE)
df_clean = df_merged[,-str_which(colnames(df_merged),"\\.y")]
colnames(df_clean) = gsub("\\.x","",colnames(df_clean))

# Save
write.csv(df_clean, file=paste0(paste0(dataFolder, "metrics_v2021-02.csv")),row.names=FALSE)


###########################################
# Add new columns from already calculated 
###########################################
df_data = read.csv(paste0(dataFolder, "metrics_v2021-02.csv"), header=T)
##########################
# INTENSIFICATION METRICS
##########################
# active agri land / sum(lowActivity, seminatural, abandoned)
df_agriActive = df_data[,paste0("prop_",agriActive)]
df_agriLowAct = df_data[,paste0("prop_",lowActivity)]
df_seminatural= df_data[,paste0("prop_",seminatural)]
df_abandoned  = df_data[,paste0("prop_",abandAgri)]
activeSum     = rowSums(df_agriActive)
lowActSum     = rowSums(df_agriActive)+rowSums(df_seminatural)+rowSums(df_abandoned)
actIndex      = activeSum-lowActSum
stActIndex    = scale(actIndex)
summary(stActIndex)
# crop field size 
columns     = "avgFieldSize"
fieldSize   = df_data[,columns]
stFieldSize = scale(fieldSize)
hist(stFieldSize)
summary(stFieldSize)
# crop diversity
columns     = "cropsPerCropHa"
diversity   = -df_data[,columns] # change sign to go along with intensification
stDiversity = scale(diversity)
hist(stDiversity)
summary(stDiversity)
# INTENSIFICATION
intensification = stActIndex + stFieldSize + stDiversity
scaledIntensif  = (intensification - min(intensification)) / (max(intensification) - min(intensification))
summary(intensification)
summary(scaledIntensif)

# Save as a new column:
df_data$intensification = scaledIntensif
write.csv(df_data, file=paste0(dataFolder,"metrics_v2021-02.csv"),row.names=FALSE)

############
# Diversity
############
agriColnames = paste0("prop_",agriLand)
baseCols     = c("D1_HUS","D2_NUM","YEA","segArea","segAreaNoWater")
d            = df_data[,c(baseCols,agriColnames)]
d$cropArea   = rowSums(df_data[,agriColnames]) * d$segAreaNoWater
d$alpha      = apply(d[,agriColnames], 1, function(x) sum(x>0)) / d$segAreaNoWater
d$alphaCropArea = apply(d[,agriColnames], 1, function(x) sum(x>0)) / d$cropArea
d[d$cropArea<0.5, 'alphaCropArea'] = 0  # Set diversity to 0 in places where crop area is below certain threshold (e.g. 0.5 hectares)
d[is.na(d)]  = 0
df_data$cropsPerHa = d$alpha
df_data$cropsPerCropHa = d$alphaCropArea
write.csv(df_data, file=paste0(dataFolder,"metrics_v2021-02.csv"),row.names=FALSE)







