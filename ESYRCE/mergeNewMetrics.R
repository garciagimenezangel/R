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
newMetrics     = paste0(dataFolder, "avgSizeLCType_2021-02.csv")
df_newMetrics  = read.csv(newMetrics, header=T)

# Left join
df_merged = merge(df_newMetrics, df_currMetrics, by=c("D1_HUS","D2_NUM","YEA"), all.x=TRUE)

# Save
write.csv(df_merged, file=paste0(paste0(dataFolder, "metrics_v2021-02.csv")),row.names=FALSE)







