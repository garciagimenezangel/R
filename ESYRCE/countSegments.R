# Count segments by year
for(i in seq(2001,2019)) {
  nSeg = nrow(df_data[df_data$YEA == i,])
  print(paste("Year:",i,"n:",nSeg))
}

# Get total area by year
for(i in seq(2001,2019)) {
  dataYear = df_data[df_data$YEA == i,]
  area = sum(dataYear$segArea)
  print(paste("Year:",i,"Area:",area))
}

# Count re-surveys
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
df_resurveys = df_currMetrics %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(countVisitSegment(.)))
hist(df_resurveys$count)
