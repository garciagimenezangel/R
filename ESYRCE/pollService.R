library(dplyr)
library(gstat) 

###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
GEEFolder  = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"

# Define which years of the interval 2001-2019 correspond to which version of CORINE
corine2000 = seq(1996,2003)
corine2006 = seq(2004,2009)
corine2012 = seq(2010,2015)
corine2018 = seq(2016,2021)

# Read datasets
dataFile     = paste0(dataFolder, "geo_metrics_climate_intensif_20-12-18.csv")
df_data      = read.csv(dataFile, header=T)
modelFile    = paste0(dataFolder, "geo_model_20-12-18.csv")
df_pollModel = read.csv(modelFile, header=T)

#######################
# Pollination service 
#######################
# Calculate as (pollinator score) - (demand), but set to 0 places where there is low demand

# Identify places with low demand
threshold = 0.1
lowDemand = df_data$demand < threshold

# Get pollinators' score at every point with demand larger than threshold
model = "ZonasNaturales_man0_mod0"
getPollScore = function(x) {
  year = x["YEA"]
  hus  = x["D1_HUS"]
  num  = x["D2_NUM"]
  if (year %in% corine2000) corineYear = 2000
  if (year %in% corine2006) corineYear = 2006
  if (year %in% corine2012) corineYear = 2012
  if (year %in% corine2018) corineYear = 2018
  selected = df_pollModel$D1_HUS == hus & df_pollModel$D2_NUM == num & df_pollModel$YEA == corineYear
  pollScore = df_pollModel[selected,model] 
  return(pollScore)
}
df_data$pollScore = 0
pollScore = apply(df_data[!lowDemand,], 1, FUN = getPollScore) 
df_data[!lowDemand,"pollScore"] = pollScore

# Get value of the service
df_data$pollService = df_data$pollScore - df_data$demand

# Save data 
write.csv(df_data, file=paste0(dataFolder, "geo_metrics_climate_intensif_pollService_20-12-18.csv"), row.names=FALSE)





