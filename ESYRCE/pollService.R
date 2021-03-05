library(dplyr)
library(gstat) 

###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

# Define which years of the interval 2001-2019 correspond to which version of CORINE
corine2000 = seq(1996,2003)
corine2006 = seq(2004,2009)
corine2012 = seq(2010,2015)
corine2018 = seq(2016,2021)

# Read datasets
dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
dataFile     = paste0(dataFolder, "metrics_v2021-02-25.csv")
df_data      = read.csv(dataFile, header=T)
modelFile    = paste0(dataFolder, "intermediateProducts/geo_model_21-02-25.csv")
df_pollModel = read.csv(modelFile, header=T)

#######################
# Pollination service 
#######################
# Get pollinators' score at every point
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
pollScore = apply(df_data, 1, FUN = getPollScore) 
pollScore[is.na(pollScore)] = 0

############
# 1. Calculate as (pollinator score) - (demand), but set to 0 places where there is low demand
# Identify places with low demand
threshold = 0.1
lowDemand = df_data$demand < threshold
df_data$pollScore = 0
df_data[!lowDemand,"pollScore"] = pollScore
# Get value of the service
df_data$pollService = 0
df_data[!lowDemand, "pollService"] = df_data[!lowDemand, "pollScore"] - df_data[!lowDemand,"demand"]

############
# 2. Calculate as (2xy - y^2) where x=(pollinator score), y=(demand)
df_data$pollScore = pollScore
x = df_data$pollScore
y = df_data$demand
df_data$pollService2 = 2*x*y - y*y

############
# 3. Calculate as [exp(2xy - y^2)-1]/[exp(1)-1] where x=(pollinator score), y=(demand)
x = df_data$pollScore
y = df_data$demand
df_data$pollService3 = (exp(2*x*y - y*y)-1)/(exp(1)-1)

# Save data 
write.csv(df_data, file=paste0(dataFolder, "metrics_v2021-02-25.csv"), row.names=FALSE)





