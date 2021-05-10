library(dplyr)
rm(list=ls())
###########

# setwd("C:/Users/angel.gimenez/git/R/ESYRCE/")
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

# dataFolder = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/ESYRCE/"
dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"

# Read datasets
dataFile     = paste0(dataFolder, "metrics_v2021-02-25.csv")
df_data      = read.csv(dataFile, header=T)

############################
# Yield gap 
############################
# Get max value per province
df_selected = df_data[,c("province",paste0("yield_",agriLand))]
for (crop in paste0("yield_",agriLand)) {
  df_selected[,crop] = df_selected[,crop] %>% replace_na(0)
}
df_maxyield = df_selected %>% group_by(province) %>% summarise(across(everything(), list(max)))

# Get yield gap as the difference between mean yield and max yield
getYieldGap = function(dataRow, crop) {
  province = dataRow["province"]
  maxYield = df_maxyield[df_maxyield$province == province, paste0("yield_",crop,"_1")]
  yieldGap = maxYield - dataRow[,paste0("yield_",crop)]
  return(tibble::tibble(yield_gap = yieldGap))
}

df_metricsAll = read.csv(file=paste0(dataFolder,"intermediateProducts/slopeMetrics.csv"), header=T)
for (crop in agriLand) {
  if (paste0("mean_yield_",crop) %in% colnames(df_metricsAll)) {
    df_metricsAll[, paste0("yield_gap_",crop)] = NA
    for (province in unique(df_maxyield$province)) {
      print(paste0("Province: ", province, "   Crop: ", crop))
      maxYield = df_maxyield[df_maxyield$province == province, paste0("yield_",crop,"_1")]
      df_metricsAll[df_metricsAll$province == province , paste0("yield_gap_",crop)] = as.numeric(maxYield) - df_metricsAll[df_metricsAll$province == province , paste0("mean_yield_",crop)] 
    }  
  }
}




