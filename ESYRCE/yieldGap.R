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

# Compute yield gap: max(province) - mean(segment)
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
write.csv(df_metricsAll, file=paste0(dataFolder,"intermediateProducts/slopeMetrics2.csv"),row.names=FALSE)


# Normalize yields (by province)
for (crop in agriLand) {
  if (paste0("mean_yield_",crop) %in% colnames(df_metricsAll)) {
    df_metricsAll[, paste0("norm_gap_",crop)] = NA
    for (province in unique(df_maxyield$province)) {
      print(paste0("Province: ", province, "   Crop: ", crop))
      yield_gap = df_metricsAll[df_metricsAll$province == province , paste0("yield_gap_",crop)]
      maxVal    = max(yield_gap, na.rm = T)
      minVal    = min(yield_gap, na.rm = T)
      if ( minVal == maxVal ) {
        if (minVal == 0) {
          df_metricsAll[df_metricsAll$province == province , paste0("norm_gap_",crop)] = 0
        }
        else {
          df_metricsAll[df_metricsAll$province == province , paste0("norm_gap_",crop)] = yield_gap / yield_gap
        }
      }
      df_metricsAll[df_metricsAll$province == province , paste0("norm_gap_",crop)] = (yield_gap - minVal) / (maxVal-minVal)
    }  
  }
}

# Poll dep and poll no dep (mean and quartiles)
colPollDep = colnames(df_metricsAll) %in% paste0("norm_gap_",pollDependent)
colPollNot = colnames(df_metricsAll) %in% paste0("norm_gap_",pollNotDepent)
df_pollDep = df_metricsAll[, colnames(df_metricsAll)[colPollDep]]
df_pollNot = df_metricsAll[, colnames(df_metricsAll)[colPollNot]]
df_metricsAll$norm_gap_pollDependent        = rowMeans(df_pollDep, na.rm = T)
df_metricsAll$norm_gap_pollNotDependent     = rowMeans(df_pollNot, na.rm = T)
df_metricsAll$quartile_gap_pollDependent    = ntile(df_metricsAll$norm_gap_pollDependent , 4)
df_metricsAll$quartile_gap_pollNotDependent = ntile(df_metricsAll$norm_gap_pollNotDependent , 4)

