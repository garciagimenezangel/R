# The idea is to categorize the segments into 4 categories, according to the percentage of
# seminatural area over time:
# - Always-high: Those that are always over 20% of seminatural cover
# - Lately-high: Those that start below 20% of seminatural cover, but end up over that 20% 
# - Lately-low: Those that start over 20% of seminatural cover, but end up below that 20% 
# - Always-low: Those that are always below 20% of seminatural cover
# Note that we use the linear regression as reference, not the actual values, to smooth out variability

library(dplyr)
library(rlist)
rm(list=ls())
###########

# setwd("C:/Users/angel.gimenez/git/R/ESYRCE/")
setwd("C:/Users/angel/git/R/ESYRCE/")

# Functions
source("./functions.R")

# Re-arrange seminatural category
seminatural = c(seminatural_forest, seminatural_meadow, seminatural_shrub) # seminatural[ !(seminatural %in% seminatural2) ] = "pawlonia","quercusIlexTruffle","carobTree","otherOtherWoody" 

# Read data
# dataFolder = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/ESYRCE/"
dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
df_filledData = read.csv(file=paste0(dataFolder,"metrics_v2021-02-25_FILLED_CLEANYIELD.csv"),header=T)
df_metricsAll = read.csv(file=paste0(dataFolder,"intermediateProducts/slopeMetrics.csv"), header=T)

calculateCategory <- function(data, columnName) {
  xaxis    = data$YEA # xaxis: years
  yaxis    = data[,columnName] # yaxis: column name
  valid    = !is.na(xaxis) & !is.na(yaxis) # discard NA values
  xaxis    = xaxis[valid]
  yaxis    = yaxis[valid]
  lmMod    = lm(yaxis ~ xaxis, data=data.frame(xaxis = xaxis, yaxis=yaxis))
  summ     = summary(lmMod)
  coeff    = summ$coefficients
  initVal  = xaxis[1]*coeff[2] + coeff[1]
  finalVal = xaxis[length(xaxis)]*coeff[2] + coeff[1]
  category = "unknown"
  if ((initVal > 0.2) & (finalVal > 0.2))      { category = "always_high"
  }else if ((initVal < 0.2) & (finalVal > 0.2)){ category = "lately_high"
  }else if ((initVal > 0.2) & (finalVal < 0.2)){ category = "lately_low"
  }else if ((initVal < 0.2) & (finalVal < 0.2)){ category = "always_low" }
  
  return(tibble::tibble(category = category))
}

getCategories = function(columns, outcolname)
{
  baseCols         = c("D1_HUS", "D2_NUM", "province", "YEA")
  df_metric        = df_filledData[,c(baseCols,columns)]
  df_metric$metric = rowSums(df_metric[,columns], na.rm = T)
  df_categories    = df_metric %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(calculateCategory(., "metric")))
  df_categories$category = as.factor(df_categories$category)
  names(df_categories)[names(df_categories) == 'category'] <- paste0("category_",outcolname)
  return(df_categories)
}

# Seminatural
df_categories = getCategories(paste0("prop_",seminatural), "seminatural")

# sanity check
df_check = merge(df_metricsAll[,c("D1_HUS", "D2_NUM", "mean_seminatural")], df_categories, by=c("D1_HUS", "D2_NUM"))

# Merge
df_metricsAll = merge(df_metricsAll, df_categories, by=c("D1_HUS", "D2_NUM"))

# SAVE/READ
write.csv(df_metricsAll, file=paste0(dataFolder,"intermediateProducts/slopeMetrics2.csv"),row.names=FALSE)

