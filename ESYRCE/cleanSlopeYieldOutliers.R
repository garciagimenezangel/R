library(plyr)
library(BBmisc)
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
df_metricsAll = read.csv(file=paste0(dataFolder,"intermediateProducts/slopeMetrics_cleanSlopeYield.csv"), header=T)

# Histograms by crop
# Stack crops
df_agriLand       = df_metricsAll[ , grepl( "slope_yield_" , names( df_metricsAll ) ) ]
df_stack_raw      = stack(df_agriLand)
df_stack_raw      = df_stack_raw[!is.na(df_stack_raw$values),]
df_stack_raw$crop = gsub("slope_yield_","",df_stack_raw$ind)
df_stack_raw      = df_stack_raw[c("crop","values")]
colnames(df_stack_raw) = c("crop","slope_yield")
df_stack_raw      = arrange(df_stack_raw, crop)
a = df_stack_raw %>% count(crop)
df_stack_raw = merge(df_stack_raw, a)
# Plot
histo = df_stack_raw[df_stack_raw$n > 100,]
ggplot(histo, aes(slope_yield, fill=crop)) +
  geom_histogram() +
  facet_wrap(~crop, scales="free")

# Remove outliers
for (crop in unique(df_stack_raw$crop)) {
  print(crop)
  slope_yield = df_metricsAll[ , paste0("slope_yield_",crop)]
  if (!all(is.na(slope_yield))) {
    norm     = normalize(slope_yield, method = "standardize") 
    outliers = (abs(norm) > 3) 
    slope_yield[outliers] = NA
    df_metricsAll[ , paste0("slope_yield_",crop)] = slope_yield    
  }
}

# Save
write.csv(df_metricsAll, file=paste0(dataFolder,"slopeMetrics_cleanSlopeYield.csv"),row.names=FALSE)
