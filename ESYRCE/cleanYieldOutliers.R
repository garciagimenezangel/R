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
dataFile     = paste0(dataFolder, "metrics_v2021-02-25.csv")
df_data      = read.csv(dataFile, header=T)

# Histograms by crop
# Stack crops
df_agriLand       = df_data[ , paste0("yield_", agriLand)]
df_stack_raw      = stack(df_agriLand)
df_stack_raw      = df_stack_raw[!is.na(df_stack_raw$values),]
df_stack_raw$crop = gsub("yield_","",df_stack_raw$ind)
df_stack_raw      = df_stack_raw[c("crop","values")]
colnames(df_stack_raw) = c("crop","yield")
df_stack_raw      = arrange(df_stack_raw, crop)
a = df_stack_raw %>% count(crop)
df_stack_raw = merge(df_stack_raw, a)
# Plot
histo = df_stack_raw[df_stack_raw$n > 100,]
ggplot(histo, aes(yield, fill=crop)) +
  geom_histogram() +
  facet_wrap(~crop, scales="free")

# Remove outliers
threshold = 50 # minimum yield to be considered
for (crop in agriLand) {
  print(crop)
  yield    = df_data[ , paste0("yield_",crop)]
  if (!all(is.na(yield))) {
    norm     = normalize(yield, method = "standardize") 
    outliers = (abs(norm) > 3) | (yield <= threshold)
    yield[outliers] = NA
    df_data[ , paste0("yield_",crop)] = yield    
  }
}

# Save
write.csv(df_data, file=paste0(dataFolder,"metrics_v2021-02-25_CLEAN_YIELD.csv"),row.names=FALSE)
