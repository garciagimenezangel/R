library(raster)
library(rgdal)
library(dplyr)
library(stringr)

######################################################
# Clean session
rm(list = ls())

# Working directory
work_dir = "C:/Users/angel.gimenez/Documents/Projects/OBServ/R repo/Extract modeldata/"
setwd(work_dir);

# Configuration file
source("config.R")

# Extract predictions from different sources (for now, only from KLab models)
if (KLAB_models) {
  source("GetValues_KLabModels.R");
  df_klab = GetValues_KLabModels();
}

# Stack predictions (for now, only KLab models)
df_final = rbind(df_klab);

# Write csv 
write.csv(df_final, out_file, row.names=FALSE) 
print("Output table:")
print(out_file)
