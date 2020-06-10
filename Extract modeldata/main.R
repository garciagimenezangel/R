library(raster)
library(rgdal)
library(dplyr)
library(stringr)
library(rlist)

######################################################
# Clean session
rm(list = ls())

# Working directory
work_dir = "C:/Users/angel.gimenez/Documents/REPOSITORIES/R/Extract modeldata/"
setwd(work_dir);

# Configuration file
source("config.R")
source('../SDMs/lib/dataFunctions.R')

# Extract model data 
# kLAB
#source("GetValues_KLabModels.R");
#df_klab = GetValues_KLabModels();
#df_klab = clean(df_klab, lon="longitude", lat="latitude", species="")

# GEE
source("GetValues_GEEModels.R");
df_gee = GetValues_GEEModels();
df_gee = clean(df_gee, lon="longitude", lat="latitude", species="")

# Merge results from different sources
#df_final = merge(df_klab, df_gee, by = base_col_names)
df_final = df_gee;

# Sort table by the two first columns (they should be "study_id" and "field_id")
ord = order(df_final[,1], as.numeric(df_final[,2]))
df_final = df_final[ord,]

# Write csv 
write.csv(df_final, out_file, row.names=FALSE) 
print("Output table:")
print(out_file)
