library(raster)
library(rgdal)
library(dplyr)
library(stringr)
library(rlist)

######################################################
# Clean session
rm(list = ls())

# Working directory
work_dir = "C:/Users/angel.gimenez/git/R/Extract modeldata/"
#work_dir = "C:/Users/angel/git/R/Extract modeldata/"
setwd(work_dir);

# Configuration file
source("config.R")
source('../SDMs/lib/dataFunctions.R')

# Extract model data 
# Rasters (kLAB or CPF)
source("GetValues_Rasters.R");
df_rasters = GetValues_Rasters();

# GEE
source("GetValues_GEEModels.R");
df_gee = GetValues_GEEModels();

# Merge results from different sources
df_final = merge(df_rasters, df_gee, by = base_col_names)

# Sort table by the two first columns (they should be "study_id" and "field_id")
ord = order(df_final[,1], as.numeric(df_final[,2]))
df_final = df_final[ord,]

# Write csv 
write.csv(df_final, out_file, row.names=FALSE) 
print("Output table:")
print(out_file)

