library(raster)
library(rgdal)

######################################################
# Clean session
rm(list = ls())

# Working directory 
work_dir = "C:/Users/angel.gimenez/Documents/Projects/OBServ/R/Extract_geodata/";
setwd(work_dir);

# Configuration file
source("config.R")

# Extract geodata
if (GEE_tables) source("GetGeodata_GEEtables.R")
if (KLAB_layers) source("GetGeodata_KLabLayers.R")

# Write csv 
write.csv(df_out, out_file, row.names=FALSE) 
print("Output table:");
print(out_file);
