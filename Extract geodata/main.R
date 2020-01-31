library(raster)
library(rgdal)
library(dplyr)

######################################################
# Clean session
rm(list = ls())

# Working directory
work_dir = "C:/Users/angel.gimenez/Documents/Projects/OBServ/R repo/Extract geodata/"
setwd(work_dir);

# Configuration file
source("config.R")

# Extract geodata
if (GEE_tables) {
  source("GetGeodata_GEEtables.R");
  geodata_gee = GetGeodata_GEEtables();
}
if (KLAB_layers) {
  source("GetGeodata_KLabLayers.R");
  geodata_KLab = GetGeodata_KLabLayers();
}

# Merge results from different sources
df_final = merge(geodata_gee, geodata_KLab, by = base_col_names)

# Sort table by the two first columns (they should be "study_id" and "field_id")
ord = order(df_final[,1], as.numeric(df_final[,2]))
df_final = df_final[ord,]

# Write csv 
write.csv(df_final, out_file, row.names=FALSE) 
print("Output table:")
print(out_file)
