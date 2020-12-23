library(dplyr)
library(gstat) 

###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/analysis/2020-12/"
GEEFolder  = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/GEE/ZonasNaturales/"

# Read dataset
dataFile = paste0(dataFolder, "metrics_20-12-18.csv")
df_data  = read.csv(dataFile, header=T)


###############################
# Calculate slopes by category
###############################
# The function calculateSlope will aggregate those columns stored in the variable "columns" before the call to the function

# Seminatural area
columns = seminatural
df_slope_seminatural = df_data %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(slope=calculateSlope(.)))

# Field size
columns = fieldSize
df_slope_fieldSize = df_data %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(slope=calculateSlope(.)))

#...


##########################################################################
# Add coordinates of the center of the segments, and associate with region
##########################################################################
# To avoid replication of calculations, get unique combinations of "D1_HUS" and "D2_NUM" and associate coordinates of the center of the segment (assume unique combinations have the same size of the segment, which in practice is not always true)
lookupCoord = calculateLookupCoord(df_data[,c("D1_HUS","D2_NUM","segArea")])
lookupCoord = lookupCoord %>% select(-c("segArea"))
# Add region associated to the coordinates
lookupCoordRegions = do.call("rbind", apply(lookupCoord,1,calculateRegion))
fileShp = "C:/Users/angel/DATA/Administrative areas/ESP_adm/ESP_adm2.shp"
map     = readOGR(fileShp)
# Save file
write.csv(lookupCoordRegions, file=paste0(dataFolder,"lookup_coordinates_regions.csv"),row.names=FALSE)

# Add coordinates and region by merging dataframe with lookup table
df_new = merge(df_data, lookupCoordRegions, by=c("D1_HUS","D2_NUM"), all.x = TRUE)
write.csv(df_new, file=paste0(dataFolder,"geo_metrics_20-12-18.csv"),row.names=FALSE)


#####################################
# Join data from pollination models
#####################################
modelFiles = paste0(GEEFolder, c("ZonasNaturales2018_man0_mod0.csv"))
for (modelFile in modelFiles) {
  df_pollination  = read.csv(modelFile, header=T)
  df_new = merge(df_new, df_pollination, by=c("D1_HUS","D2_NUM"), all.x = TRUE)
}



#######################
# Calculate variograms
#######################
randomPt = sample_n(df_new, 1)
randomPt$x_center
randomPt$y_center
xmax = randomPt$x_center + 3
xmin = randomPt$x_center - 3
ymax = randomPt$y_center + 3
ymin = randomPt$y_center - 3

# Seminatural
neighbours = subset(df_new, x_center>xmin & x_center<xmax & y_center>ymin & y_center<ymax)
prop_seminatural_cols  = paste0("prop_",seminatural)
neighbours["prop_seminatural"] = rowSums(neighbours[,prop_seminatural_cols])
coordinates(neighbours)= ~ x_center+y_center
variogramNeighbors = variogram(prop_seminatural~1, data=neighbours)
plot(variogramNeighbors)

# Cropfields
neighbours = subset(df_new, x_center>xmin & x_center<xmax & y_center>ymin & y_center<ymax)
prop_agriLand_cols  = paste0("prop_",agriLand)
neighbours["prop_agriLand"] = rowSums(neighbours[,prop_agriLand_cols])
coordinates(neighbours)= ~ x_center+y_center
variogramNeighbors = variogram(prop_agriLand~1, data=neighbours)
plot(variogramNeighbors)

# Individual crop, for example maize
neighbours = subset(df_new, x_center>xmin & x_center<xmax & y_center>ymin & y_center<ymax)
coordinates(neighbours)= ~ x_center+y_center
variogramNeighbors = variogram(prop_maize~1, data=neighbours)
plot(variogramNeighbors)




