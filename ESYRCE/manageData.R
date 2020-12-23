library(dplyr)
library(gstat) 

###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

# Read dataset
dataFile = "C:/Users/angel/DATA/ESYRCE/PROCESSED - local testing/z30/metrics/metrics_20-12-18.csv"
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


#####################################
# Add coordinates of the center of the segments
#####################################
# To avoid replication of calculations, get unique combinations of "D1_HUS" and "D2_NUM" and associate coordinates of the center of the segment (assume unique combinations have the same size of the segment, which in practice is not always true)
lookupCoord = calculateLookupCoord(df_data[,c("D1_HUS","D2_NUM","segArea")]) 
write.csv(lookupCoord, file="C:/Users/angel/DATA/ESYRCE/PROCESSED - local testing/z30/metrics/lookup_coordinates.csv",row.names=FALSE)

# Add coordinates by merging dataframe with lookup table
df_new  = merge(df_data, lookupCoord, by=c("D1_HUS","D2_NUM"), all.x = TRUE)
write.csv(df_new, file="C:/Users/angel/DATA/ESYRCE/PROCESSED - local testing/z30/metrics/metrics_20-12-18_centers.csv",row.names=FALSE)

#####################################
# Add province and region to dataset
#####################################
fileShp = "C:/Users/angel/DATA/Administrative areas/ESP_adm/ESP_adm2.shp"
map     = readOGR(fileShp)
df_new  = do.call("rbind", apply(df_new,1,calculateRegion))
write.csv(df_new, file="C:/Users/angel/DATA/ESYRCE/PROCESSED - local testing/z30/metrics/metrics_20-12-18_centers_CCAA.csv",row.names=FALSE)

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

# Some crops, for example maize
neighbours = subset(df_new, x_center>xmin & x_center<xmax & y_center>ymin & y_center<ymax)
coordinates(neighbours)= ~ x_center+y_center
variogramNeighbors = variogram(prop_maize~1, data=neighbours)
plot(variogramNeighbors)




