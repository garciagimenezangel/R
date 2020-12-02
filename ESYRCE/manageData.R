library(dplyr)
###########
setwd("C:/Users/angel.gimenez/Documents/REPOSITORIES/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

# Read dataset
dataFile = "../../../DATA/OBServ/ESYRCE/PROCESSED/z30/metrics/data_flag012_CCAA.csv"
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
# Add province and region to dataset
#####################################
fileShp = "C:/Users/angel.gimenez/Documents/DATA/OBServ/Administrative areas/ESP_adm/ESP_adm2.shp"
map     = readOGR(fileShp)
df_new  = do.call("rbind", apply(df_data,1,calculateRegion))
write.csv(df_new, file="../../../DATA/OBServ/ESYRCE/PROCESSED/z30/metrics/data_flag012_CCAA.csv")




