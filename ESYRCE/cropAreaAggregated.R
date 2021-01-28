###########
setwd("C:/Users/angel/git/R/ESYRCE/")

# Organize categories
source("./categories.R")

# Functions
source("./functions.R")

# Read datasets
dataFolder   = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
dataFile     = paste0(dataFolder, "geo_metrics_climate_20-12-18.csv")
df_data      = read.csv(dataFile, header=T)
df_data$region   = abbreviate(df_data$region)
df_data$province = abbreviate(df_data$province)

###########
# PROVINCE
###########
# define base dataframe
df_crops     = df_data[,c("D1_HUS","D2_NUM","province","YEA", "segArea","segAreaNoWater")] %>% 
  group_by(YEA,province) %>% 
  summarise(totalArea = sum(segArea, na.rm = TRUE), totalAreaNoWater = sum(segAreaNoWater, na.rm = TRUE))  
  
for (crop in agriLand) {
  areaCropBySite = df_data[,paste0("prop_",crop)]*df_data$segAreaNoWater  # calculate area of the crop in hectares (from proportion in the segment)
  df_aux         = cbind(df_data[,c("D1_HUS","D2_NUM","province","YEA")],areaCropBySite) # add area of the crop to the dataframe
  df_aux         = df_aux %>% group_by(YEA,province) %>% summarise(cropArea = sum(areaCropBySite, na.rm = TRUE)) # aggregate by province and year 
  df_crops[,paste0("area_",crop)] = df_aux$cropArea
}

write.csv(df_crops, file=paste0(dataFolder,"cropAreaByProvince.csv"),row.names=FALSE)

###########
# Com.Aut.
###########
# define base dataframe
df_crops     = df_data[,c("D1_HUS","D2_NUM","region","YEA", "segArea","segAreaNoWater")] %>% 
  group_by(YEA,region) %>% 
  summarise(totalArea = sum(segArea, na.rm = TRUE), totalAreaNoWater = sum(segAreaNoWater, na.rm = TRUE))  

for (crop in agriLand) {
  areaCropBySite = df_data[,paste0("prop_",crop)]*df_data$segAreaNoWater  # calculate area of the crop in hectares (from proportion in the segment)
  df_aux         = cbind(df_data[,c("D1_HUS","D2_NUM","region","YEA")],areaCropBySite) # add area of the crop to the dataframe
  df_aux         = df_aux %>% group_by(YEA,region) %>% summarise(cropArea = sum(areaCropBySite, na.rm = TRUE)) # aggregate by province and year 
  df_crops[,paste0("area_",crop)] = df_aux$cropArea
}

write.csv(df_crops, file=paste0(dataFolder,"cropAreaByComAut.csv"),row.names=FALSE)



