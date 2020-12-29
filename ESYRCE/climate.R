# install.packages("remotes")
remotes::install_github("mikejohnson51/climateR")
remotes::install_github("mikejohnson51/AOI")
library(climateR)
library(AOI)
library(sf)

setwd("C:/Users/angel/git/R/ESYRCE/")

dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/Analysis/2020-12/"

# Read datasets
dataFile  = paste0(dataFolder, "geo_metrics_20-12-18.csv")
df_data   = read.csv(dataFile, header=T)

# Get climatic data by year and add to dataframe
for (year in unique(df_data$YEA)) {
  # CREAR STARTDATE Y ENDDATE
  # COGER SITES DE ESOS AÑOS Y HACERLOS ST_AS_SF
  # COGER DATOS CLIMÁTICOS (MENSUALES) EN LOS SITES Y EN ESE PERIODO TEMPORAL, AGREGARLOS Y AÑADIRLOS A DF_DATA
}
test_sites = df_test %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
aa = getTerraClim(AOI=test_sites, param="prcp", startDate = "2018-06-01", endDate ="2019-06-01" )