# install.packages("remotes")
# remotes::install_github("mikejohnson51/climateR")
# remotes::install_github("mikejohnson51/AOI")
library(climateR)
library(AOI)
library(sf)

setwd("C:/Users/angel/git/R/ESYRCE/")

dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/Analysis/2020-12/"

# Read datasets
dataFile  = paste0(dataFolder, "geo_metrics_20-12-18.csv")
df_data   = read.csv(dataFile, header=T)

# Add columns of climatic variables
df_data$total_prec = NA
df_data$tmin       = NA
df_data$tmax       = NA
df_data$aet        = NA
df_data$water_def  = NA

# Get climatic data by year and add to dataframe
for (year in unique(df_data$YEA)) {
  indYear   = df_data$YEA == year
  startDate = paste0(year-1,"-12-31")
  endDate   = paste0(year,"-12-01")
  sites     = df_data[indYear,] %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  # Total precipitation
  climData  = getTerraClim(AOI=sites, param="prcp", startDate = startDate, endDate = endDate )
  data_extr = extract_sites(climData, sites, "ID")
  df_clim   = as.data.frame(t(data_extr$prcp[,-1])) # remove column of date and transpose, in order to sum monthly data
  df_clim$total = rowSums(df_clim)
  df_data[indYear,]$total_prec = df_clim$total
  
  # Tmin
  climData  = getTerraClim(AOI=sites, param="tmin", startDate = startDate, endDate = endDate )
  data_extr = extract_sites(climData, sites, "ID")
  df_clim   = as.data.frame(t(data_extr$tmin[,-1])) # remove column of date and transpose, in order to average monthly data
  df_clim$mean = rowMeans(df_clim)
  df_data[indYear,]$tmin = df_clim$mean
  
  # Tmax
  climData  = getTerraClim(AOI=sites, param="tmax", startDate = startDate, endDate = endDate )
  data_extr = extract_sites(climData, sites, "ID")
  df_clim   = as.data.frame(t(data_extr$tmax[,-1])) # remove column of date and transpose, in order to average monthly data
  df_clim$mean = rowMeans(df_clim)
  df_data[indYear,]$tmax = df_clim$mean
  
  # aet
  climData  = getTerraClim(AOI=sites, param="aet", startDate = startDate, endDate = endDate )
  data_extr = extract_sites(climData, sites, "ID")
  df_clim   = as.data.frame(t(data_extr$aet[,-1])) # remove column of date and transpose, in order to average monthly data
  df_clim$mean = rowMeans(df_clim)
  df_data[indYear,]$aet = df_clim$mean
  
  # water deficit
  climData  = getTerraClim(AOI=sites, param="water_deficit", startDate = startDate, endDate = endDate )
  data_extr = extract_sites(climData, sites, "ID")
  df_clim   = as.data.frame(t(data_extr$water_deficit[,-1])) # remove column of date and transpose, in order to sum monthly data
  df_clim$total = rowSums(df_clim)
  df_data[indYear,]$water_def = df_clim$total
  
  closeAllConnections() # close connections to avoid accumulation
}

write.csv(df_data, file=paste0(dataFolder,"geo_metrics_climate_20-12-18.csv"),row.names=FALSE)

