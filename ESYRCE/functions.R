library(rgeos)
library(sp)
library(rgdal)
library(tidyverse)
        
####################################
# INPUT: 
# - data from a group by operation. 
# - columns to aggregate
# OUTPUT: slope of a linear regression using the aggregated values of the columns at every year with data
####################################
calculateSlope <- function(data, columns) {
  xaxis = data$YEA # xaxis: years
  yaxis = rowSums(data[,columns]) # yaxis: sum of the columns selected for aggregation
  valid = !is.na(xaxis) & !is.na(yaxis) # discard NA values
  xaxis = xaxis[valid]
  yaxis = yaxis[valid]
  slope = NA
  if (length(xaxis) > 2) { # calculate slope only when we have at least 3 points (with 2 points, the slope might be misleading)
    lmMod <- lm(yaxis ~ xaxis, data=data.frame(xaxis = xaxis, yaxis=yaxis))
    summ  = summary(lmMod)
    coeff = summ$coefficients
    slope = coeff[2]
  }
  return(tibble::tibble(slope = slope, longitude=data$longitude, latitude=data$latitude))
}


####################################
# INPUT: dataframe from ESYRCE dataset, with coordinates of the segments encoded in the fields "D1_HUS" and "D2_NUM", and size of the segment 
# OUTPUT: coordinates in the EPSG:4326 reference system, for every combination "D1_HUS", "D2_NUM"
####################################
calculateLookupCoord <- function(data) {
  df_dups <- data[c("D1_HUS", "D2_NUM")]
  lookup = data[!duplicated(df_dups),]
  lookup = do.call("rbind", apply(lookup,1,calculateCoordRow))
  lookup = lookup %>% dplyr::select(-c("segArea"))
  return(lookup)
}

calculateCoordRow <- function(dataRow) {
  data            = data.frame(as.list(dataRow))
  zone            = as.character(data["D1_HUS"])
  num             = as.character(data["D2_NUM"])
  segAreaM2       = data$segArea*1e4
  shiftToCentroid = sqrt(segAreaM2) / 2
  x               = as.numeric(substr(num,1,3))*1000 + shiftToCentroid
  y               = as.numeric(substr(num,4,7))*1000 + shiftToCentroid
  pts  = SpatialPoints(data.frame(x=x, y=y))
  proj = paste0("+init=epsg:230",zone)
  pts@proj4string <- CRS(proj)
  ptsT = spTransform(pts, CRS("EPSG:4326"))
  newdata = data
  newdata["longitude"] = as.numeric(ptsT@coords[,"x"])
  newdata["latitude"]  = as.numeric(ptsT@coords[,"y"])
  return(newdata)
}


####################################
# INPUT: 
# - dataset with coordinates [longitude, latitude] in the CRS("EPSG:4326")
# - map where the Spanish provinces and ccaa are delimited (must be defined before the call to the function)
# OUTPUT: array with the Spanish province or com. aut. where each point is located
####################################
calculateRegion <- function(data) {
  data = data.frame(as.list(data))
  x    = as.numeric(data["longitude"])
  y    = as.numeric(data["latitude"])
  pts  = SpatialPoints(data.frame(x=x, y=y))
  pts@proj4string <- CRS("EPSG:4326")
  info = over(pts, map) # get information of the map at the location of the point (note: the map MUST be in the same CRS ("EPSG:4326") )
  newdata = data
  newdata["province"] = as.character(info$NAME_2)
  newdata["region"]   = as.character(info$NAME_1)
  return(newdata)
}


####################################
# INPUT: 
# - dataframe with locations [longitude, latitude]
# - dataframe with model values at those locations
# - digits to round coordinates
# OUTPUT: original dataset with added column with model values
####################################
addModelValues <- function(df_base, df_model, digits) {
  df_model = roundCoordinates(df_model,digits)
  df_base  = roundCoordinates(df_base,digits)
  df_out   = merge(df_base, df_model, by=c("longitude","latitude"), all.x = TRUE)
  if (nrow(df_base) != nrow(df_out)) print("ERROR addModelValues-> CHECK") # Sanity check
  return(df_out)
}
roundCoordinates <- function(data, digits) {
  data$longitude = as.numeric(data$longitude)
  data$latitude  = as.numeric(data$latitude)
  data[,"longitude"] = round(data$longitude, digits=digits)
  data[,"latitude"]  = round(data$latitude, digits=digits)
  return(data)
}


####################################
# INPUT: dataframe with coordinates in the fields "x_center" and "y_center" (EPSG:4326)
# OUTPUT: gpkg file with the dataframe information at the locations of [x_center, y_center]
####################################
saveGPKG <- function(data, outfile) {
  x   = data["x_center"]
  y   = data["y_center"]
  pts = SpatialPoints(data.frame(x=x, y=y))
  pts@proj4string <- CRS("EPSG:4326")
  writeOGR(pts,dsn=outfile,layer=layer,driver="GPKG")
}

mean1000 = function(x){
  return(mean(x)*1000)
}

median1000 = function(x){
  return(median(x)*1000)
}

