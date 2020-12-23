library(rgeos)
library(sp)
library(rgdal)
library(tidyverse)
        
####################################
# INPUT: 
# - data from a group by operation. 
# - columns to aggregate (must be defined before the call to the function)
# OUTPUT: slope of a linear regression using the aggregated values of the columns at every year with data
####################################
calculateSlope <- function(data, columns) {
  xaxis = data$YEA # xaxis: years
  yaxis = rowSums(data[,columns]) # yaxis: sum of the columns selected for aggregation (defined before the call to the function)
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
  return(slope)
}


####################################
# INPUT: dataframe from ESYRCE dataset, with coordinates of the segments encoded in the fields "D1_HUS" and "D2_NUM", and size of the segment 
# OUTPUT: coordinates in the EPSG:4326 reference system, for every combination "D1_HUS", "D2_NUM"
####################################
calculateLookupCoord <- function(data) {
  df_dups <- data[c("D1_HUS", "D2_NUM")]
  lookup = data[!duplicated(df_dups),]
  lookup = do.call("rbind", apply(lookup,1,calculateCoordRow))
  lookupCoord = lookupCoord %>% select(-c("segArea"))
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
  newdata["x_center"] = ptsT@coords[,"x"]
  newdata["y_center"] = ptsT@coords[,"y"]
  return(newdata)
}


####################################
# INPUT: 
# - dataset with coordinates [x_center, y_center] in the CRS("EPSG:4326")
# - map where the Spanish provinces and ccaa are delimited (must be defined before the call to the function)
# OUTPUT: array with the Spanish province or com. aut. where each point is located
####################################
calculateRegion <- function(data) {
  data = data.frame(as.list(data))
  x    = as.numeric(data["x_center"])
  y    = as.numeric(data["y_center"])
  pts  = SpatialPoints(data.frame(x=x, y=y))
  pts@proj4string <- CRS("EPSG:4326")
  ptsT = spTransform(pts, proj4string(map)) # transform coordinates into map's crs
  info = over(ptsT, map) # get information of the map at the location of the point
  newdata = data
  newdata["province"] = as.character(info$NAME_2)
  newdata["region"]   = as.character(info$NAME_1)
  newdata["longitude"]= coordinates(ptsT)[,"x"]
  newdata["latitude"] = coordinates(ptsT)[,"y"]
  return(newdata)
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

