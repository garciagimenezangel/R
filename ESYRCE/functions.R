library(rgeos)
library(sp)
library(rgdal)

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
# INPUT: 
# - dataset with coordinates
# - map where the Spanish provinces and ccaa are delimited (must be defined before the call to the function)
# OUTPUT: array with the Spanish province or com. aut. where each point is located
####################################
calculateRegion <- function(data) {
  data = data.frame(as.list(data))
  zone = as.character(data["D1_HUS"])
  num  = as.character(data["D2_NUM"])
  x    = as.numeric(substr(num,1,3))*1000
  y    = as.numeric(substr(num,4,7))*1000
  pts  = SpatialPoints(data.frame(x=x, y=y))
  proj = paste0("+init=epsg:230",zone)
  pts@proj4string <- CRS(proj)
  ptsT = spTransform(pts, proj4string(map)) # transform coordinates into map's crs
  info = over(ptsT, map) # get information of the map at the location of the point
  newdata = data
  newdata["province"] = as.character(info$NAME_2)
  newdata["region"]   = as.character(info$NAME_1)
  newdata["longitude"]= coordinates(ptsT)[,"x"]
  newdata["latitude"] = coordinates(ptsT)[,"y"]
  return(newdata)
}


