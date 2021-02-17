library(rgeos)
library(sp)
library(rgdal)
library(tidyverse)
source("./categories.R")

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
# INPUT: 
# - data from a group by operation. 
# - column name
# OUTPUT: slope of a linear regression using the aggregated values of the columns at every year with data
####################################
calculateSlopeOnecolumn <- function(data, columnName) {
  xaxis = data$YEA # xaxis: years
  yaxis = data[,columnName] # yaxis: column name
  valid = !is.na(xaxis) & !is.na(yaxis) # discard NA values
  xaxis = xaxis[valid]
  yaxis = yaxis[valid]
  slope = NA
  if (length(xaxis) > 1) { # calculate slope only when we have at least 3 points (with 2 points, the slope might be misleading)
    lmMod <- lm(yaxis ~ xaxis, data=data.frame(xaxis = xaxis, yaxis=yaxis))
    summ  = summary(lmMod)
    coeff = summ$coefficients
    slope = coeff[2]
  }
  else { slope = 0 }
  return(tibble::tibble(slope = slope))
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


####################################
# INPUT: data file where land cover at control points has been registered
# OUTPUT: data frame with the accumulated number of transitions for every combination of land cover type
####################################
getLandCoverTransitionsFromControlPoints = function(dataFile, timeInterval=c(1900,2100)) {
  print(paste("Calculating Land Cover Transitions From Control Points, time interval:",timeInterval[1],"-",timeInterval[2]))
  df_data                    = read.csv(dataFile, header=T)
  df_data                    = df_data[df_data$YEA >= timeInterval[1] & df_data$YEA <= timeInterval[2], ]
  lccp_landcovertypes        = c(landcovertypes, "water", "other") # water also captured at control points, and 'other' to account for unidentified cover types
  df_LCtransitions           = data.frame(matrix(0, ncol = length(lccp_landcovertypes), nrow = length(lccp_landcovertypes)))
  rownames(df_LCtransitions) = lccp_landcovertypes
  colnames(df_LCtransitions) = lccp_landcovertypes
  lccpNames                  = c("lccp1", "lccp2", "lccp3", "lccp4", "lccp5", "lccp6", "lccp7", "lccp8", "lccp9")
  df_LCCP                    = df_data[, c("D1_HUS", "D2_NUM", "YEA", lccpNames)]
  husOld                     = ""
  numOld                     = ""
  lccpOldValues              = data.frame(matrix("", ncol = 9, nrow = 1))
  lccpNewValues              = data.frame(matrix("", ncol = 9, nrow = 1))
  colnames(lccpOldValues)    = lccpNames
  colnames(lccpNewValues)    = lccpNames
  for (i in 1:nrow(df_LCCP)) {
    husNew     = df_LCCP[i, "D1_HUS"]
    numNew     = df_LCCP[i, "D2_NUM"]
    isNewSegment = (husNew != husOld | numNew != numOld)
    for (j in 1:9) {
      # get lccp's 
      if (df_LCCP[i,lccpNames[j]] %in% lccp_landcovertypes) { # if new lccp valid
        
        lccpNewValues[1, lccpNames[j]] = df_LCCP[i, lccpNames[j]] # get new lccp
        
        if (!isNewSegment & (lccpOldValues[1,lccpNames[j]] %in% lccp_landcovertypes) ) { # if not new segment and old lccp valid, save transition
          lccpOld = lccpOldValues[1,lccpNames[j]]
          lccpNew = lccpNewValues[1,lccpNames[j]]                    
          df_LCtransitions[lccpOld, lccpNew] = df_LCtransitions[lccpOld, lccpNew] + 1
        }
        
        lccpOldValues[1,lccpNames[j]] = lccpNewValues[1, lccpNames[j]] # update old lccp
      }
      else {
        # lccp not valid, skip
      }
    }
    husOld = husNew
    numOld = numNew
  }
  return(df_LCtransitions)
}


####################################
# INPUT: data file with the proportion of every land cover type at each segment, and area of the segment
# OUTPUT: data frame with the accumulated area exchanged between any combination of land cover type
####################################
getLandCoverTransitionsFromProportion = function(dataFile, timeInterval=c(1900,2100)) {
  print(paste("Calculating Land Cover Transitions From Proportion, time interval:",timeInterval[1],"-",timeInterval[2]))
  df_data                    = read.csv(dataFile, header=T)
  df_data                    = df_data[df_data$YEA >= timeInterval[1] & df_data$YEA <= timeInterval[2], ]
  df_LCtransitions           = data.frame(matrix(0, ncol = length(landcovertypes), nrow = length(landcovertypes)))
  rownames(df_LCtransitions) = landcovertypes
  colnames(df_LCtransitions) = landcovertypes
  df_LCtransitionsOld        = df_LCtransitions # use a copy to do sanity checks at every step
  df_LCprop                  = df_data[, c("D1_HUS", "D2_NUM", "YEA", "segAreaNoWater", prop_landcovertypes)]
  df_LCarea                  = df_LCprop[, prop_landcovertypes] * df_LCprop$segAreaNoWater
  colnames(df_LCarea)        = landcovertypes
  husOld                     = df_LCprop[1, "D1_HUS"]
  numOld                     = df_LCprop[1, "D2_NUM"]
  segAreaOld                 = df_LCprop[1, "segAreaNoWater"]
  for (i in 2:nrow(df_LCprop)) {
    husNew     = df_LCprop[i, "D1_HUS"]
    numNew     = df_LCprop[i, "D2_NUM"]  
    segAreaNew = df_LCprop[i, "segAreaNoWater"]
    if (husNew == husOld & numNew == numOld & abs(segAreaNew-segAreaOld) < 1e-3) { # if area changes (rare), we skip the row
      
      # Use this row and the previous to obtain gains and loses in area
      changeVector = df_LCarea[i,landcovertypes] - df_LCarea[i-1,landcovertypes]
      changeVector[abs(changeVector) < 1e-3] = 0 # remove negligible changes
      #Sanity check:
      if (abs(sum(changeVector)) > 1e-2) {
        print(paste("Warning: gains not equal to loses in row:",i))
        print(paste("Difference:",sum(changeVector)))
      }
      else {
        # LC types according to change in extension
        noChange = landcovertypes[df_LCarea[i-1,landcovertypes]>1e-3 & changeVector==0]  # there was some area of the lc type and no change
        winners  = landcovertypes[changeVector>0]
        losers   = landcovertypes[changeVector<0]
        
        # Set win/loss matrices
        win  = matrix(changeVector[changeVector>0]) / sum(changeVector[changeVector>0])
        loss = matrix(changeVector[changeVector<0]) 
        rownames(win)  = winners
        rownames(loss) = losers
        
        for (lcKept in c(noChange,winners)) {
          df_LCtransitions[lcKept,lcKept] = df_LCtransitionsOld[lcKept,lcKept] + df_LCarea[i-1,lcKept] # add all the area that was already there, because no change or increased, so we assume all has been kept
        }
        for (lcLos in losers) {
          df_LCtransitions[lcLos,lcLos] = df_LCtransitionsOld[lcLos,lcLos] + df_LCarea[i,lcLos] # add the area that remains
          # Find the total share of the loss for each winner
          totalLoss = -changeVector[,lcLos]
          for (lcWin in winners) {
            df_LCtransitions[lcLos,lcWin] = df_LCtransitionsOld[lcLos,lcWin] + totalLoss*win[lcWin,]
          }
        }
        # Sanity check
        for (loser in losers){
          testSums = rowSums(df_LCtransitions[loser,] - df_LCtransitionsOld[loser,])
          if( all(abs(df_LCarea[i-1,loser] - testSums) > 1e-3) ) print(paste("Warning: area lost not well distributed among winners:",i))
        }
      }
    }
    
    husOld     = husNew
    numOld     = numNew
    segAreaOld = segAreaNew
    df_LCtransitionsOld = df_LCtransitions
  }
  return(df_LCtransitions)
}

####################################
# INPUT: data frame of area for each land cover transition (row=origin, col=destination)
# OUTPUT: data frame with the probability of the transition with row=destination and col=origin
####################################
getLCoriginProb = function(df_LCtransitions) {
  df_LCorigin = as.data.frame(t(df_LCtransitions))
  df_LCorigin_prob = df_LCorigin*0
  for (lc in landcovertypes) {
    if (rowSums(df_LCorigin[lc,]) > 0) df_LCorigin_prob[lc, ] = df_LCorigin[lc, ] /  rowSums(df_LCorigin[lc,])
  }
  return(df_LCorigin_prob)
}

####################################
# INPUT: data frame of area for each land cover transition (row=origin, col=destination)
# OUTPUT: data frame with the probability of the transition with row=origin and col=destination
####################################
getLCdestinProb = function(df_LCtransitions) {
  df_LCdestin = df_LCtransitions
  df_LCdestin_prob = df_LCdestin*0
  for (lc in landcovertypes) {
    if (rowSums(df_LCdestin[lc,]) > 0) df_LCdestin_prob[lc, ] = df_LCdestin[lc, ] /  rowSums(df_LCdestin[lc,])
  }
  return(df_LCdestin_prob)
}

####################################
# INPUT: data frame of area for each land cover transition (row=origin, col=destination)
# OUTPUT: data frame with the probability of the transition with row=destination and col=origin
# by group of land cover types (groups in he variable 'groups')
####################################
getLCoriginProbByGroup = function(df_LCtransitions) {
  df_LCorigin = as.data.frame(t(df_LCtransitions))
  df_LCorigin_gr = data.frame(matrix(0, ncol = length(groups), nrow = length(groups)))
  rownames(df_LCorigin_gr) = lapply(groups, `[[`, 1)
  colnames(df_LCorigin_gr) = lapply(groups, `[[`, 1)
  for(groupRow in groups) {
    for (groupCol in groups)
    {
      rowName    = groupRow[1]
      colName    = groupCol[1]
      lcTypesRow = groupRow[2:length(groupRow)]
      lcTypesCol = groupCol[2:length(groupCol)]
      df_subset  = df_LCorigin[lcTypesRow, lcTypesCol] 
      df_LCorigin_gr[rowName,colName] = sum(df_subset)
    }
  }
  df_LCorigin_gr_norm = df_LCorigin_gr*0
  for (group in rownames(df_LCorigin_gr)) {
    if (rowSums(df_LCorigin_gr[group,]) > 0) df_LCorigin_gr_norm[group, ] = df_LCorigin_gr[group, ] /  rowSums(df_LCorigin_gr[group,])
  }
  return(df_LCorigin_gr_norm)
}
####################################
# INPUT: data frame of area for each land cover transition (row=origin, col=destination)
# OUTPUT: data frame with the probability of the transition with row=origin and col=destination
# by group of land cover types (groups in he variable 'groups')
####################################
getLCdestinProbByGroup = function(df_LCtransitions) {
  df_LCdestin = df_LCtransitions
  df_LCdestin_gr = data.frame(matrix(0, ncol = length(groups), nrow = length(groups)))
  rownames(df_LCdestin_gr) = lapply(groups, `[[`, 1)
  colnames(df_LCdestin_gr) = lapply(groups, `[[`, 1)
  for(groupRow in groups) {
    for (groupCol in groups)
    {
      rowName    = groupRow[1]
      colName    = groupCol[1]
      lcTypesRow = groupRow[2:length(groupRow)]
      lcTypesCol = groupCol[2:length(groupCol)]
      df_subset  = df_LCdestin[lcTypesRow, lcTypesCol] 
      df_LCdestin_gr[rowName,colName] = sum(df_subset)
    }
  }
  df_LCdestin_gr_norm = df_LCdestin_gr*0
  for (group in rownames(df_LCdestin_gr)) {
    if (rowSums(df_LCdestin_gr[group,]) > 0) df_LCdestin_gr_norm[group, ] = df_LCdestin_gr[group, ] /  rowSums(df_LCdestin_gr[group,])
  }
  return(df_LCdestin_gr_norm)
}

library(scales)
squash_axis <- function(from, to, factor) { 
  # A transformation function that squashes the range of [from, to] by factor on a given axis 
  
  # Args:
  #   from: left end of the axis
  #   to: right end of the axis
  #   factor: the compression factor of the range [from, to]
  #
  # Returns:
  #   A transformation called "squash_axis", which is capsulated by trans_new() function
  
  trans <- function(x) {    
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    
    return(x)
  }
  
  inv <- function(x) {
    
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor
    ito <- x >= from + (to - from)/factor
    
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    
    return(x)
  }
  
  # return the transformation
  return(trans_new("squash_axis", trans, inv))
}


# Counts visits to each segment
countVisitSegment <- function(data) {
  x = data$YEA # xaxis: years
  d1 = data$D1_HUS
  d2 = data$D2_NUM
  count=length(x)
  return(tibble::tibble(d1=d1, d2=d2, count = count))
}

# rescale to the interval [0,1]
rescaleVariable = function(x) {
  y = x-min(x)
  y = y/max(y)
  return(y)
}

# mean x 1000
mean1000 = function(x){
  return(mean(x)*1000)
}

# median x 1000
median1000 = function(x){
  return(median(x)*1000)
}

