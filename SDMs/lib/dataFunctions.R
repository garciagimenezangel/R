library(dismo)
library(dplyr)
library(CoordinateCleaner)
library(rgeos)
library(sp)
library(stringr)

downloadGbifData <- function(species, timespan) {
  pars = c(timeSpan)  # use dismo syntax for query
  splitted = strsplit(species, " ")
  genus = splitted[[1]][1]
  sp = splitted[[1]][2]
  return(gbif(genus, sp, args=pars, geo=TRUE))
}

getOBServInsectSampling <- function(observDir){
  observFiles = list.files(observDir, full.names = TRUE, pattern = "\\.csv$", recursive = FALSE)
  indInsectSampling = lapply(observFiles, function(x){grepl("insect_sampling",x)})
  indInsectSampling = unlist(indInsectSampling)
  fsInsectSampling  = observFiles[indInsectSampling]
  dfInsectSampling  = do.call(rbind, lapply(fsInsectSampling, function(file){ read.csv(file = file, header = TRUE) }))
  dfInsectSampling  = dfInsectSampling[, names(dfInsectSampling) %in% c("study_id", "site_id", "pollinator","abundance")]
  names(dfInsectSampling)[names(dfInsectSampling) == "abundance"] <- "abundance_species"
  return(dfInsectSampling)
}

getOBServFieldData <- function(observDir){
  observFiles = list.files(observDir, full.names = TRUE, pattern = "\\.csv$", recursive = FALSE)
  indFieldData = lapply(observFiles, function(x){grepl("field_level_data",x)})
  indFieldData = unlist(indFieldData)
  fsFieldData  = observFiles[indFieldData]
  dfFieldData  = do.call(rbind, lapply(fsFieldData, function(file){ read.csv(file = file, header = TRUE) }))
  dfFieldData  = dfFieldData[, names(dfFieldData) %in% c("study_id", "site_id", "latitude","longitude", "sampling_year")]
  return(dfFieldData)
}

getPresenceData <- function(species, dfOBServInsectSampling, dfOBServFieldData, dfGbif, otherNames=""){
  # Select subset data from OBServ and GBIF data
  dfInsectPresence = dfOBServInsectSampling[dfOBServInsectSampling$pollinator %in% c(species, otherNames) ,]
  dfObservPresence = merge(dfInsectPresence, dfOBServFieldData, by=c("study_id", "site_id"))
  dfGbifPresence   = dfGbif[c("lat","lon","year")]
  
  # Manipulate names
  names(dfObservPresence)[names(dfObservPresence) == "longitude"] <- "lon"
  names(dfObservPresence)[names(dfObservPresence) == "latitude"]  <- "lat"
  names(dfGbifPresence)[names(dfGbifPresence) == "year"]  <- "sampling_year"
  
  # Tag origin of the points 
  dfGbifPresence$source   = replicate(nrow(dfGbifPresence),"GBIF")
  dfObservPresence$source = replicate(nrow(dfObservPresence),"OBServ")
  
  # Bind GBIF and OBServ
  dfPresence  = plyr::rbind.fill(dfObservPresence, dfGbifPresence)
  dfPresence["presence"]    = rep(1, nrow(dfPresence))
  dfPresence["pollinator"]  = rep(species, nrow(dfPresence)) # other_names -> species name
  return(dfPresence)
}

getAbsenceData <- function(dfPresence, dfOBServInsectSampling, dfOBServFieldData, excludeNames){
  
  # Exclude species that show pattern $excludeInAbsenceSelection
  dfToExclude = data.frame(matrix(ncol = length(names(dfOBServInsectSampling)), nrow = 0))
  for (i in seq(1,length(excludeNames))) {
    #dfToExclude = rbind(dfToExclude, dfOBServInsectSampling[grepl(exclude[i], dfOBServInsectSampling$pollinator) ,])
    dfToExclude = rbind(dfToExclude, dfOBServInsectSampling[excludeNames[i] == dfOBServInsectSampling$pollinator, ])
  }
  dfInsectAbsence = anti_join(dfOBServInsectSampling, dfToExclude)
  dfInsectAbsence = dfInsectAbsence[dfInsectAbsence$abundance_species > 0,]
  
  # Select those locations (study_id, site_id) not present in dfPresence
  dfInsectAbsence = anti_join(dfInsectAbsence, dfPresence, by=c("study_id", "site_id"))
  
  # Join tables by study_id and site_id and save
  dfAbsence = merge(dfInsectAbsence, dfOBServFieldData, by=c("study_id", "site_id"))
  names(dfAbsence)[names(dfAbsence) == "longitude"] <- "lon"
  names(dfAbsence)[names(dfAbsence) == "latitude"]  <- "lat"
  dfAbsence["presence"]  = rep(0, nrow(dfAbsence))
  
  # Tag origin of the points 
  dfAbsence$source = replicate(nrow(dfAbsence),"OBServ")
  
  # Take points within the convex hull of the presence points, buffered 1 degree
  conHull   = convHull(dfPresence[c("lon","lat")])
  conHull   = buffer(polygons(conHull),width=1)
  absPoints = SpatialPoints(cbind(dfAbsence$lon, dfAbsence$lat))
  inConHull = array()
  for(i in seq(1:length(absPoints))) {
    inConHull[i] = gContains(conHull, absPoints[i])
  }
  dfAbsence = dfAbsence[inConHull,]
  
  # Remove NA and duplicated locations
  dfAbsence = dfAbsence[!is.na(dfAbsence$lat) & !is.na(dfAbsence$lon),]
  dfAbsence = dfAbsence[!duplicated(dfAbsence[c("lon","lat")]),]
  return(dfAbsence)
}

getFeatures <- function(featDir, removePatterns, coords_digits=4) {
  # Features collected in GEE
  featFiles = list.files(featDir, full.names = TRUE);
  featNames = tools::file_path_sans_ext(list.files(featDir, full.names = FALSE));
  for (i in seq(featFiles)) {
    dfFeature = read.csv(file = featFiles[i], header = TRUE)
    
    # Round coordinates to the specified level of precision
    dfFeature$longitude = round(dfFeature$longitude, digits=coords_digits)
    dfFeature$latitude  = round(dfFeature$latitude, digits=coords_digits)
    
    # Remove duplicated locations 
    dfFeature = dfFeature[!duplicated(dfFeature[c("longitude","latitude")]),]
    
    # Take feature column 
    newName=""
    for (j in seq(names(dfFeature))) {
      if(grepl(names(dfFeature)[j], featNames[i])) {
        newName = featNames[i]
        for (k in seq(removePatterns)) {
          newName = str_remove(newName, removePatterns[k])
        }
        names(dfFeature)[j] <- newName
        break
      }
    }
    if (newName == "") break
    dfFeature = dfFeature %>% select(c(newName,"longitude","latitude"))
    
    # Left join
    if (i==1) dfFeatures = dfFeature else dfFeatures = merge(dfFeatures, dfFeature, all.x = TRUE)
  }
  return(dfFeatures)
}

getHistos <- function(histoDir, removePatterns, coords_digits=4) {
  histoFiles = list.files(histoDir, full.names = TRUE);
  histoNames = tools::file_path_sans_ext(list.files(histoDir, full.names = FALSE));
  for (i in seq(histoFiles)) {
    dfHisto = read.csv(file = histoFiles[i], header = TRUE)
    
    # Round coordinates to the specified level of precision
    dfHisto$longitude = round(dfHisto$longitude, digits=coords_digits)
    dfHisto$latitude  = round(dfHisto$latitude, digits=coords_digits)
    
    # Remove duplicated locations 
    dfHisto = dfHisto[!duplicated(dfHisto[c("longitude","latitude")]),]
    
    # Take feature column 
    newName=""
    for (j in seq(names(dfHisto))) {
      if(grepl(names(dfHisto)[j], histoNames[i])) {
        newName = histoNames[i]
        for (k in seq(removePatterns)) {
          newName = str_remove(newName, removePatterns[k])
        }
        names(dfHisto)[j] <- newName
        break
      }
    }
    if (newName == "") break
    dfHisto = dfHisto %>% select(c(newName,"longitude","latitude"))
    
    # Left join
    if (i==1) dfHistos = dfHisto else dfHistos = merge(dfHistos, dfHisto, all.x = TRUE)
  }
  return(dfHistos)
}

##########################
# CORINE-RELATED FEATURES
# This function takes two arrays from the histograms calculated for CORINE LC types
# and converts it to percentage of Urban Fabric (codes 11X)
getLandcoverPercentage <- function(lcCode, types, values) {
  selValues = values[lcCode == types]
  perc      = 100 * sum(selValues) / sum(values)  
  return(perc)
}

# This function takes a string with dictionary format (e.g. {211: XX.xxx, 322: xx.xx})
# and creates a single-row dataframe with the percentages of the major CORINE LC types
getCorinePercentages <- function(x, coords_digits=4) {
  string = x["CORINE"]
  lon    = round(as.numeric(x["longitude"]), digits=coords_digits)
  lat    = round(as.numeric(x["latitude"]), digits=coords_digits)
  aux = str_split(string,"=",simplify = TRUE)
  elts=c()
  for (elt in aux) elts = c(elts, str_split(elt,",",simplify = TRUE))
  elts = str_remove(elts, "[{} ]")
  types =c()
  values=c()
  for (i in seq(elts)) {
    if ( (i %% 2) == 1) types  = c(types, elts[i])
    if ( (i %% 2) == 0) values = c(values, as.numeric(elts[i]))
  }
  types = substr(types, 1, 2) # get only the first 2 characters of the lc type
  df = data.frame(matrix(ncol = 0, nrow = 1))
  df$longitude= lon
  df$latitude = lat
  df$urban    = getLandcoverPercentage("11",types,values)
  df$indust   = getLandcoverPercentage("12",types,values)
  df$mine     = getLandcoverPercentage("13",types,values)
  df$art_veg  = getLandcoverPercentage("14",types,values)
  df$arable   = getLandcoverPercentage("21",types,values)
  df$per_crop = getLandcoverPercentage("22",types,values)
  df$pasture  = getLandcoverPercentage("23",types,values)
  df$het_crop = getLandcoverPercentage("24",types,values)
  df$forest   = getLandcoverPercentage("31",types,values)
  df$scrub    = getLandcoverPercentage("32",types,values)
  df$open     = getLandcoverPercentage("33",types,values)
  df$wet_inl  = getLandcoverPercentage("41",types,values)
  df$wet_mar  = getLandcoverPercentage("42",types,values)
  df$wat_inl  = getLandcoverPercentage("51",types,values)
  df$wat_mar  = getLandcoverPercentage("52",types,values)
  return(df)
}

removeNAandDupLocations <- function(df, lon, lat, coords_digits = 4) {
  df = df[!is.na(df$lat) & !is.na(df$lon),]
  df$round_lon = round(df$lon, digits=coords_digits)
  df$round_lat = round(df$lat, digits=coords_digits)
  df = df[!duplicated(df[c("round_lon","round_lat")]),]
  df = df %>% select(-c("round_lon","round_lat"))
  return(df)
}

decimalplaces <- function(x) {
  decimal = x - floor(x)
  nDecimal = nchar(decimal) - 2
  nDecimal <- if(nDecimal > 0) nDecimal else 0
  return(nDecimal)
}

cleanSamplingYear <- function(df, yrFrom, yrTo) {
  df$sampling_year = as.numeric(substr(df$sampling_year, 1, 4)) # get only the first 4 characters (remove entries like 2001-2002, 2001/2000, etc)
  return(df[df$sampling_year >= yrFrom & df$sampling_year <= yrTo,])
}

clean <- function(df, yrFrom=1800, yrTo=2100, minDec=3, lon="lon", lat="lat", species="pollinator", coords_digits = 4) {
  df = cleanSamplingYear(df, yrFrom, yrTo)
  df = removeNAandDupLocations(df, "lon", "lat", coords_digits)
  # Use cleanCoordinates package
  rownames(df)<-1:nrow(df)
  if (species == "") {
    df["species"] = rep("mockSpecies",nrow(df))
    cleancoord <- clean_coordinates(x = df, lon = lon, lat = lat, species = "species", 
                                    tests = c("capitals", "centroids", "equal", "gbif", "institutions", "seas", "zeros"))
    df = df %>% select(-c("species"))
  } else {
    cleancoord <- clean_coordinates(x = df, lon = lon, lat = lat, species = species, 
                                    tests = c("capitals", "centroids", "equal", "gbif", "institutions", "seas", "outliers", "zeros"))
  }
  df = df[cleancoord$.summary,]
  # Rule out coordinates that have less than 3 decimal digits
  indDecimGe3 = ( decimalplaces(df[lon]) >= minDec ) && ( decimalplaces(df[lat]) >= minDec )
  df = df[indDecimGe3,]
  return(df)
}
