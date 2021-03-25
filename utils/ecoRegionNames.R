
library(sf)

fileShp = "C:/Users/angel/DATA/RESOLVE/Ecoregions2017.shp"
polys <- st_read(fileShp)

getEcoNameFromID = function(ecoID) {
  return(polys$ECO_NAME[polys$ECO_ID == ecoID])
}

getBiomeFromID = function(biomeNum) {
  return(unique(polys$BIOME_NAME[polys$BIOME_NUM == biomeNum]))
}

# Examples:
getBiomeFromID(3)
getEcoNameFromID(118)