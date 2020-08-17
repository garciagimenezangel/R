
# setwd("C:/Users/angel.gimenez/Documents/DATA/OBServ/CPF/")
# df = read.csv(file = "Inputs/GEE/nestingBaleares.csv", header = TRUE)
# n=101
# m <- matrix(df$mean, nrow = 101)
# mdf=data.frame(m)
# write.csv(mdf, file = "Inputs/tableNesting.csv", row.names = FALSE)

library(raster)
df = read.csv(file = "Outputs/PVDmap_Amparo.csv", header = TRUE)
cellsize = 750
initCoord = c(460000, 4340000)
endCoord = c(535000, 4415000)
dfr = data.frame(lon=numeric(0), lat=numeric(0), value=numeric(0))
indLon = 1
for (lon in seq(from = initCoord[1], to = endCoord[1], cellsize)) {
  indLat = 1
  for (lat in seq(from = initCoord[2], to = endCoord[2], cellsize)) { 
    dfr = rbind(dfr, data.frame(lon=lon, lat=lat, value=df[indLat, indLon]))
    indLat = indLat+1
  }
  indLon = indLon+1
}

r23031 <- rasterFromXYZ(dfr)
crs(r23031) <- CRS('+init=EPSG:23031')
r4326 <- projectRaster(rOutput, crs=CRS('+init=EPSG:4326'))
writeRaster(r4326, filename="Outputs/PVDmap_Amparo.tiff", format="GTiff", overwrite=TRUE)
