#require(raster)

#a = CRS("+proj=longlat +datum=WGS84 +no_defs")

#crs(spDf) <- a

years = c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)

for (year in years) {
  spDf = readOGR(dsn = "C:/Users/angel.gimenez/Documents/DATA/OBServ/LandCover/ESYRCE/Shapefiles/PROJECTED/esyrceVal01.shp")
  ii = spDf@data$YEA == year
  spDf_year = spDf[ii,]
  spDf = readOGR(dsn = "C:/Users/angel.gimenez/Documents/DATA/OBServ/LandCover/ESYRCE/Shapefiles/PROJECTED/esyrceVal02-04.shp")
  ii = spDf@data$YEA == year
  spDf_year = rbind(spDf_year, spDf[ii,])
  name = paste("esyrce",year,sep="")
  name = paste(name,"_1",sep="")
  writeOGR(spDf_year, dsn = "PROJECTED", layer = name, driver="ESRI Shapefile", overwrite_layer = TRUE)
}

for (year in years) {
  spDf = readOGR(dsn = "C:/Users/angel.gimenez/Documents/DATA/OBServ/LandCover/ESYRCE/Shapefiles/PROJECTED/esyrceVal05-07.shp")
  ii = spDf@data$YEA == year
  spDf_year = spDf[ii,]
  spDf = readOGR(dsn = "C:/Users/angel.gimenez/Documents/DATA/OBServ/LandCover/ESYRCE/Shapefiles/PROJECTED/esyrceVal08-10.shp")
  ii = spDf@data$YEA == year
  spDf_year = rbind(spDf_year, spDf[ii,])
  name = paste("esyrce",year,sep="")
  name = paste(name,"_2",sep="")
  writeOGR(spDf_year, dsn = "PROJECTED", layer = name, driver="ESRI Shapefile", overwrite_layer = TRUE)
}
