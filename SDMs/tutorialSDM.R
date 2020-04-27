# The following script follows: https://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf


###############
# loads the dismo library
library(dismo)
library(raster)

###############
# Retrieve occurrence records of Bombus Terrestris in Europe since 2018
bTerrestris = gbif("bombus", "terrestris", args=c("year=2000,2020","country=ES"), geo=TRUE)

###############
# Plot map
library(maptools)
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-10,30), ylim=c(30,70), axes=TRUE, col="light yellow")
# restore the box around the map
box()
# plot points
points(bTerrestris$lon, bTerrestris$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(bTerrestris$lon, bTerrestris$lat, col='red', cex=0.75)

###############
# Remove duplicates and NA coordinates
bTerrestris = bTerrestris[!is.na(bTerrestris$lat) & !is.na(bTerrestris$lon), ]
dups <- duplicated(bTerrestris)
which.min(dups==TRUE)
bTerrestris=bTerrestris[dups==FALSE,]

###############
# Cross-checking coordinates with country
library(sp)
bTerrGeo = bTerrestris
coordinates(bTerrGeo) <- ~lon+lat
crs(bTerrGeo) <- crs(wrld_simpl)
ovr <- over(bTerrGeo, wrld_simpl)
cntr <- ovr$NAME
j <- which(cntr != bTerrestris$country)
cbind(cntr, bTerrestris$country)[j,] 

###############
# Georeferencing (SKIP THIS): If you have records with locality descriptions but no coordinates, you should consider georeferencing these.

###############
# Sampling bias
# One can attempt to remove some of the sampling bias by subsampling records,
# and this is illustrated below. However, subsampling reduces the number of
# records, and it cannot correct the data for areas that have not been sampled at
# all. It also suffers from the problem that locally dense records might in fact be
# a true reflection of the relative suitable of habitat.

# create a RasterLayer with the extent of bTerrestris
r <- raster(bTerrGeo)
# set the resolution of the cells to (for example) 1 degree
res(r) <- 1
# expand (extend) the extent of the RasterLayer a little
r <- extend(r, extent(r)+1)
# sample:
bTerrSel <- gridSample(bTerrGeo, r, n=1)
# to illustrate the method and show the result
p <- rasterToPolygons(r)
plot(p, border='gray')
points(bTerrGeo)
# selected points in red
points(bTerrSel, cex=1, col='red', pch='x')
# Save selected data
write.csv(bTerrSel, paste(system.file(package="dismo"), '/ex/bombusTerrestris.csv', sep=''))
bTerrSel <- read.csv(file)

###############
# Absence and background points
# get the file names
Spain <- wrld_simpl[wrld_simpl$NAME=="Spain",]
plot(Spain)

# select 100 random points. Set seed to assure that the examples will always have the same random sample.
set.seed(1963)
bg=spsample(Spain, n=100, type="random")
points(bg, cex=0.5)

###############
# Environmental data
wclim0 = getData('worldclim', var='bio', res=0.5, lon=-16, lat=28)
wclim1 = getData('worldclim', var='bio', res=0.5, lon=-3.7, lat=40.41)
wclim2 = getData('worldclim', var='bio', res=0.5, lon=1, lat=40.41)
# annual mean temperature
t0 = mask(wclim0$bio1_25,Spain)
t1 = mask(wclim1$bio1_15,Spain)
t2 = mask(wclim2$bio1_16,Spain)
annMeanT = crop(x=merge(t0,t1,t2), y=Spain)
# annual precipitation
t0 = mask(wclim0$bio12_25,Spain)
t1 = mask(wclim1$bio12_15,Spain)
t2 = mask(wclim2$bio12_16,Spain)
annPrecip = crop(x=merge(t0,t1,t2), y=Spain)
# elevation
ele <- raster(paste(system.file(package="dismo"), "/ex/elevationSpain.tiff", sep=""))
# land cover
lc <- raster(paste(system.file(package="dismo"), "/ex/lcSpain.tiff", sep=""))

# Extract values
selPoints = SpatialPoints(cbind(bTerrSel$lon,bTerrSel$lat))
landcover = extract(lc, selPoints)
bio1      = extract(annPrecip, selPoints)
bio2      = extract(annMeanT, selPoints)
alt       = extract(ele, selPoints)
pb        = rep(1, length(alt))
sdmdata1  = as.data.frame(cbind(pb, landcover, bio1, bio2, alt))
predValues[,'lcSel'] = as.factor(predValues[,'lcSel']) # 'lcSel' is categorical variable (called a 'factor' in R ) and it is important to explicitly define it that way, so that it won't be treated like any other numerical variable.
landcover = extract(lc, bg)
bio1      = extract(annPrecip, bg)
bio2      = extract(annMeanT, bg)
alt       = extract(ele, bg)
pb        = rep(0, length(eleBg))
sdmdata0  = as.data.frame(cbind(pb, landcover, bio1, bio2, alt))
sdmdata   = rbind(sdmdata0, sdmdata1)
sdmdata   = sdmdata[!is.na(sdmdata$bio1) & !is.na(sdmdata$bio2) & !is.na(sdmdata$alt),]
sdmdata[,'landcover'] = as.factor(sdmdata[,'landcover']) # 'landcover' is categorical variable (called a 'factor' in R ) and it is important to explicitly define it that way, so that it won't be treated like any other numerical variable.

