# The following script follows: https://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf


###############
# loads the dismo library
library(dismo)

###############
# Retrieve occurrence records of Bombus Terrestris in Europe since 2018
bTerrestris = gbif("bombus", "terrestris", args=c("year=2018,2020", "continent=EUROPE"))

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
# Remove duplicates
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
file <- paste(system.file(package="dismo"), '/ex/bombusTerrestris.csv', sep='')
bTerrSel <- read.csv(file)

###############
# Absence and background points
# get the file names
Belgium <- wrld_simpl[wrld_simpl$NAME=="Belgium",]
plot(Belgium)

a=spsample(Belgium, n=500, type="random")
points(a, cex=0.5)

# we use the first file to create a RasterLayer
mask <- raster(Belgium)
# select 500 random points
# set seed to assure that the examples will always
# have the same random sample.
set.seed(1963)
bg <- randomPoints(mask, 500 )



