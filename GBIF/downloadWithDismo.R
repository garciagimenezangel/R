
library(dismo)

dataFolder = "C:/Users/angel.gimenez/Documents/DATA/OBServ/GBIF/"

speciesEuropeKleijn2015 = c("Bombus terrestris",
                            "Bombus lapidarius",
                            "Andrena chrysosceles",
                            "Andrena flavipes",
                            "Andrena haemorrhoa",
                            "Andrena carantonica",
                            "Bombus pascuorum",
                            "Andrena fulva",
                            "Andrena dorsata",
                            "Lasioglossum calceatum",
                            "Lasioglossum malachurum",
                            "Bombus hypnorum",
                            "Osmia bicornis",
                            "Bombus pratorum",
                            "Andrena nitida",
                            "Andrena minutula",
                            "Bombus hortorum",
                            "Lasioglossum politum",
                            "Lasioglossum morio",
                            "Andrena cineraria")

# Download occurrences 
pars = c("year=1990,2020")
species = "Andrena nitida"
splitted = strsplit(species, " ")
genus = splitted[[1]][1]
sp = splitted[[1]][2]
aNitida = gbif(genus, sp, args=pars, geo=TRUE)
write.csv(aNitida, paste(dataFolder,"andrena_nitida.csv", sep=""))


###############
# Plot map
library(maptools)
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-10,30), ylim=c(30,70), axes=TRUE, col="light yellow")
# restore the box around the map
box()
# plot points
points(aNitida$lon, aNitida$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(aNitida$lon, aNitida$lat, col='red', cex=0.75)

###############
# Remove duplicates and NA coordinates
aNitida = aNitida[!is.na(aNitida$lat) & !is.na(aNitida$lon), ]
dups <- duplicated(aNitida)
which.min(dups==TRUE)
aNitida=aNitida[dups==FALSE,]

###############
# Cross-checking coordinates with country
library(sp)
aNitGeo = aNitida
coordinates(aNitGeo) <- ~lon+lat
crs(aNitGeo) <- crs(wrld_simpl)
ovr <- over(aNitGeo, wrld_simpl)
cntr <- ovr$NAME
j <- which(cntr != aNitida$country)
cbind(cntr, aNitida$country)[j,] 

###############
# Sampling bias
# One can attempt to remove some of the sampling bias by subsampling records,
# and this is illustrated below. However, subsampling reduces the number of
# records, and it cannot correct the data for areas that have not been sampled at
# all. It also suffers from the problem that locally dense records might in fact be
# a true reflection of the relative suitable of habitat.
r <- raster(aNitGeo)
# set the resolution of the cells to (for example) 1 degree
res(r) <- 1
# expand (extend) the extent of the RasterLayer a little
r <- extend(r, extent(r)+1)
# sample:
aNitSel <- gridSample(aNitGeo, r, n=1)
# to illustrate the method and show the result
p <- rasterToPolygons(r)
plot(p, border='gray')
points(aNitGeo)
# selected points in red
points(aNitSel, cex=1, col='red', pch='x')

