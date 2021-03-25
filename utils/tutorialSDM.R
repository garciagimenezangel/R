# The following script follows: https://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf


###############
# loads the dismo library
library(dismo)
library(raster)
library(sp)

file <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
bradypus <- read.table(file, header=TRUE, sep=",")
data(acaule)


###############
# Plot map
library(maptools)
data(wrld_simpl)
acgeo <- subset(acaule, !is.na(lon) & !is.na(lat))
# Europe: plot(wrld_simpl, xlim=c(-10,30), ylim=c(30,70), axes=TRUE, col="light yellow")
plot(wrld_simpl, xlim=c(-80,70), ylim=c(-60,10), axes=TRUE, col="light yellow")
# restore the box around the map
box()
# plot points
points(acgeo$lon, acgeo$lat, col='orange', pch=20, cex=0.75)
# plot points again to add a border, for better visibility
points(acgeo$lon, acgeo$lat, col='red', cex=0.75)


###############
# Environmental data
files <- list.files(path=paste(system.file(package="dismo"),'/ex', sep=''), pattern='grd', full.names=TRUE )
predictors <- stack(files)

bradypus = bradypus[2:3]
presvals <- extract(predictors, bradypus)
set.seed(0)
backgr <- randomPoints(predictors,500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata[,'biome'] = as.factor(sdmdata[,'biome'])
head(sdmdata)


################
# Model fitting
m1 <- glm(pb ~ bio1 + bio5 + bio12, data=sdmdata)
bc <- bioclim(presvals[,c('bio1', 'bio5', 'bio12')])


################
# Model prediction
p <- predict(predictors, m1)
plot(p)
