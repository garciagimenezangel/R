library(raster)
library(rgdal)

######################################################
# FILES AND DIRECTORIES
# working directory 
work_dir = "C:/Users/angel.gimenez/Documents/Projects/OBServ/R/";
setwd(work_dir)

# Name of raster
raster_name = "Poll_serv_typDist150m_rad600m"

# Data file
data_file = "../AA_GITHUB/data/field_level_data_Bartomeus2015.csv"

# Raster file 
raster_file = paste("../Model outputs/Lonsdorf/", raster_name, sep="")
raster_file = paste(raster_file, ".tiff", sep="")

# Plot (output) file
plot_file = paste("../AA_GITHUB/Angel1stExplor/BartomeusData/Lonsdorf predictions/", raster_name, sep="")
plot_file = paste(plot_file, ".png", sep="")

######################################################
# CALCULATION
# Load Data
Bdata<- read.csv(file= data_file, header=T)

# Sites' coordinates
sps = cbind(Bdata[,8], Bdata[,7])	
sp<-SpatialPoints( rbind(sps) )

# Get values from raster
rast<-raster(raster_file)
values = extract(rast, sp, method='bilinear')
png(filename = plot_file)
par(mfrow=c(2,2))
plot(values, Bdata[,12], xlab="Model", ylab="Total Yield")
plot(values, Bdata[,19], xlab="Model", ylab="Abundance")
plot(values, Bdata[,22], xlab="Model", ylab="Wildbees")
plot(values, Bdata[,17], xlab="Model", ylab="Richness Estimator")
dev.off()

