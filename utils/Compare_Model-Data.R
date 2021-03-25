library(raster)
library(rgdal)

######################################################
# FILES AND DIRECTORIES
# working directory 
work_dir = "C:/Users/angel.gimenez/Documents/Projects/OBServ/R repo/Other scripts";
setwd(work_dir)

# Name of raster
raster_folder = "../../Data/Model outputs/Lonsdorf/exp/r3350m/"
raster_file = paste(raster_folder, "visit_as_pollinator_probability.tiff", sep="")

# Data file
data_file = "../../AA_GITHUB/data/field_level_data_Bartomeus2015.csv"

# Plot (output) file
plot_file = paste(raster_folder, "Bartomeus2015.png", sep="")

######################################################
# Load Data
df = read.csv(file= data_file, header=T)

# Sites' coordinates
coords = cbind(df["longitude"], df["latitude"])
coords = coords[!is.na(coords["longitude"]) & !is.na(coords["latitude"]),]
sp<-SpatialPoints(coords)

# Get values from raster
rast<-raster(raster_file)
values = extract(rast, sp, method='bilinear')
png(filename = plot_file)
par(mfrow=c(2,2))
plot(values, df[,"total_yield"], xlab="Model", ylab="Total Yield")
plot(values, df[,"abundance"], xlab="Model", ylab="Abundance")
plot(values, df[,"visitation_rate"], xlab="Model", ylab="Visit rate")
plot(values, df[,"pollinator_richness"], xlab="Model", ylab="Richness Estimator")
dev.off()

