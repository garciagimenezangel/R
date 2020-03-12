
require(rgdal)

# The input file geodatabase
setwd("C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/Data/Land cover/ESYRCE")
fgdb<-"Esyrce2001_2016.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="z30") # z28 ó z30

# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
plot(fc)
