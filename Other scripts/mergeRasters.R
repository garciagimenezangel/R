library(raster)
library(rlist)
root = "C:/Users/angel.gimenez/Documents/DATA/OBServ/SDMs/rasters/toMerge/"
#root = "C:/Users/angel.gimenez/Documents/DATA/OBServ/SDMs/rasters/Topography/elevation/"
subdirs = list.dirs(root, full.names = TRUE)
for (dir in subdirs[2:16]) {
  files = list.files(dir, full.names = TRUE, pattern = ".tif")
  x = list()
  for (i in seq(1,length(files))) {
    r = raster(files[i])
    #r = flip(r, direction="y")
    x = list.append(x, r)
    print(paste("File checked:",files[i]))
  }
  filename = paste0(dir,".tif")
  x$filename <- filename
  x$overwrite <- TRUE
  m <- do.call(raster::merge, x)
  print(paste("Files merged in dir:",dir))
}

# One by one:
i=2
dir = subdirs[i]
files = list.files(dir, full.names = TRUE, pattern = ".tif")
x = list()
for (i in seq(1,length(files))) {
  r = raster(files[i])
  #r = flip(r, direction="y")
  x = list.append(x, r)
  print(paste("File checked:",files[i]))
}
filename = paste0(dir,".tif")
x$filename <- filename
x$overwrite <- TRUE
m <- do.call(raster::merge, x)
print(paste("Files merged in dir:",dir))
