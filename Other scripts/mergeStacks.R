library(raster)
library(rlist)
root = "C:/Users/angel.gimenez/Documents/DATA/OBServ/SDMs/rasters/stacksToMerge/"
subdirs = list.dirs(root, full.names = TRUE)

mergeRasters = function(x, rootDir) {
  name = x[[1]]@data@names
  filename = paste0(rootDir, name, ".tif")
  x$filename <- filename
  x$overwrite <- TRUE
  m <- do.call(raster::merge, x)
  print(paste("File merged:",filename))
}

# One by one:
i=3
dir = subdirs[i]
files = list.files(dir, full.names = TRUE, pattern = ".tif")
x = list(list())
for (i in seq(1,length(files))) {
  s = stack(files[i])
  for (j in seq(1, length(s@layers))) {
    if (length(x) < j) {
      x[[j]] = list(s@layers[[j]])
    } else {
      x[[j]] = list.append(x[[j]], s@layers[[j]])  
    }
  }
  print(paste("File checked:",files[i]))
}
lapply(x, mergeRasters, rootDir=root)


