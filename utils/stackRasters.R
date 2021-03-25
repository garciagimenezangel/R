rm(list=ls())
library(raster)
library(rlist)
root = "C:/Users/angel.gimenez/Documents/DATA/OBServ/SDMs/"

mergeRasters = function(x, rootDir) {
  name = x[[1]]@data@names
  print(paste("Name:", name))
  filename = paste0(rootDir, name, ".tif")
  x$filename <- filename
  x$overwrite <- TRUE
  m <- do.call(raster::merge, x)
  print(paste("File merged:",filename))
}

files = list.files(root, full.names = TRUE, pattern = ".tif", recursive = TRUE)[1:2]
for (i in seq(1,length(files))) {
  x = list(list())
  s1 = stack(files[i])
  for (j in seq(1, length(s1@layers))) {
    if (length(x) < j) {
      x[[j]] = list(flip(s1@layers[[j]],direction='y'))
    } else {
      x[[j]] = list.append(x[[j]], flip(s1@layers[[j]],direction='y'))  
    }
  }
  print(paste("File checked:",files[i]))
  i=i+1
  s2 = stack(files[i])
  for (j in seq(1, length(s1@layers))) {
    if (length(x) < j) {
      x[[j]] = list(flip(s1@layers[[j]],direction='y'))
    } else {
      x[[j]] = list.append(x[[j]], flip(s1@layers[[j]],direction='y'))  
    }
  }
  print(paste("File checked:",files[i]))
  lapply(x, mergeRasters, rootDir=root)
}




