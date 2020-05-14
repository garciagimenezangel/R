library(sf)
dataDir = "C:/Users/angel.gimenez/Documents/DATA/"
df = data.frame()
p.sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326) 
st_write(p.sf, paste0(dataDir, "data.shp"))