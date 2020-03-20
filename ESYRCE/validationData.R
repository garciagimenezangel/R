library(sf)
library(validate)

# Crear data frame con los datos
shapefile <- st_read("C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/Data/Land cover/ESYRCE/Shapefiles/Test_selection/result.shp")

# Lo que quiero hacer:

# Bucle sobre los números de parcela, que deben cumplir unos requisitos
parcels = unique(shapefile[["D2_NUM"]])
for (parcel in parcels)
{
  # Seleccionar número de parcela
  # parcel_data = shapefile[shapefile["D2_NUM"] == parcel,]
  
  # Todos los años tienen que tener la misma cobertura (los mismos cuadrados)
  # val = check_that(parcel_data, D2_NUM > 0)
  
  # Número de años mínimo?
  # summary(val)
}

# Tener en cuenta que la variable shapefile es una lista de listas, a las que se accede por el nombre de los atributos: shapefile[["D2_NUM"]] o shapefile[["geometry"]]