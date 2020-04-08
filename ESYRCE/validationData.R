
# Validar los datos: comprobar que las parcelas se han muestreado más de un año, y en el mismo área todos los años, 
# para poder hacer un seguimiento de indicadores de intensificación y potencial polinizador. 

require(rgdal)

cont = 0;
for (i in seq(8,10)) {
  
  cont = cont+1;
  
  file = paste("C:/Users/angel.gimenez/Documents/DATA/OBServ/LandCover/ESYRCE/Shapefiles/Splitted/esyrce",i, sep = "")
  file = paste(file, ".shp", sep = "")
  spDf <- readOGR(dsn = file)
  
  # Bucle sobre los números de parcela
  validParcels = vector()
  parcels = unique(spDf$D2_NUM)
  for (parcel in parcels)
  {
    # Seleccionar número de parcela
    dataParcel = spDf[spDf$D2_NUM == parcel, ]
    
    # Bucle sobre los años de cada número de parcela
    years = unique(dataParcel$YEA)
    valid = length(years) > 1
    for (year in years[order(years)]) 
    {
      # Seleccionar año de muestreo
      dataParcelYear = dataParcel[dataParcel$YEA == year, ]
      
      # Comprobar que su cobertura es exactamente la misma año a año
      minYear = min(as.numeric(as.character(years)))
      if (year != minYear) valid = isTRUE(all.equal(dataParcelYear@bbox, lastBBox, tolerance=1e-6))
      lastBBox = dataParcelYear@bbox
      
      #cat("Year:", year, ";  Valid =", valid, "\n")
      
      # If !valid, parcel must be ruled out
      if (!valid) break
    }
    
    if (valid) validParcels = append(validParcels, parcel)
  }
  
  # Select valid parcels
  validSpDf_temp = spDf[is.element(spDf$D2_NUM, validParcels),]
  
  # Add valid parcels to final spatial data frame
  if(cont==1) {
    validSpDf = validSpDf_temp
  }
  else {  
    validSpDf = rbind(validSpDf, validSpDf_temp)
  }
}

writeOGR(validSpDf, dsn = "Data", layer = "esyrceVal", driver="ESRI Shapefile", overwrite_layer = TRUE)

