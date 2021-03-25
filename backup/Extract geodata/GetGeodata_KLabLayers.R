#########################################################
#
# This script samples raster files ".tiff" (e.g. Elevation), at the points of the datasets in OBServ.
#
# This script must be called at "main.R"
# Input/Output variables set in "config.R"
#
# INPUT: 
# - OBServ datasets (CSV) (field data)
# - Folders where the .tiff files are stored
# OUTPUT:
# - Table CSV, merging OBServ datasets and sampled measurements
#
#########################################################

GetGeodata_KLabLayers <- function() {
  
  # Geolocalized measurements from KLab layers. For each geolocalized measurement, we need:
  # 1) A folder where the corresponding layer is stored
  # 2) A column name to add to the ouput table
  geo_folders = paste(list.dirs(KLAB_root)[-1], "/", sep="");
  geo_names = list.dirs(KLAB_root, full.names = FALSE)[-1];
  
  # Output dataframe 
  col_names = c(base_col_names, geo_names)
  df_out = data.frame(matrix(ncol = length(col_names), nrow = 0))
  names(df_out) = col_names
  
  # Loop datasets
  for (ds in datasets) {
    
    # Read dataset and define temporary dataframe, where we will store the extracted geodata 
    # at the points of this dataset 
    df = read.csv(file= paste(field_data_folder, ds, sep = ""), header=T);
    df_temp = df[base_col_names]
    
    # Coordinates to extract geodata
    coords = cbind(df_temp["longitude"], df_temp["latitude"])
    coords = coords[!is.na(coords["longitude"]) & !is.na(coords["latitude"]),]
    if (nrow(coords) == 0) next
    sp<-SpatialPoints(coords)
    
    # Loop geodata
    for (i_geo in seq(geo_folders)) {
      
      geo_data = data.frame(matrix(ncol = 1, nrow = nrow(coords)))
      names(geo_data) = geo_names[i_geo];
      folder = geo_folders[i_geo];
  
      # Loop KLab raster layers
      rasters = list.files(folder, pattern = "*.tiff$");
      for (tiff_file in rasters) {
        rast<-raster(paste(folder, tiff_file, sep=""))
        values = extract(rast, sp, method='bilinear')
        found = !is.na(values); 
        geo_data[found, 1] = values[found]
      }
      
      # Add extracted data to temporary dataframe
      df_temp = cbind(df_temp, geo_data);
    }
    
    # Add geodata associated with this dataset, into the output dataframe
    df_out = rbind(df_out, df_temp)
    
  }
  # Return df_out
  df_out
}
