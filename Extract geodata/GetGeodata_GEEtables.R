#########################################################
#
# This script links the CSVs generated in Google Earth Engine, which are point samples 
# of datasets available in the Google Earth Engine's Catalog (e.g. Wind Speed), 
# at the points of the datasets of OBServ.
#
# This script must be called at "main.R"
# Input/Output variables set in "config.R"
#
# INPUT: 
# - OBServ datasets (CSV) (field data)
# - Measures from Google Earth Engine (CSV point samples)
# - Coordinates' precision in the merge process (rows with coordinates equal within such precision,
#   will be considered equal in the merge process)
# OUTPUT:
# - Tables CSV, merging OBServ dataset and measurements
# 
#########################################################

GetGeodata_GEEtables <- function() {
    
  # Geolocalized measurements from GEE tables. For each geolocalized measurement, we need:
  # 1) A file with the corresponding geolocalized measurement
  # 2) A column name to add to the ouput table
  geo_files = list.files(GEE_root, full.names = TRUE);
  geo_names = list.files(GEE_root, full.names = FALSE);
  geo_names = tools::file_path_sans_ext(geo_names);
  
  # Output dataframe 
  col_names = c(base_col_names, geo_names)
  df_out = data.frame(matrix(ncol = length(col_names), nrow = 0))
  names(df_out) = col_names
  
  for (ds in datasets) {
  
    # Read dataset and define temporary dataframe, where we will store the extracted geodata 
    # at the points of this dataset 
    df = read.csv(file= paste(field_data_folder, ds, sep = ""), header=T);
    df_temp = df[base_col_names]
    
    # Round coordinates to the specified level of precision (used to merge tables)
    df_temp$round_lon = round(df_temp$longitude, digits=coords_digits)
    df_temp$round_lat = round(df_temp$latitude,  digits=coords_digits)
    
    # Loop geodata
    for (i_geo in seq(geo_names)) {
      # Read measurements data from csv file
      geo_data = read.csv(file = geo_files[i_geo], header=T)
      
      # Round coordinates to the specified level of precision
      geo_data$round_lon = round(geo_data$longitude, digits=coords_digits)
      geo_data$round_lat = round(geo_data$latitude, digits=coords_digits)
      
      # Drop not useful columns (inherited from the export in GEE)
      geo_data = geo_data %>% select(-contains("latitude"))  # we use round_lat instead of latitude for merging
      geo_data = geo_data %>% select(-contains("longitude")) # we use round_lon instead of longitude for merging
      geo_data = geo_data %>% select(-c("system.index", ".geo"))
  
      # There might be duplicated values in the tables from GEE, because some field points are very near and have the same coordinates. Remove those rows:
      geo_data = geo_data[!duplicated(geo_data),]
      
      # LEFT JOIN tables
      df_temp = merge(df_temp, geo_data, all.x=TRUE, by=c("round_lon","round_lat"))
      
      # Rename added column (the last one) in df_temp with geo_name
      names(df_temp)[length(names(df_temp))] = geo_names[i_geo]
    }
    
    # Drop rounded coordinates, sort and export table
    df_temp = df_temp %>% select(-c("round_lon"))
    df_temp = df_temp %>% select(-c("round_lat"))
    
    # Add geodata associated with this dataset, into the output dataframe
    df_out = rbind(df_out, df_temp)
  
  }
  # Return df_out
  df_out
}

