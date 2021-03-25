#########################################################
#
# This script samples output files from GEE models (tables), 
# at the points of the datasets in OBServ.
#
# Models GEE. Example of folders' structure for a model:
# CSV file calculated from the Lonsdorf model in GEE: model.csv
# Subfolders in GEE_root correspond to model options (e.g. gaussian kernel and maximum distance flight 3000m)
# Then, file is found in KLAB_root/Lonsdorf/gaussKernel/distMax3350m/model.csv
# Model ID will be built using that subfolders hierarchy: model_id = Lonsdorf.gaussKernel.distMax3350m.model
#
# This script must be called at "main.R"
# Input/Output variables set in "config.R"
#
# INPUT: 
# - OBServ datasets (CSV) (field data)
# - Root folders of the GEE-models' output files
# OUTPUT:
# - Table CSV, merging OBServ datasets and GEE models. For each model, a new column is added,
#   using the model_id as its name, and filled with the model predictions (if available for that location)
#   
#
#########################################################

GetValues_GEEModels <- function() {
  
  # Get datasets
  observFiles = list.files(field_data_folder, full.names = TRUE, pattern = "\\.csv$", recursive = FALSE)
  indFieldData = lapply(observFiles, function(x){grepl("field_level_data",x)})
  indFieldData = unlist(indFieldData)
  fsFieldData  = observFiles[indFieldData]
  
  # Set criteria (using reg expr) acceptable to be considered as a model output
  ext_valid   = ".(csv)$"

  # Models = list of files in the GEE_root folde
  model_files = list.files(GEE_root, full.names = FALSE, recursive = TRUE);
  
  # Filter according to defined criteria
  accept = str_detect(model_files, ext_valid);
  model_files = model_files[accept];
  
  # Get model ids
  model_ids = tools::file_path_sans_ext(model_files);
  model_ids = gsub("/", ".", model_ids);         # replace slashes by points
  
  # Output dataframe 
  col_names = c(base_col_names, model_ids)
  df_out = data.frame(matrix(ncol = length(col_names), nrow = 0))
  names(df_out) = col_names
  
  # Loop datasets
  for (fFieldData in fsFieldData) {
    
    # Read dataset and define temporary dataframe, where we will store the extracted model data 
    # at the points of this dataset 
    df = read.csv(file= fFieldData, header=T);
    df_temp = df[base_col_names]
    
    # Round coordinates to the specified level of precision (used to merge tables)
    df_temp$round_lon = round(df_temp$longitude, digits=coords_digits)
    df_temp$round_lat = round(df_temp$latitude,  digits=coords_digits)
    
    # Loop models
    for (i_model in seq(model_files)) {
        
      # Get model file and id
      model_id   = model_ids[i_model];
      model_file = model_files[i_model];
      
      # Read csv file
      model_data = read.csv(file = paste(GEE_root, model_file, sep=""), header=T)
      
      # Round coordinates to the specified level of precision
      model_data$round_lon = round(model_data$longitude, digits=coords_digits)
      model_data$round_lat = round(model_data$latitude, digits=coords_digits)
      
      # Drop not useful columns (inherited from the export in GEE)
      model_data = model_data %>% dplyr::select(-contains("latitude"))  # we use round_lat instead of latitude for merging
      model_data = model_data %>% dplyr::select(-contains("longitude")) # we use round_lon instead of longitude for merging
      if (("system.index" %in% colnames(model_data)) & (".geo" %in% colnames(model_data))){
        model_data = model_data %>% dplyr::select(-c("system.index", ".geo"))
      }
      
      # There might be duplicated values in the tables from GEE, because some field points are very near and have the same coordintes. Remove those rows:
      model_data = model_data[!duplicated(model_data),]
      
      # LEFT JOIN tables
      df_temp = merge(df_temp, model_data, all.x=TRUE, by=c("round_lon","round_lat","sampling_year"))
      
      # Rename added column (the last one) in df_temp with model_id
      names(df_temp)[length(names(df_temp))] = model_id;
    }
    
    # Drop rounded coordinates, sort and export table
    df_temp = df_temp %>% dplyr::select(-c("round_lon"))
    df_temp = df_temp %>% dplyr::select(-c("round_lat"))
    
    # Add geodata associated with this dataset, into the output dataframe
    df_out = rbind(df_out, df_temp)
    
  }
  # Return df_out
  df_out
}
