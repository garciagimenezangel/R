#########################################################
#
# This script samples output files from KLab models (for now, rasters), 
# at the points of the datasets in OBServ.
#
# Models KLab. Example of folders' structure for a model:
# Raster file calculated from the Lonsdorf model in KLab: model.tiff
# Subfolders in KLAB_root correspond to model options (e.g. exponential decay and radius 3000m)
# Then, file is found in KLAB_root/Lonsdorf/exp/r3000m/model.tiff
# Model ID will be built using that subfolders hierarchy: model_id = Lonsdorf.exp.r3000m.model
#
# This script must be called at "main.R"
# Input/Output variables set in "config.R"
#
# INPUT: 
# - OBServ datasets (CSV)
# - Root folders of the KLab-models' output files
# OUTPUT:
# - Table CSV, merging OBServ datasets and KLab models. For each model, a new column is added,
#   using the model_id as its name, and filled with the model predictions (if available for that location)
#   
#
#########################################################

GetPredictions_KLabModels <- function() {
  
  # Set (using reg expr) extensions acceptable to be considered as a model output
  valid_ext = "*.(tiff|tif)$"
  
  # Models = list of files in the KLAB_root folder that match the valid extensions defined above
  model_files = list.files(KLAB_root, full.names = FALSE, recursive = TRUE, pattern = valid_ext);
  
  # Get model ids
  model_ids = tools::file_path_sans_ext(model_files); # name of the files without extension
  model_ids = gsub("/", ".", model_ids);              # replace slashes by points
  
  # Output dataframe 
  col_names = c(base_col_names, model_ids)
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
    
    # Loop models
    for (i_model in seq(model_files)) {
      
      # Init dataframe for this model
      model_data = data.frame(matrix(ncol = 1, nrow = nrow(coords)))
      names(model_data) = model_ids[i_model];
      
      # Extract data from the model file
      rast<-raster(paste(KLAB_root, model_files[i_model], sep=""));
      model_data[,1] = extract(rast, sp, method='bilinear');

      # Add extracted data to temporary dataframe
      df_temp = cbind(df_temp, model_data);
    }
    
    # Add geodata associated with this dataset, into the output dataframe
    df_out = rbind(df_out, df_temp)
    
  }
  # Return df_out
  df_out
}
