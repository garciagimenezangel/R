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
# - OBServ datasets (CSV) (field data)
# - Root folders of the KLab-models' output files
# OUTPUT:
# - Table CSV, merging OBServ datasets and KLab models. For each model, a new column is added,
#   using the model_id as its name, and filled with the model predictions (if available for that location)
#   
#
#########################################################

GetValues_KLabModels <- function() {
  
  # Set criteria (using reg expr) acceptable to be considered as a model output
  ext_valid   = ".(tiff|tif)$"
  reg_defined = ".region"
  
  # Models = list of files in the KLAB_root folder that match the valid extensions defined above
  model_files = list.files(KLAB_root, full.names = FALSE, recursive = TRUE);
  
  # Filter according to defined criteria
  accept = str_detect(model_files, ext_valid) & str_detect(model_files, reg_defined);
  model_files = model_files[accept];
  
  # Get model ids
  dict_models <- list();
  model_ids = sub(".region.*", '', model_files); # name of the files before ".region (used as ID of the model)
  model_ids = gsub("/", ".", model_ids);         # replace slashes by points
  
  # Store relation model_id and model_file in a dictionary
  for (i in seq(model_files)) { 
    model_id = model_ids[i];
    already_associated = dict_models[[model_id]];                          # model files already associated to that id in the dictionary
    dict_models[[model_ids[i]]] = c(already_associated, model_files[i]);   # add this model file
  }
  model_ids = unique(model_ids);

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
    for (model_id in model_ids) {
      
      # Init dataframe for this model
      model_data = data.frame(matrix(ncol = 1, nrow = nrow(coords)))
      names(model_data) = model_id;
      
      # Loop files associated to the model
      assoc_files = dict_models[[model_id]];
      for (assoc_file in assoc_files) {
        rast<-raster(paste(KLAB_root, assoc_file, sep=""));     # Extract data from the model file
        values = extract(rast, sp, method='bilinear');
        found  = !is.na(values); 
        model_data[found, 1] = values[found]
      }

      # Add extracted data to temporary dataframe
      df_temp = cbind(df_temp, model_data);
    }
    
    # Add geodata associated with this dataset, into the output dataframe
    df_out = rbind(df_out, df_temp)
    
  }
  # Return df_out
  df_out
}
