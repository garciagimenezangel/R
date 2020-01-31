######################################################

# Datasets
ds_names = c("field_level_data_Bartomeus2015",
             "field_level_data_Garibaldi2016",
             "field_level_data_Fijen2016")
datasets = paste(ds_names, ".csv", sep="")

# Source of predictions
KLAB_models = TRUE

# Folders
KLAB_root = "../../Data/KLab Models/"
field_data_folder = "../../AA_GITHUB/data/"
output_folder = "../../Data/Processed predictions/"

# Column names that will be in datasets and in the processed geodata tables
base_col_names = c("study_id", "field_id", "latitude", "longitude") 

# Output file
out_file = paste(Sys.Date(), ".csv", sep = "")
out_file = paste(output_folder, out_file, sep = "")

# Other Settings
coords_digits = 3 # Precision of the coordinates when doing JOIN operations between tables
