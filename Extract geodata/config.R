######################################################

# Datasets
ds_names = c("field_level_data_Bart01",
             "field_level_data_Gari01",
             "field_level_data_Hevi01",
             "field_level_data_Knap01")
datasets = paste(ds_names, ".csv", sep="")

# Folders
KLAB_root = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/Data/KLab Layers/"
GEE_root = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/Data/GEE Layers/"
field_data_folder = "../../OBServ/data/"
output_folder = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/Data/Processed geodata/"

# Column names that will be in datasets and in the processed geodata tables
base_col_names = c("study_id", "site_id", "sampling_year", "latitude", "longitude") 

# Output file
out_file = paste(Sys.Date(), ".csv", sep = "")
out_file = paste(output_folder, out_file, sep = "")

# Other Settings
coords_digits = 3 # Precision of the coordinates when doing JOIN operations between tables
