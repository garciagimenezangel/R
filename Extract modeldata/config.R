######################################################

# Folders
KLAB_root = "C:/Users/angel.gimenez/Documents/DATA/OBServ/KLab Models/"
GEE_root = "C:/Users/angel.gimenez/Documents/DATA/OBServ/GEE Models/"
field_data_folder = "../../OBservData/Datasets_storage/"
output_folder = "C:/Users/angel.gimenez/Documents/DATA/OBServ/Processed modeldata/"

# Column names that will be in datasets and in the processed geodata tables
base_col_names = c("study_id", "site_id", "sampling_year", "latitude", "longitude") 

# Output file
out_file = paste(Sys.Date(), ".csv", sep = "")
out_file = paste(output_folder, out_file, sep = "")

# Other Settings
coords_digits = 3 # Precision of the coordinates when doing JOIN operations between tables

