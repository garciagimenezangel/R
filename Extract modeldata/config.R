######################################################

# Folders
Rasters_root = "C:/Users/angel/DATA/Output Models/Rasters/"
GEE_root = "C:/Users/angel/DATA/Output Models/GEE/"
field_data_folder = "C:/Users/angel/git/OBservData/Final_Data/"
output_folder = "C:/Users/angel/DATA/Processed modeldata/"

# Column names that will be in datasets and in the processed geodata tables
base_col_names = c("study_id", "site_id", "sampling_year", "latitude", "longitude") 

# Output file
out_file = paste(Sys.Date(), ".csv", sep = "")
out_file = paste(output_folder, out_file, sep = "")

# Other Settings
coords_digits = 3 # Precision of the coordinates when doing JOIN operations between tables

