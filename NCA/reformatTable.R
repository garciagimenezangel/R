
dataFolder = "C:/Users/angel/DATA/NCA/India/"
dataFile   = "Indian_Crop_Production_Statistics.csv"
df_data = read.csv(paste0(dataFolder,dataFile), header=T)

# Format columns
df_aux = df_data %>% select(-c(State,District,Year))    # Select columns to format
colNumeric = colnames(df_aux)
apply(df_data[ , colNumeric], 2, function(x) {as.numeric(x)})