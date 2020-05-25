library(stringr)

setwd("C:/Users/angel.gimenez/Documents/REPOSITORIES/R/Other scripts/")
data_folder = "../../OBservData/Datasets_storage/"
getOBServFieldData <- function(observDir){
  observFiles = list.files(observDir, full.names = TRUE, pattern = "\\.csv$", recursive = FALSE)
  indFieldData = lapply(observFiles, function(x){grepl("field_level_data",x)})
  indFieldData = unlist(indFieldData)
  fsFieldData  = observFiles[indFieldData]
  dfFieldData  = do.call(rbind, lapply(fsFieldData, function(file){ read.csv(file = file, header = TRUE) }))
  return(dfFieldData)
}

# This function takes a string with dictionary format (e.g. {211: XX.xxx, 322: xx.xx})
# and creates a single-row dataframe with the percentages of the major CORINE LC types
getLatLon <- function(x, coords_digits=4) {
  string = x[".geo"]
  aux = str_split(string,"coordinates",simplify = TRUE)
  aux = aux[length(aux)]
  aux = str_split(aux,":",simplify = TRUE)
  aux = aux[length(aux)]
  aux = str_remove(aux, "[{} ]")
  aux = str_split(aux,",",simplify = TRUE)
  aux = gsub("\\[|\\]", "", aux)
  df = data.frame(matrix(ncol = 0, nrow = 1))
  df$longitude= as.numeric(aux[1])
  df$latitude = as.numeric(aux[2])
  df$landscape.suitability = x["first"]
  return(df)
}

df_field = getOBServFieldData(data_folder)
df_field$round_lon = round(df_field$longitude, digits=4)
df_field$round_lat = round(df_field$latitude,  digits=4)
df_model = read.csv("C:/Users/angel.gimenez/Documents/DATA/OBServ/GEE Models Grid/landscape_suitability.csv", header=T)
df_model = do.call(rbind, apply(df_model, 1, getLatLon))
df_model$round_lon = round(df_model$longitude, digits=4)
df_model$round_lat = round(df_model$latitude,  digits=4)
nCombinations = 81
nLocations = nrow(df_model) / nCombinations

# Get visits
df_visit_rate = df_field[,c("round_lon","round_lat","visitation_rate")]
df_visit_rate = df_visit_rate[complete.cases(df_visit_rate),]
# remove outliers
Q <- quantile(df_visit_rate$visitation_rate, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(df_visit_rate$visitation_rate, na.rm = TRUE)
ind_valid = df_visit_rate$visitation_rate > (Q[1] - 1.5*iqr) & df_visit_rate$visitation_rate < (Q[2]+1.5*iqr)
df_visit_rate = df_visit_rate[ind_valid,]

# Get richness
df_richness = df_field[,c("round_lon","round_lat","other_pollinator_richness")]
df_richness = df_richness[complete.cases(df_richness),]
# remove outliers
Q <- quantile(df_richness$other_pollinator_richness, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(df_richness$other_pollinator_richness, na.rm = TRUE)
ind_valid = df_richness$other_pollinator_richness > (Q[1] - 1.5*iqr) & df_richness$other_pollinator_richness < (Q[2]+1.5*iqr)
df_richness = df_richness[ind_valid,]

df_out = data.frame(matrix(nrow=nCombinations , ncol=3))
names(df_out)=c("combination",'r2.visit.rate','r2.richness')
for(i in seq(1,nCombinations)) {
  
  df_aux = data.frame(matrix(ncol = 3, nrow = 1))
  start = (i-1)*nLocations + 1
  end   = i*nLocations
  df_model_i = df_model[start:end,]
  
  # Visitation rate
  df = merge(df_model_i, df_visit_rate, by=c("round_lon","round_lat"))
  target = df$visitation_rate
  predictor = df$landscape.suitability
  lin_reg = lm(target~predictor, data.frame(predictor, target))
  summ = summary(lin_reg)
  df_out[i,'combination'] = i
  df_out[i,'r2.visit.rate'] = summ$r.squared
  
  # Richness
  df = merge(df_model_i, df_richness, by=c("round_lon","round_lat"))
  target = df$other_pollinator_richness
  predictor = df$landscape.suitability
  lin_reg = lm(target~predictor, data.frame(predictor, target))
  summ = summary(lin_reg)
  df_out[i,'r2.richness'] = summ$r.squared
}


