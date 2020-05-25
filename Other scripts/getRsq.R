library(stringr)
rm(list=ls())

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

harmonize_visit_units <- function(x, limit_rate) {
  units = x["visitation_rate_units"]
  rate  = as.numeric(x["visitation_rate"])
  if (is.na(units) | is.na(rate)) {
    rate = NA
  } else if (units ==  "visits per 100 flowers and hour") {
    rate = rate/100  
  } else if (units == "visits in 100 flowers during one hour") {
    rate = rate/100  
  } else if (units == "visits per 100 flowers and hour") {
    rate = rate/100  
  } else if (units == "(average number of) visits per 100 flowers and hour") {
    rate = rate/100  
  } else if (units == "visits per flower and second") {
    rate = rate*3600  
  } else if (units == "Insects per flower") {
    rate = NA
  } else if (units == "visits per flower per time") {
    rate = NA
  } else if (units == "visited flowers per hour") {
    rate = NA
  } else if (units == "visits to flowers per hour") {
    rate = NA
  }
  
  if (!is.na(rate) & rate > limit_rate) rate=NA
  df = data.frame(matrix(ncol = 0, nrow = 1))
  df$round_lon= as.numeric(x["round_lon"])
  df$round_lat= as.numeric(x["round_lat"])
  df$visitation_rate  = rate
  df$units = "visits per flower and hour"
  return(df)  
}

linear_reg <- function(predictor, target) {
  lm(target~predictor, data.frame(predictor, target))
}

df_field = getOBServFieldData(data_folder)
df_field$round_lon = round(df_field$longitude, digits=4)
df_field$round_lat = round(df_field$latitude,  digits=4)
df_model = read.csv("C:/Users/angel.gimenez/Documents/DATA/OBServ/GEE Models Grid/landscape_suitability_0-1500.csv", header=T)
df_model = do.call(rbind, apply(df_model, 1, getLatLon))
df_model$round_lon = round(df_model$longitude, digits=4)
df_model$round_lat = round(df_model$latitude,  digits=4)
nCombinations = 1500
nLocations = nrow(df_model) / nCombinations

# Get visits
df_visit_rate = df_field[,c("round_lon","round_lat","visitation_rate","visitation_rate_units")]
df_visit_rate = do.call(rbind, apply(df_visit_rate, 1, harmonize_visit_units, 1.0))

df_out = data.frame(matrix(nrow=nCombinations , ncol=2))
names(df_out)=c("combination",'r2.visit.rate')
for(i in seq(1,nCombinations)) {
  
  df_aux = data.frame(matrix(ncol = 3, nrow = 1))
  start = (i-1)*nLocations + 1
  end   = i*nLocations
  df_model_i = df_model[start:end,]
  
  # Visitation rate
  df        = merge(df_model_i, df_visit_rate, by=c("round_lon","round_lat"))
  df        = df[complete.cases(df),]
  target    = as.numeric(df$visitation_rate)
  predictor = as.numeric(df$landscape.suitability)
  if (length(target)>1 & length(predictor)) {
    lin_reg = linear_reg(predictor, target)
    summ = summary(lin_reg)
    df_out[i,'combination'] = i
    df_out[i,'r2.visit.rate'] = summ$r.squared
  } else {
    df_out[i,'combination'] = i
    df_out[i,'r2.visit.rate'] = -1
  }
}


