library(stringr)
library(dplyr)
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
# df_model1 = read.csv("C:/Users/angel.gimenez/Documents/DATA/OBServ/GEE Models Grid/Grid1/landscape_suitability_0-1500.csv", header=T)
# df_model2 = read.csv("C:/Users/angel.gimenez/Documents/DATA/OBServ/GEE Models Grid/Grid1/landscape_suitability_1500-3000.csv", header=T)
# df_model3 = read.csv("C:/Users/angel.gimenez/Documents/DATA/OBServ/GEE Models Grid/Grid1/landscape_suitability_3000-4500.csv", header=T)
# df_model4 = read.csv("C:/Users/angel.gimenez/Documents/DATA/OBServ/GEE Models Grid/Grid1/landscape_suitability_4500-6000.csv", header=T)
# df_model5 = read.csv("C:/Users/angel.gimenez/Documents/DATA/OBServ/GEE Models Grid/Grid1/landscape_suitability_6000-6561.csv", header=T)
# df_model = do.call("rbind", list(df_model1, df_model2, df_model3, df_model4, df_model5))
df_model = read.csv("C:/Users/angel.gimenez/Documents/DATA/OBServ/GEE Models/Lonsdorf/gaussKer/itDefault24/landscape_suitability_10001021.csv", header=T)
df_model$round_lon = round(df_model$longitude, digits=4)
df_model$round_lat = round(df_model$latitude,  digits=4)
names(df_model)[names(df_model) == "first"] <- "landscape.suitability"
df_model = df_model %>% select(-c(".geo","system.index","latitude","longitude","sampling_year"))
nCombinations = 6561
nLocations = nrow(df_model) / nCombinations

# Get visits
df_visit_rate = df_field[,c("round_lon","round_lat","visitation_rate","visitation_rate_units")]
df_visit_rate = do.call(rbind, apply(df_visit_rate, 1, harmonize_visit_units, 1.0))

df_out = data.frame(matrix(nrow=nCombinations , ncol=3))
names(df_out)=c("combination",'r2.visit.rate','slope.visit.rate')
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
  if (length(target)>1 & length(predictor)>1) {
    lin_reg = linear_reg(predictor, target)
    summ = summary(lin_reg)
    df_out[i,'combination'] = i
    df_out[i,'r2.visit.rate'] = summ$r.squared
    df_out[i,'slope.visit.rate'] = summ$coefficients[2,1]
  } else {
    df_out[i,'combination'] = i
    df_out[i,'r2.visit.rate'] = -1
    df_out[i,'slope.visit.rate'] = summ$coefficients[2,1]
  }
}
slopePos = df_out[df_out$slope.visit.rate > 0,]
bestR2 = slopePos[slopePos$r2.visit.rate == max(slopePos$r2.visit.rate),]

