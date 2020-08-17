library(dplyr)
###########
setwd("C:/Users/angel.gimenez/Documents/REPOSITORIES/R/ESYRCE/")

# Read dataset
dataFile = "../../OBServ/ESYRCE_processed/data_flag012.csv"
df_data  = read.csv(dataFile, header=T)

# Organize categories
source("./categories.R")

####################################
# Function CALCULATE SLOPE
# INPUT: data from a group by operation. A variable 'columns' must be previously defined too
# OUTPUT: slope of a linear regression using the aggregated values of the columns at every year with data
####################################
calculateSlope <- function(data) {
  xaxis = data$YEA # xaxis: years
  yaxis = rowSums(data[,columns]) # yaxis: sum of the columns selected for aggregation (defined before the call to the function)
  valid = !is.na(xaxis) & !is.na(yaxis) # discard NA values
  xaxis = xaxis[valid]
  yaxis = yaxis[valid]
  slope = NA
  if (length(xaxis) > 2) { # calculate slope only when we have at least 3 points (with 2 points, the slope might be misleading)
    lmMod <- lm(yaxis ~ xaxis, data=data.frame(xaxis = xaxis, yaxis=yaxis))
    summ  = summary(lmMod)
    coeff = summ$coefficients
    slope = coeff[2]
  } 
  return(slope)
}

###############################
# Calculate slopes by category
###############################
# The function calculateSlope will aggregate those columns stored in the variable "columns" before the call to the function

# Seminatural area
columns = seminatural
df_slope_seminatural = df_data %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(slope=calculateSlope(.)))

# Field size
columns = fieldSize
df_slope_fieldSize = df_data %>% group_by(D1_HUS, D2_NUM) %>% do(data.frame(slope=calculateSlope(.)))

