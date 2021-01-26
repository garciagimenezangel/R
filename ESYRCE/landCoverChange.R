# library(dplyr)
# 
# # Objetivo: sacar una tabla de probabilidades de cada posible transición entre clases. 
# # Método 1: usar landcover extraído en 9 puntos de control (lccp1, lccp2...) de cada segmento, y recoger todas las transiciones existentes de año a año
# # Método 2: de un año a otro, extraer pérdidas y ganancias de cada landcover, y distribuirlas equitativamente. Es decir, si por ejemplo maíz supone un 10% de todas las ganancias de cobertura ese año, supondríamos que el 10% de las pérdida de cada landcover que pierde cobertura, ha ido a parar a cobertura de maíz.
# 
# setwd("C:/Users/angel/git/R/ESYRCE/")
# source("./categories.R")
# dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
# dataFile     = paste0(dataFolder, "geo_metrics_climate_20-12-18.csv")
# df_data      = read.csv(dataFile, header=T)

##############
# METHOD 1
##############

##############
# METHOD 2
##############
df_LCtransitions = data.frame(matrix(0, ncol = length(landcovertypes), nrow = length(landcovertypes)))
rownames(df_LCtransitions) = landcovertypes
colnames(df_LCtransitions) = landcovertypes
df_LCtransitionsOld = df_LCtransitions

df_LCprop = df_data[, c("D1_HUS", "D2_NUM", "YEA", "segAreaNoWater", prop_landcovertypes)]
df_LCarea = df_LCprop[, prop_landcovertypes] * df_LCprop$segAreaNoWater
colnames(df_LCarea) = landcovertypes

husOld     = df_LCprop[1, "D1_HUS"]
numOld     = df_LCprop[1, "D2_NUM"]
segAreaOld = df_LCprop[1, "segAreaNoWater"]
for (i in 2:nrow(df_LCprop)) {
  husNew     = df_LCprop[i, "D1_HUS"]
  numNew     = df_LCprop[i, "D2_NUM"]  
  segAreaNew = df_LCprop[i, "segAreaNoWater"]
  if (husNew == husOld & numNew == numOld & abs(segAreaNew-segAreaOld) < 1e-3) { # if area changes (rare), we skip the row
    
    # Use this row and the previous to obtain gains and loses in area
    changeVector = df_LCarea[i,landcovertypes] - df_LCarea[i-1,landcovertypes]
    changeVector[abs(changeVector) < 1e-3] = 0 # remove negligible changes
    #Sanity check:
    if (abs(sum(changeVector)) > 1e-2) {
      print(paste("Warning: gains not equal to loses in row:",i))
      print(paste("Difference:",sum(changeVector)))
    }
    
    # LC types according to change in extension
    noChange = landcovertypes[df_LCarea[i-1,landcovertypes]>1e-3 & changeVector==0]  # there was some area of the lc type and no change
    winners  = landcovertypes[changeVector>0]
    losers   = landcovertypes[changeVector<0]
    
    # Set win/loss matrices
    win  = matrix(changeVector[changeVector>0]) / sum(changeVector[changeVector>0])
    loss = matrix(changeVector[changeVector<0]) 
    rownames(win)  = winners
    rownames(loss) = losers
    
    for (lcKept in c(noChange,winners)) {
      df_LCtransitions[lcKept,lcKept] = df_LCtransitionsOld[lcKept,lcKept] + df_LCarea[i-1,lcKept] # add all the area that was already there, because no change or increased, so we assume all has been kept
    }
    for (lcLos in losers) {
      df_LCtransitions[lcLos,lcLos] = df_LCtransitionsOld[lcLos,lcLos] + df_LCarea[i,lcLos] # add the area that remains
      # Find the total share of the loss for each winner
      totalLoss = -changeVector[,lcLos]
      for (lcWin in winners) {
        df_LCtransitions[lcLos,lcWin] = df_LCtransitionsOld[lcLos,lcWin] + totalLoss*win[lcWin,]
      }
    }
    # Sanity check
    for (loser in losers){
      testSums = rowSums(df_LCtransitions[loser,] - df_LCtransitionsOld[loser,])
      if( all(abs(df_LCarea[i-1,loser] - testSums) > 1e-3) ) print(paste("Warning: area lost not well distributed among winners:",i))
    }
  }
  else {
    # do nothing, a new segment started
  }
  husOld     = husNew
  numOld     = numNew
  segAreaOld = segAreaNew
  df_LCtransitionsOld = df_LCtransitions
}

df_LCtransitions_norm = df_LCtransitions*0
for (lc in landcovertypes) {
  if (rowSums(df_LCtransitions[lc,]) > 0) df_LCtransitions_norm[lc, ] = df_LCtransitions[lc, ] /  rowSums(df_LCtransitions[lc,])
}
 