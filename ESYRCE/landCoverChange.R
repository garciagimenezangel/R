library(dplyr)
library(rlist)
library(ggplot2)
library(reshape2)

# Objetivo: sacar una tabla de probabilidades de cada posible transición entre clases.
# Método 1: usar landcover extraído en 9 puntos de control (lccp1, lccp2...) de cada segmento, y recoger todas las transiciones existentes de año a año
# Método 2: de un año a otro, extraer pérdidas y ganancias de cada landcover, y distribuirlas equitativamente. Es decir, si por ejemplo maíz supone un 10% de todas las ganancias de cobertura ese año, supondríamos que el 10% de las pérdida de cada landcover que pierde cobertura, ha ido a parar a cobertura de maíz.

setwd("C:/Users/angel/git/R/ESYRCE/")
# setwd("C:/Users/angel.gimenez/git/R/ESYRCE/")
source("./categories.R")
dataFolder = "G:/My Drive/PROJECTS/OBSERV/ESYRCE/"
# dataFolder = "C:/Users/angel.gimenez/Google Drive/PROJECTS/OBSERV/ESYRCE/"
dataFile     = paste0(dataFolder, "geo_metrics_climate_20-12-18.csv")
dataFile     = paste0(dataFolder, "landCoverChange/landCoverTransitions.csv")
df_data      = read.csv(dataFile, header=T)

##############
# METHOD 1
##############
# Método 1: usar landcover extraído en 9 puntos de control (lccp1, lccp2...) de cada segmento, y recoger todas las transiciones existentes de año a año
lccp_landcovertypes = c(landcovertypes, "water", "other") # water also captured at control points, and 'other' to account for unidentified cover types
df_LCtransitions = data.frame(matrix(0, ncol = length(lccp_landcovertypes), nrow = length(lccp_landcovertypes)))
rownames(df_LCtransitions) = lccp_landcovertypes
colnames(df_LCtransitions) = lccp_landcovertypes
lccpNames = c("lccp1", "lccp2", "lccp3", "lccp4", "lccp5", "lccp6", "lccp7", "lccp8", "lccp9")
df_LCCP  = df_data[, c("D1_HUS", "D2_NUM", "YEA", lccpNames)]
husOld   = ""
numOld   = ""
lccpOldValues = data.frame(matrix("", ncol = 9, nrow = 1))
lccpNewValues = data.frame(matrix("", ncol = 9, nrow = 1))
colnames(lccpOldValues) = lccpNames
colnames(lccpNewValues) = lccpNames
for (i in 1:nrow(df_LCCP)) {
  husNew     = df_LCCP[i, "D1_HUS"]
  numNew     = df_LCCP[i, "D2_NUM"]
  isNewSegment = (husNew != husOld | numNew != numOld)
  for (j in 1:9) {
    # get lccp's 
    if (df_LCCP[i,lccpNames[j]] %in% lccp_landcovertypes) { # if new lccp valid
      
      lccpNewValues[1, lccpNames[j]] = df_LCCP[i, lccpNames[j]] # get new lccp
      
      if (!isNewSegment & (lccpOldValues[1,lccpNames[j]] %in% lccp_landcovertypes) ) { # if not new segment and old lccp valid, save transition
        lccpOld = lccpOldValues[1,lccpNames[j]]
        lccpNew = lccpNewValues[1,lccpNames[j]]                    
        df_LCtransitions[lccpOld, lccpNew] = df_LCtransitions[lccpOld, lccpNew] + 1
      }
      
      lccpOldValues[1,lccpNames[j]] = lccpNewValues[1, lccpNames[j]] # update old lccp
    }
    else {
      # lccp not valid, skip
    }
  }
  husOld = husNew
  numOld = numNew
}


##############
# METHOD 2
##############
# Método 2: de un año a otro, extraer pérdidas y ganancias de cada landcover, y distribuirlas equitativamente. Es decir, si por ejemplo maíz supone un 10% de todas las ganancias de cobertura ese año, supondríamos que el 10% de las pérdida de cada landcover que pierde cobertura, ha ido a parar a cobertura de maíz.
# if already calculated: df_LCtransitions = read.csv(paste0(dataFolder,"landCoverChange/landCoverChange_method2.csv"), header=T, row.names=1)

df_LCtransitions = data.frame(matrix(0, ncol = length(landcovertypes), nrow = length(landcovertypes)))
rownames(df_LCtransitions) = landcovertypes
colnames(df_LCtransitions) = landcovertypes
df_LCtransitionsOld = df_LCtransitions # use a copy to do sanity checks at every step

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
    else {
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
  }

  husOld     = husNew
  numOld     = numNew
  segAreaOld = segAreaNew
  df_LCtransitionsOld = df_LCtransitions
}


############################
# NORMALIZED TRANSITIONS
############################
df_LCdestin = df_LCtransitions
df_LCorigin = as.data.frame(t(df_LCdestin))
# Normalized transitions can be read, at each row, as the probability of transition from the row lc type, to the columns land cover types -> destination probability
# If df_LCtransitions is transposed, the normalization yields the probability of transition to the row lc type, from the columns land cover types -> origin probability
df_LCdestin_prob = df_LCdestin*0
for (lc in landcovertypes) {
  if (rowSums(df_LCdestin[lc,]) > 0) df_LCdestin_prob[lc, ] = df_LCdestin[lc, ] /  rowSums(df_LCdestin[lc,])
}
df_LCtransitions_t = as.data.frame(t(df_LCtransitions))
df_LCorigin_prob = df_LCorigin*0
for (lc in landcovertypes) {
  if (rowSums(df_LCorigin[lc,]) > 0) df_LCorigin_prob[lc, ] = df_LCorigin[lc, ] /  rowSums(df_LCorigin[lc,])
}

############################
# BY GROUP OF LANDCOVER
############################
# groups = list(c("cerealGrain",cerealGrain),
#               c("legumeGrain",legumeGrain),
#               c("tuber",tuber),
#               c("industrial",industrial),
#               c("fodder",fodder),
#               c("vegetable",vegetable),
#               c("orchard",orchard),
#               c("ornamental",ornamental),
#               c("citric",citric),
#               c("fruitNoCitric",fruitNoCitric),
#               c("vineyard",vineyard),
#               c("oliveTrees",oliveTrees),
#               c("nursery",nursery),
#               c("forested",c(forested,otherWoodyCrop)),
#               c("pasture",pasture),
#               c("fallow",other),
#               c("improductive",improductive),
#               c("notAgri",notAgri))
groups = list(c("Agri Land",agriLand),
              c("Forested",c(forested,otherWoodyCrop)),
              c("Pasture",pasture),
              c("Abandoned",abandAgri),
              c("Improductive",improductive),
              c("Artificial",notAgri))
# groups = list(c("Poll-dep",pollDependent),
#               c("Poll-indep",pollNotDepent),
#               c("Unkn-dep",pollUnknown),
#               c("Fallow", lowActivity),
#               c("Forested",c(forested,otherWoodyCrop)),
#               c("Pasture",pasture),
#               c("Abandoned",abandAgri),
#               c("Improductive",improductive),
#               c("Artificial",notAgri))
# Land cover types not in groups:
grpElts = unlist(lapply(groups, function(x) x[2:length(x)]))
paste0("Land cover types not considered:",landcovertypes[!(landcovertypes %in% grpElts)])

# If method 1, add water and other:
# groups = list.append(groups,c("water",c("water")),c("other",c("other"))) 

# ORIGIN
df_LCorigin_gr = data.frame(matrix(0, ncol = length(groups), nrow = length(groups)))
rownames(df_LCorigin_gr) = lapply(groups, `[[`, 1)
colnames(df_LCorigin_gr) = lapply(groups, `[[`, 1)
for(groupRow in groups) {
  for (groupCol in groups)
  {
    rowName    = groupRow[1]
    colName    = groupCol[1]
    lcTypesRow = groupRow[2:length(groupRow)]
    lcTypesCol = groupCol[2:length(groupCol)]
    df_subset  = df_LCorigin[lcTypesRow, lcTypesCol] 
    df_LCorigin_gr[rowName,colName] = sum(df_subset)
  }
}
df_LCorigin_gr_norm = df_LCorigin_gr*0
for (group in rownames(df_LCorigin_gr)) {
  if (rowSums(df_LCorigin_gr[group,]) > 0) df_LCorigin_gr_norm[group, ] = df_LCorigin_gr[group, ] /  rowSums(df_LCorigin_gr[group,])
}

# DESTINATION
df_LCdestin_gr = data.frame(matrix(0, ncol = length(groups), nrow = length(groups)))
rownames(df_LCdestin_gr) = lapply(groups, `[[`, 1)
colnames(df_LCdestin_gr) = lapply(groups, `[[`, 1)
for(groupRow in groups) {
  for (groupCol in groups)
  {
    rowName    = groupRow[1]
    colName    = groupCol[1]
    lcTypesRow = groupRow[2:length(groupRow)]
    lcTypesCol = groupCol[2:length(groupCol)]
    df_subset  = df_LCdestin[lcTypesRow, lcTypesCol] 
    df_LCdestin_gr[rowName,colName] = sum(df_subset)
  }
}
df_LCdestin_gr_norm = df_LCdestin_gr*0
for (group in rownames(df_LCdestin_gr)) {
  if (rowSums(df_LCdestin_gr[group,]) > 0) df_LCdestin_gr_norm[group, ] = df_LCdestin_gr[group, ] /  rowSums(df_LCdestin_gr[group,])
}


############
# PLOTS
############
figFolder = "C:/Users/angel/git/ESYRCE/figures/"
removeNoTransition = FALSE
abbreviateNames = FALSE

# Origin agri land
target = "Agri Land"
title = "Agricultural Land origin"
df_plot = df_LCorigin_gr_norm
df_melt = melt(as.matrix(df_plot))
colnames(df_melt) = c("origin","final","value")
if (removeNoTransition) df_melt = df_melt[ df_melt$origin != df_melt$final , ]
if (abbreviateNames) {
  df_melt$origin = abbreviate(df_melt$origin)
  df_melt$final  = abbreviate(df_melt$final)  
}
df_melt_sel = df_melt[df_melt$origin == target,]
(p<-ggplot(data=df_melt_sel, aes(x=final, y=value)) +
    geom_bar(stat="identity") + 
    ggtitle(title))
# Save in a png file
png(paste0(figFolder,title,".png")) 
p
dev.off() 

# Destination agri land
target = "Agri Land"
title = "Agricultural Land destination"
df_plot = df_LCdestin_gr_norm
df_melt = melt(as.matrix(df_plot))
colnames(df_melt) = c("origin","final","value")
if (removeNoTransition) df_melt = df_melt[ df_melt$origin != df_melt$final , ]
if (abbreviateNames) {
  df_melt$origin = abbreviate(df_melt$origin)
  df_melt$final  = abbreviate(df_melt$final)  
}
df_melt_sel = df_melt[df_melt$origin == target,]
(p<-ggplot(data=df_melt_sel, aes(x=final, y=value)) +
    geom_bar(stat="identity") + 
    ggtitle(title))
# Save in a png file
png(paste0(figFolder,title,".png")) 
p
dev.off() 

# Origin poll-dep crops
target = "Poll-dep"
title = "Poll-dep crops origin"
df_plot = df_LCorigin_gr_norm
df_melt = melt(as.matrix(df_plot))
colnames(df_melt) = c("origin","final","value")
if (removeNoTransition) df_melt = df_melt[ df_melt$origin != df_melt$final , ]
if (abbreviateNames) {
  df_melt$origin = abbreviate(df_melt$origin)
  df_melt$final  = abbreviate(df_melt$final)  
}
df_melt_sel = df_melt[df_melt$origin == target,]
(p<-ggplot(data=df_melt_sel, aes(x=final, y=value)) +
    geom_bar(stat="identity") + 
    ggtitle(title)) + ylim(0,0.25)
# Save in a png file
png(paste0(figFolder,title,".png")) 
p
dev.off() 

# Origin poll-indep crops
target = "Poll-indep"
title = "Poll-indep crops origin"
df_plot = df_LCorigin_gr_norm
df_melt = melt(as.matrix(df_plot))
colnames(df_melt) = c("origin","final","value")
if (removeNoTransition) df_melt = df_melt[ df_melt$origin != df_melt$final , ]
if (abbreviateNames) {
  df_melt$origin = abbreviate(df_melt$origin)
  df_melt$final  = abbreviate(df_melt$final)  
}
df_melt_sel = df_melt[df_melt$origin == target,]
(p<-ggplot(data=df_melt_sel, aes(x=final, y=value)) +
    geom_bar(stat="identity") + 
    ggtitle(title)) + ylim(0,0.25)
# Save in a png file
png(paste0(figFolder,title,".png")) 
p
dev.off() 

# All groups
# (p1<-ggplot(data=df_melt, aes(x=final, y=value)) +
#   geom_bar(stat="identity")) + 
#   ggtitle("Transitions") +
#   facet_wrap(~origin)

##################
# ANALYSE REGIONS 
##################
df_data = df_data[df_data$region == "Galicia",] # By region/province?
df_lcsums = data.frame(landcover = landcovertypes, totalArea=colSums(df_data[,prop_landcovertypes]),row.names = NULL)
df_lcsums_groups = data.frame(matrix(0, ncol = 1, nrow = 0))
colnames(df_lcsums_groups) = c("totalArea")
for(group in groups) {
  name    = group[1]
  lcTypes = group[2:length(group)]
  df_subset = df_lcsums[df_lcsums$landcover %in% lcTypes, ] 
  df_lcsums_groups[name, "totalArea"] = sum(df_subset$totalArea)
}
df_lcsums_groups$landcover = abbreviate(rownames(df_lcsums_groups))
(pie<- ggplot(df_lcsums_groups, aes(x="", y=totalArea, fill=landcover))+
  geom_bar(width = 1, stat = "identity", color="white") +
    coord_polar("y", start=0))
(bp<-ggplot(data=df_lcsums_groups, aes(x=landcover, y=totalArea)) +
  geom_bar(stat="identity"))



# Save data
write.csv(df_LCtransitions_groups, file=paste0(dataFolder,"landCoverChange/landCoverChangeGroupsGalicia_method2.csv"))
write.csv(df_LCtransitions_groups_norm, file=paste0(dataFolder,"landCoverChange/landCoverChangeGroupsNormGalicia_method2.csv"))
