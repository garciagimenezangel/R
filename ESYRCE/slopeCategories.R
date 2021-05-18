# categorize slope seminatural
seminatural2 = c(seminatural_forest, seminatural_meadow, seminatural_shrub) # seminatural[ !(seminatural %in% seminatural2) ] = "pawlonia","quercusIlexTruffle","carobTree","otherOtherWoody" 
# df_seminatural2$relative_slope = df_seminatural2$slope_seminatural2 / df_seminatural2$mean_seminatural2
# df_seminatural2$slope_seminatural_category = 0
# categPos = df_seminatural2$relative_slope > 0.2
# categNeg = df_seminatural2$relative_slope < -0.2
# df_seminatural2$slope_seminatural_category[categPos] = 1
# df_seminatural2$slope_seminatural_category[categNeg] = -1
# df_metricsAll = merge(df_metricsAll, df_seminatural2[,c("D1_HUS", "D2_NUM", "slope_seminatural_category")], by=c("D1_HUS", "D2_NUM")) # add province