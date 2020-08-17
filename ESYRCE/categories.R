
# Organize columns in categories (e.g. agriculture, seminatural...)
agriLand    = c("cerealGrain", 
                "legumeGrain", 
                "tuber", 
                "industrial",
                "fodder", 
                "vegetable", 
                "ornamental", 
                "citric",
                "fruitNoCitric",
                "vineyard",
                "oliveTrees",
                "otherWoodyCrop",
                "nursery",
                "association",
                "emptyGreenh")

pasture     = c("naturalMeadow", 
                "highMountainMeadow", 
                "pastureGrassland", 
                "pastureShrub")

seminatural = c("orchard", 
                "fallow", 
                "posio", 
                "conifers", 
                "broadleafFast", 
                "broadleafSlow", 
                "poplar", 
                "mixedForest", 
                "shrub", 
                "wasteland", 
                "spartizal", 
                "abandoned", 
                "improductive")

artificial   = c("notAgri")

soilMaint    = c("traditional", 
                 "minimal", 
                 "spontVegCover", 
                 "sowedVegCover", 
                 "inertCover", 
                 "noMainten", 
                 "noTillage")

sowTechn     = c("directSowing", 
                 "traditSowing")

crops       = c("hardWheat",
                "softWheat",
                "barleyTwoRows",
                "barleySixRows",
                "oat",
                "rye",
                "rice",
                "maize",
                "quinoa",
                "frijol",
                "fabaBean",
                "lentil",
                "chickpea",
                "pea",
                "ervil",
                "potato",
                "sugarbeet",
                "cotton",
                "sunflower",
                "soybean",
                "rapeseed",
                "industTomato",
                "fodderMaize",
                "garlic",
                "artichoke",
                "eggplant",
                "zucchini",
                "onion",
                "strawberry",
                "greenPea",
                "greenBean",
                "kidneyBean",
                "melon",
                "cucumber",
                "sweetPepper",
                "watermelon",
                "carrot",
                "orange",
                "clementine",
                "lemon",
                "apple",
                "pear",
                "apricot",
                "cherry",
                "peach",
                "plum",
                "banana",
                "almond",
                "walnut",
                "whiteGrapeSeedless",
                "whiteGrape",
                "redGrapeSeedless",
                "redGrape",
                "transfGrape",
                "olive",
                "oliveTable",
                "oliveMill")

varianceYield = paste0("var_", crops)

fieldSize = c("avgFieldSize")

heterogen = c("heterogeneity")

demand    = c("demand")

# Sanity check (columns categorized)
#for (name in colnames(df_data)) { if(name %in% agriculture | name %in% pastures | name %in% seminatural | name %in% artificial | name %in% soilMaint| name %in% sowTechn | name %in% crops | name %in% varianceYield | name %in% fieldSize | name %in% heterogen | name %in% demand) {} else {print(name)} }



