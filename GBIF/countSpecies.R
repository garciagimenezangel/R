
library(dismo)

countSpecies = function(species, args="", bGeo=TRUE) {
  splitted = strsplit(species, " ")
  genus = splitted[[1]][1]
  sp = splitted[[1]][2]
  count = gbif(genus, sp, args=args, geo=bGeo, download = FALSE)
  df = as.data.frame(t(c(species,count)))
  names(df) = names(dfCount)
  df
}

speciesEuropeKleijn2015 = c("Bombus terrestris",
                            "Bombus lapidarius",
                            "Andrena chrysosceles",
                            "Andrena flavipes",
                            "Andrena haemorrhoa",
                            "Andrena carantonica",
                            "Bombus pascuorum",
                            "Andrena fulva",
                            "Andrena dorsata",
                            "Lasioglossum calceatum",
                            "Lasioglossum malachurum",
                            "Bombus hypnorum",
                            "Osmia bicornis",
                            "Bombus pratorum",
                            "Andrena nitida",
                            "Andrena minutula",
                            "Bombus hortorum",
                            "Lasioglossum politum",
                            "Lasioglossum morio",
                            "Andrena cineraria")

pars = c("year=1990,2020")
dfCount = do.call(rbind, lapply(speciesEuropeKleijn2015, countSpecies, pars))

field_data_folder = "../../OBservData/Dominant_Pollinators_Data/"
pollinators = read.csv(file = paste(field_data_folder, 'Summary_dominant_species.csv',sep=""), header = TRUE)
pollinators = unique(pollinators$pollinator)

dfCount["inOBServDominant"] = dfCount$species %in% pollinators
