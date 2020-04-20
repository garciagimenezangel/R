
library(dismo)

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

# Download occurrences of Bombus terrestris, from 1990-2020
pars = c("year=1990,2020")
species = speciesEuropeKleijn2015[1]
splitted = strsplit(species, " ")
genus = splitted[[1]][1]
sp = splitted[[1]][2]
bTerr = gbif(genus, sp, args=pars, geo=TRUE)






