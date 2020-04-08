library(dplyr)

# TODO: igual se puede usar más especies de OBSERV, o si no, especies de GBIF

traitbase_file = "C:/Users/angel.gimenez/Documents/DATA/OBServ/Traitbase/Apoidea/data.csv"
trait_data = read.csv(file=traitbase_file,header=T)

observ_file = "C:/Users/angel.gimenez/Documents/REPOSITORIES/OBservData/Dominant_Pollinators_Data/Summary_dominant_species.csv"
observ_data = read.csv(file=observ_file,header=T)

species_traitbase = unique(tdata$species)
species_observ = unique(tdata2$pollinator)

species_traitObserv = species_observ[species_observ %in% species_traitbase]
trait_data_species_traitObserv = trait_data[trait_data$species %in% species_traitObserv, ]
