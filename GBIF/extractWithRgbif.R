#Gbif----
library(rgbif)
#get codes of places and families
spain_code <- isocodes[grep("Spain", isocodes$name), "code"]
portugal_code <- isocodes[grep("Portugal", isocodes$name), "code"]
apidae_key <- name_backbone(name="Apidae", rank = "family")$usageKey
andrenidae_key <- name_backbone(name="Andrenidae", rank = "family")$usageKey
halictidae_key <- name_backbone(name="Halictidae", rank = "family")$usageKey
colletidae_key <- name_backbone(name="Colletidae", rank = "family")$usageKey
megachilidae_key <- name_backbone(name="Megachilidae", rank = "family")$usageKey
stenotritidae_key <- name_backbone(name="Stenotritidae", rank = "family")$usageKey
melittidae_key <- name_backbone(name="Melittidae", rank = "family")$usageKey

occ_count(taxonKey= apidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #2679
occ_count(taxonKey= andrenidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #722
occ_count(taxonKey= halictidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #958
occ_count(taxonKey= colletidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #597
occ_count(taxonKey= megachilidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #1623
occ_count(taxonKey= stenotritidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #0 (expected)
occ_count(taxonKey= melittidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #178

occ_count(taxonKey= apidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #1286
occ_count(taxonKey= andrenidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #133
occ_count(taxonKey= halictidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #329
occ_count(taxonKey= colletidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #139
occ_count(taxonKey= megachilidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #238
occ_count(taxonKey= stenotritidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #0 (expected)
occ_count(taxonKey= melittidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #10
#fetch data
dat <-  data.frame(scientificName = NA, decimalLatitude = NA,
                   decimalLongitude = NA, scientificName = NA,
                   family = NA, genus = NA, species = NA,
                   year = NA, month = NA, day = NA, recordedBy = NA,
                   identifiedBy = NA, sex = NA, stateProvince = NA,
                   locality = NA, coordinatePrecision = NA)
for(i in c(apidae_key, andrenidae_key,
           halictidae_key, colletidae_key,
           megachilidae_key, 
           melittidae_key)){
  temp <- occ_search(taxonKey= i, 
                     return='data', 
                     hasCoordinate=TRUE,
                     hasGeospatialIssue=FALSE,
                     limit=7000, #safe threshold based on rounding up counts above
                     country = c(spain_code, portugal_code),
                     fields = c('scientificName','name', 'decimalLatitude',
                                'decimalLongitude', 'scientificName',
                                'family','genus', 'species',
                                'year', 'month', 'day', 'recordedBy',
                                'identifiedBy', 'sex', 'stateProvince', 
                                'locality', 'coordinatePrecision'))
  if(length(temp$PT) == 1){
    temp$PT <- data.frame(scientificName = NA, decimalLatitude = NA,
                          decimalLongitude = NA, scientificName = NA,
                          family = NA, genus = NA, species = NA,
                          year = NA, month = NA, day = NA, recordedBy = NA,
                          identifiedBy = NA, sex = NA,  stateProvince = NA,
                          locality = NA, coordinatePrecision = NA)
  }
  if(is.null(temp$ES$sex)){
    temp$ES$sex <- NA
  }
  if(is.null(temp$PT$sex)){
    temp$PT$sex <- NA
  }
  if(is.null(temp$PT$coordinatePrecision)){
    temp$PT$coordinatePrecision <- NA
  }
  if(is.null(temp$ES$coordinatePrecision)){
    temp$ES$coordinatePrecision <- NA
  }
  if(is.null(temp$ES$stateProvince)){
    temp$ES$stateProvince <- NA
  }
  if(is.null(temp$PT$stateProvince)){
    temp$PT$stateProvince <- NA
  }
  temp$ES <- temp$ES[,c('scientificName','decimalLatitude',
                        'decimalLongitude', 'scientificName',
                        'family','genus', 'species',
                        'year', 'month', 'day', 'recordedBy',
                        'identifiedBy', 'sex',  'stateProvince', 
                        'locality', 'coordinatePrecision')]
  temp$PT <- temp$PT[,c('scientificName','decimalLatitude',
                        'decimalLongitude', 'scientificName',
                        'family','genus', 'species',
                        'year', 'month', 'day', 'recordedBy',
                        'identifiedBy', 'sex',  'stateProvince', 
                        'locality', 'coordinatePrecision')]
  dat <- rbind(dat, as.data.frame(temp$ES), as.data.frame(temp$PT))
}
dat <- dat[-1,]
head(dat)
tail(dat)
dim(dat) #8889
