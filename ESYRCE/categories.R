# Organize land cover columns from ESYRCE into more general categories
cerealGrain = c('hardWheat',       
                'softWheat',       
                'barleyTwoRows',   
                'barleySixRows',   
                'oat',             
                'rye',             
                'triticale',       
                'rice',            
                'maize',           
                'sorghum',         
                'mixCerealGrain',  
                'otherCerealGrain',
                'quinoa')

legumeGrain = c('frijol',          
                'fabaBean',        
                'lentil',          
                'chickpea',        
                'pea',             
                'commonVetch',     
                'lupine',          
                'carob',
			        	'ervil',
                'otherLegumeGrain')	

tuber = c('potato',        
          'sweetPotato',   
          'yellowNutsedge',
          'otherTuber')	  

industrial = c(	'sugarcane',      
                'sugarbeet',      
                'cotton',         
                'sunflower',      
                'soybean',        
                'rapeseed',       
                'flax',           
                'peanut',         
                'camelina',       
                'safflower',      
                'otherOleaginous',
                'tobacco',        
                'industTomato',   
                'capsicumPaprika',
                'condiment',      
                'hops',           
                'finesHerbes',    
                'otherIndustrial')

fodder = c('maizeFodder',      
           'alfalfa',          
           'vetchFodder',      
	         'otherFodder',      
		       'grasslandPolifite',
		       'turnipFodder',     
		       'beetFodder',       
		       'collard',          
		       'otherWeedFodder')	 

vegetable = c('betaVulgarisCicla',		  
		          'garlic',           
		          'artichoke',        
		          'celery',           
		          'eggplant',         
		          'pumpkin',          
		          'zucchini',         
		          'agaricusBisporus', 
		          'mushroom',         
		          'onion',            
		          'broccoli',         
		          'cabbage',          
		          'cauliflower',      
		          'endive',           
		          'aspargus',         
		          'spinach',          
		          'sweetCorn',        
		          'strawberry',       
		          'rapini',           
              'greenPea',         
              'broadBean',        
              'greenBean',       
              'lettuce',          
              'redCabbage',       
              'melon',            
              'cucumber',         
              'leek',             
              'beetTable',        
              'sweetPepper',      
              'watermelon',
		          'tomato',
              'carrot',           
              'otherVegetable',   
              'emptyGarden')			

ornamental = c('ornamental')

citric = c('orange',      
           'clementine',  
           'lemon',       
           'bitterOrange',
           'grapefruit',  
           'otherCitrics')

fruitNoCitric = c('apple',            
                  'pear',             
                  'quince',           
                  'loquat',           
                  'apricot',          
                  'cherry',           
                  'peach',            
                  'plum',             
                  'fig',              
                  'cherimoya',        
                  'avocado',          
                  'banana',           
                  'persimmon',        
                  'papaya',           
                  'pineapple',        
                  'kiwi',             
                  'barbaryFig',       
                  'mango',            
                  'pomegranate',      
                  'almond',           
                  'walnut',           
                  'hazelnut',         
                  'chestnut',         
                  'redRaspberry',     
                  'pistachio',        
                  'otherFruitNoCitric')		

vineyard = c('whiteGrapeSeedless',				  
			       'whiteGrape',        	  
			       'redGrapeSeedless',  	  
			       'redGrape',          	  
			       'transfGrape')

oliveTrees = c('oliveTable', 			 
		           'olive',      	  
		           'oliveMill')

otherWoodyCrop = c('carobTree',     
                   'otherOtherWoody')		

nursery = c('nursery')

orchard = c('orchard')

forested = c('poplar',      
             'pawlonia',          
             'quercusIlexTruffle',
			       'conifers',     
			       'broadleafFast',
			       'broadleafSlow',
			       'mixedForest',  
			       'shrub')

improductive = c('improductive')

notAgri = c('notAgri')

lowActivity = c('fallow',     
          'emptyGreenh',
          'posio',      
          'spartizal')

abandAgri = c('wasteland','abandoned') # erial y baldío

pasture = c("naturalMeadow", 
            "highMountainMeadow", 
            "pastureGrassland", 
            "pastureShrub") 
		  
soilMaint    = c("traditional", 
                 "minimal", 
                 "spontVegCover", 
                 "sowedVegCover", 
                 "inertCover", 
                 "noMainten", 
                 "noTillage")

sowTechn     = c("directSowing", 
                 "traditSowing")

avgSize = c("avgSize", "avgFieldSize", "avgSeminatSize", "avgOtherSize", "avgSizeDiss", "avgSeminatSizeDiss", "avgFieldSizeDiss", "avgOtherSizeDiss")

edges = c("edgeDensity","edgeDenSeminat","edgeDenFields","edgeDenOther","edgeDensityDiss","edgeDenSemiDiss","edgeDenFielDiss","edgeDenOtherDiss")

heterogen = c("heterogeneity")

demand = c("demand")

# System
irrigated = c("waterScarce", "irrigated", "greenhouse")
dry = c("dry")

agriLand  = c(cerealGrain, legumeGrain, tuber, industrial, fodder, vegetable, orchard, 
                ornamental, citric, fruitNoCitric, vineyard, oliveTrees, nursery, lowActivity)   

agriActive = c(cerealGrain, legumeGrain, tuber, industrial, fodder, vegetable, orchard, 
                   ornamental, citric, fruitNoCitric, vineyard, oliveTrees, nursery)

seminatural = c(forested, otherWoodyCrop, pasture)

fruitTree = c(citric, fruitNoCitric)

landcovertypes = c(agriLand, seminatural, abandAgri, improductive, notAgri)

prop_landcovertypes  = paste0("prop_",landcovertypes)
yield_landcovertypes = paste0("yield_",landcovertypes)
var_landcovertypes   = paste0("var_",landcovertypes)

# Levels of pollinators' dependence
essential = c('pumpkin','zucchini','melon','watermelon','cherimoya','kiwi')
great     = c('turnipFodder','cucumber','apple','pear','loquat','apricot','cherry','peach','plum','avocado','mango','almond','redRaspberry')
modest    = c('sunflower','soybean','rapeseed','industTomato','capsicumPaprika','eggplant','strawberry','broadBean','fig','pomegranate','chestnut',
              'orchard')
little    = c('flax','peanut','safflower','tomato','orange','clementine','lemon',       
              'bitterOrange','grapefruit','otherCitrics','persimmon','papaya')
noIncrease= c(cerealGrain,'lentil','chickpea','pea','carob','yellowNutsedge','sugarcane','sugarbeet','hops','beetFodder','betaVulgarisCicla',
              'spinach','sweetCorn','greenPea','lettuce','sweetPepper','beetTable','walnut','hazelnut','pistachio','whiteGrapeSeedless',				  
              'whiteGrape','redGrapeSeedless','redGrape','transfGrape','oliveTable','olive','oliveMill','carobTree','poplar')
increase  = c('frijol','fabaBean','commonVetch','lupine','cotton','camelina','rapini','greenBean','quince','barbaryFig','pawlonia')
unknown   = c('ervil','otherLegumeGrain','otherTuber','otherOleaginous','condiment','finesHerbes','otherIndustrial','maizeFodder','otherFodder',
              'grasslandPolifite','collard','otherWeedFodder','agaricusBisporus','mushroom','otherVegetable','emptyGarden','ornamental',
              'otherFruitNoCitric','otherOtherWoody','nursery','quercusIlexTruffle','conifers','broadleafFast','broadleafSlow','mixedForest',  
              'shrub')
increaseBreeding = c('potato','sweetPotato','garlic','banana','pineapple')
increaseSeedProd = c('tobacco','alfalfa','vetchFodder', 'artichoke','celery','onion','broccoli','cabbage','cauliflower','endive','aspargus',
                     'redCabbage','leek','carrot')

pollDependent = c(little, modest, essential, great, increase)
pollNotDepent = c(noIncrease, increaseBreeding, increaseSeedProd)
pollUnknown   = unknown

# # Sanity check (columns categorized)
# landcovertypes[!(landcovertypes %in% c(pollDependent, pollNotDepent, pollUnknown))]
# for (name in colnames(df_data)) { if(name %in% prop_landcovertypes  | 
#                                      name %in% yield_landcovertypes | 
#                                      name %in% var_landcovertypes   |
#                                      name %in% soilMaint            |
#                                      name %in% sowTechn             |
#                                      name %in% avgSize              |
#                                      name %in% heterogen            |
#                                      name %in% demand               |
#                                      name %in% edges                |
#                                      name %in% irrigated            |
#                                      name %in% dry) {} else {print(name)} }



