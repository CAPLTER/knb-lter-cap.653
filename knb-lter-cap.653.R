
# README ----


# libraries ----
library(EML)
library(RPostgreSQL)
library(RMySQL)
library(tidyverse)
library(tools)
library(aws.s3)

# reml-helper-functions ----
# source('~/localRepos/reml-helper-tools/createdataTableFn.R')
source('~/localRepos/reml-helper-tools/writeAttributesFn.R')
source('~/localRepos/reml-helper-tools/createDataTableFromFileFn.R')
source('~/localRepos/reml-helper-tools/createKMLFn.R')
source('~/localRepos/reml-helper-tools/address_publisher_contact_language_rights.R')
source('~/localRepos/reml-helper-tools/createOtherEntityFn.R')
source('~/localRepos/reml-helper-tools/createPeople.R')
source('~/localRepos/reml-helper-tools/createFactorsDataframe.R')

# quick function to print unique values of all fields (except date)
unique_values <- function(dataframe) {
  apply(dataframe[,-which(names(dataframe) == "sample_date")], 2, function(x) { unique(x) })
}


# connections ----

# Amazon
source('~/Documents/localSettings/aws.s3')
  
# postgres
source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')
  
pg <- pg_prod
pg <- pg_local

# mysql
source('~/Documents/localSettings/mysql_prod.R')
prod <- mysql_prod

# dataset details to set first ----
projectid <- 653
packageIdent <- 'knb-lter-cap.653.1'
pubDate <- '2018-01-10'

# data processing ---------------------------------------------------------
source('../knb-lter-cap.652/esca_sql_queries.R')


# parcel_characteristics --------------------------------------------------

parcel_characteristics <- get_parcel_characteristics('parcel')

# empty strings to NA (skip the sample_date field [1])
parcel_characteristics[,-1][parcel_characteristics[,-1] == ''] <- NA

# review unique values
unique_values(parcel_characteristics)

parcel_characteristics <- parcel_characteristics %>% 
  mutate(
    parcel_survey_type = as.factor(parcel_survey_type),
    parcel_residence_type = gsub("single_family", "single family", parcel_residence_type),
    parcel_residence_type = gsub("single familiy", "single family", parcel_residence_type),
    parcel_social_class = as.factor(parcel_social_class),
    parcel_appearance = gsub("neigh_yard_upkeep_good", "good", parcel_appearance),
    parcel_appearance = gsub("neigh_yard_upkeep_poor", "poor", parcel_appearance),
    parcel_appearance = as.factor(parcel_appearance),
    parcel_orderliness = gsub("lacking_order_structure", "lacking order or structure", parcel_orderliness),
    parcel_orderliness = gsub("highly_structured_designed", "highly structured or designed", parcel_orderliness),
    parcel_orderliness = as.factor(parcel_orderliness),
    presence_of_bird_feeder = as.factor(presence_of_bird_feeder),
    presence_of_water_feature = as.factor(presence_of_water_feature),
    presence_of_porch_patio = as.factor(presence_of_porch_patio),
    presence_of_cats = as.factor(presence_of_cats),
    presence_of_dogs = as.factor(presence_of_dogs),
    presence_of_pet_waste = as.factor(presence_of_pet_waste),
    presence_of_statues = as.factor(presence_of_statues),
    presence_of_flagpole = as.factor(presence_of_flagpole),
    presence_of_cars_yard = as.factor(presence_of_cars_yard),
    presence_of_potted_plant = as.factor(presence_of_potted_plant),
    presence_of_play_equip = as.factor(presence_of_play_equip),
    presence_of_lawn_ornaments = as.factor(presence_of_lawn_ornaments),
    presence_of_furniture = as.factor(presence_of_furniture),
    presence_of_river_bed = as.factor(presence_of_river_bed),
    presence_of_yard_topography = as.factor(presence_of_yard_topography),
    presence_of_litter = as.factor(presence_of_litter),
    presence_of_veg_litter = as.factor(presence_of_veg_litter),
    presence_of_yard_tools = as.factor(presence_of_yard_tools),
    presence_of_light_post = as.factor(presence_of_light_post),
    presence_of_other = as.factor(presence_of_other),
    presence_of_irrigation_flood = as.factor(presence_of_irrigation_flood),
    presence_of_irrigation_drip = as.factor(presence_of_irrigation_drip),
    presence_of_irrigation_hose = as.factor(presence_of_irrigation_hose),
    presence_of_irrigation_sprinklers = as.factor(presence_of_irrigation_sprinklers),
    landscape_type = as.factor(landscape_type),
    amount_grass = as.factor(amount_grass),
    weed_quantity_mesic = as.factor(weed_quantity_mesic),
    weed_quantity_xeric = as.factor(weed_quantity_xeric),
    presence_of_weeds_other = as.factor(presence_of_weeds_other),
    parcel_pruning_trees = as.factor(parcel_pruning_trees),
    parcel_pruning_shrubs = as.factor(parcel_pruning_shrubs),
    parcel_grass_patchiness = as.factor(parcel_grass_patchiness),
    lawn_health = as.factor(lawn_health),
    lawn_quality = as.factor(lawn_quality),
    presence_of_lawn_trimmed = as.factor(presence_of_lawn_trimmed),
    presence_of_recent_cut = as.factor(presence_of_recent_cut)
  )

writeAttributes(parcel_characteristics) # write data frame attributes to a csv in current dir to edit metadata

parcel_characteristics_desc <- "General characteristics concerning the appearance, health, and presence of specific attributes of the parcel yard. The front and back yards of the parcel are surveyed independently."

factorsToFrame(parcel_characteristics)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
parcel_characteristics_DT <- createDTFF(dfname = parcel_characteristics,
                                        description = parcel_characteristics_desc,
                                        dateRangeField = 'sample_date')


# human_indicators --------------------------------------------------------

human_indicators <- get_human_indicators('parcel')

# empty strings to NA (skip the sample_date field [1])
human_indicators[,-1][human_indicators[,-1] == ''] <- NA

# review unique values
unique_values(human_indicators)

human_indicators <- human_indicators %>% 
  mutate(
    human_presence_of_path = as.factor(human_presence_of_path),
    human_footprints = as.factor(human_footprints),
    human_bike_tracks = as.factor(human_bike_tracks),
    human_off_road_vehicle = as.factor(human_off_road_vehicle),
    human_small_litter = as.factor(human_small_litter),
    human_dumped_trash_bags = as.factor(human_dumped_trash_bags),
    human_abandoned_vehicles = as.factor(human_abandoned_vehicles),
    human_graffiti = as.factor(human_graffiti),
    human_injured_plants = as.factor(human_injured_plants),
    human_informal_play = as.factor(human_informal_play),
    human_informal_recreation = as.factor(human_informal_recreation),
    human_informal_living = as.factor(human_informal_living),
    human_sports_equipment = as.factor(human_sports_equipment),
    human_social_class = as.factor(human_social_class)
  )

writeAttributes(human_indicators) # write data frame attributes to a csv in current dir to edit metadata
human_indicators_desc <- "The presence of features or characteristics that are reflective of human presence or activity in the parcel yard."

factorsToFrame(human_indicators)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
human_indicators_DT <- createDTFF(dfname = human_indicators,
                                  description = human_indicators_desc,
                                  dateRangeField = 'sample_date')


# landscape_irrigation ----------------------------------------------------

landscape_irrigation <- get_landscape_irrigation('parcel')

# empty strings to NA (skip the sample_date field [1])
landscape_irrigation[,-1][landscape_irrigation[,-1] == ''] <- NA

# review unique values
unique_values(landscape_irrigation)

# standardize responses (note na is not applicable (not NA sensu R))
# landscape_irrigation[,-1][landscape_irrigation[,-1] == 'dont know'] <- 'do_not_know'
# landscape_irrigation[,-1][landscape_irrigation[,-1] == 'na'] <- 'n/a'

landscape_irrigation <- landscape_irrigation %>% 
  mutate(
    appears_maintained = as.factor(appears_maintained),
    appears_professional = as.factor(appears_professional),
    appears_healthy = as.factor(appears_healthy),
    appears_injured = as.factor(appears_injured),
    presence_of_open_ground = as.factor(presence_of_open_ground),
    presence_of_trees = as.factor(presence_of_trees),
    presence_of_shrubs = as.factor(presence_of_shrubs),
    presence_of_cacti_succ = as.factor(presence_of_cacti_succ),
    presence_of_lawn = as.factor(presence_of_lawn),
    presence_of_herbaceous_ground = as.factor(presence_of_herbaceous_ground),
    presence_of_other = as.factor(presence_of_other),
    presence_of_hand_water = as.factor(presence_of_hand_water),
    presence_of_drip_water = as.factor(presence_of_drip_water),
    presence_of_overhead_water = as.factor(presence_of_overhead_water),
    presence_of_flood_water = as.factor(presence_of_flood_water),
    # presence_of_subterranean_water = as.factor(presence_of_subterranean_water), # only NAs throwing off function so need to do by hand
    presence_of_no_water = as.factor(presence_of_no_water),
    presence_of_pervious_irrigation = as.factor(presence_of_pervious_irrigation)
  )

writeAttributes(landscape_irrigation) # write data frame attributes to a csv in current dir to edit metadata

factorsToFrame(landscape_irrigation)

landscape_irrigation_desc <- "Characteristics reflective of the landscape type, health, and quality in the parcel yard."

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
landscape_irrigation_DT <- createDTFF(dfname = landscape_irrigation,
                                      description = landscape_irrigation_desc,
                                      dateRangeField = 'sample_date')


# shrubs_cacti_succulents -------------------------------------------------

shrubs_cacti_succulents <- get_shrubs_cacti_succulents('parcel') %>% 
  select(-year) %>% 
  mutate(
    vegetation_shape_code = as.factor(vegetation_shape_code),
    vegetation_classification_code = as.factor(vegetation_classification_code)
  )

# empty strings to NA (skip the sample_date field [1])
shrubs_cacti_succulents[,-1][shrubs_cacti_succulents[,-1] == ''] <- NA
# shrubs_cacti_succulents[,-1][shrubs_cacti_succulents[,-1] == ' '] <- NA

# review unique values
unique_values(shrubs_cacti_succulents)

writeAttributes(shrubs_cacti_succulents) # write data frame attributes to a csv in current dir to edit metadata

shrubs_cacti_succulents_desc <- "Biovolume and characteristics of shrubs and succulent plants within parcel; up to five plants of each type are surveyed."

factorsToFrame(shrubs_cacti_succulents)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
shrubs_cacti_succulents_DT <- createDTFF(dfname = shrubs_cacti_succulents,
                                         description = shrubs_cacti_succulents_desc,
                                         dateRangeField = 'sample_date')

# trees -------------------------------------------------------------------

trees <- get_trees('parcel') %>% 
  mutate(
    vegetation_shape_code = as.factor(vegetation_shape_code),
    vegetation_classification_code = as.factor(vegetation_classification_code),
    canopy_condition = tolower(canopy_condition),
    canopy_condition = as.factor(canopy_condition)
  )

# empty strings to NA (skip the sample_date field [1])
trees[,-1][trees[,-1] == ''] <- NA
# shrubs_cacti_succulents[,-1][shrubs_cacti_succulents[,-1] == ' '] <- NA

# review unique values
unique_values(trees)

writeAttributes(trees) # write data frame attributes to a csv in current dir to edit metadata

trees_desc <- "Size and characteristics of trees and Saguaros (Carnegiea gigantea) within parcels. Unlike shrubs and smaller plants where the characteristics of up to five individuals of each type are measured, characteristics of all trees and Saguaros in a parcel are assessed. As such, these data also reflect the total number of trees and Saguaros in the parcel. An exception to this is seedlings, where trees less than 1 meter in height are simply counted (and not measured); counts of these plants are available in the number_perennials data that is part of this data set."

factorsToFrame(trees)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
trees_DT <- createDTFF(dfname = trees,
                       description = trees_desc,
                       dateRangeField = 'sample_date')


# number_perennials -------------------------------------------------------

number_perennials <- get_number_perennials('parcel') 

# empty strings to NA (skip the sample_date field [1])
number_perennials[,-1][number_perennials[,-1] == ''] <- NA
# shrubs_cacti_succulents[,-1][shrubs_cacti_succulents[,-1] == ' '] <- NA

# review unique values
unique_values(number_perennials)

writeAttributes(number_perennials) # write data frame attributes to a csv in current dir to edit metadata

number_perennials_desc <- "The number of perennial plants in the parcel, except Saguaros and mature trees, the numbers of which are available in the trees data that are part of this data set."

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
number_perennials_DT <- createDTFF(dfname = number_perennials,
                                   description = number_perennials_desc,
                                   dateRangeField = 'sample_date')


# hedges ------------------------------------------------------------------

# There are not any parcel hedges so this code is commented out but left in
# place in case they are added in a future survey

# hedges <- get_hedges('parcel') %>% 
#   mutate(
#     vegetation_shape_code = as.factor(vegetation_shape_code),
#     hedge_condition = tolower(hedge_condition),
#     hedge_condition = as.factor(hedge_condition)
#   )
# 
# # empty strings to NA (skip the sample_date field [1])
# hedges[,-1][hedges[,-1] == ''] <- NA
# hedges[,-1][hedges[,-1] == ' '] <- NA
# 
# # review unique values
# unique_values(hedges)
# 
# writeAttributes(hedges) # write data frame attributes to a csv in current dir to edit metadata
# 
# hedges_desc <- "Biovolume and characteristics of hedges within study plots; up to five hedges of each type are surveyed."
# 
# factorsToFrame(hedges)
# 
# # create data table based on metadata provided in the companion csv
# # use createdataTableFn() if attributes and classes are to be passed directly
# hedges_DT <- createDTFF(dfname = hedges,
#                         description = hedges_desc,
#                         dateRangeField = 'sample_date')


# landuse -----------------------------------------------------------------

landuse <- get_landuse('parcel')

# empty strings to NA (skip the sample_date field [1])
landuse[,-1][landuse[,-1] == ''] <- NA

# review unique values
unique_values(landuse)

writeAttributes(landuse) # write data frame attributes to a csv in current dir to edit metadata

landuse_desc <- "Estimated land use or land cover in the parcel. Parcels may consist of multiple land use or land cover types, with the approximate percentage of each estimated. Land use or land cover types/labels are adapted from land use/land cover designations employed by the Maricopa County Association of Governments."

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
landuse_DT <- createDTFF(dfname = landuse,
                         description = landuse_desc,
                         dateRangeField = 'sample_date')

# neighborhood_characteristics --------------------------------------------

neighborhood_characteristics <- get_neighborhood_characteristics('parcel') %>%
  mutate(
    neigh_social_class_poor = as.factor(neigh_social_class_poor),
    neigh_social_class_rich = as.factor(neigh_social_class_rich),
    neigh_social_class_upper_middle = as.factor(neigh_social_class_upper_middle),
    neigh_social_class_working_lower = as.factor(neigh_social_class_working_lower),
    neigh_buildings_residential = as.factor(neigh_buildings_residential),
    neigh_buildings_commercial = as.factor(neigh_buildings_commercial),
    neigh_buildings_institutional = as.factor(neigh_buildings_institutional),
    neigh_buildings_industrial = as.factor(neigh_buildings_industrial),
    neigh_residence_apartments = as.factor(neigh_residence_apartments),
    neigh_residence_multi_family = as.factor(neigh_residence_multi_family),
    neigh_residence_single_family = as.factor(neigh_residence_single_family),
    neigh_irrigation_drip_trickle = as.factor(neigh_irrigation_drip_trickle),
    neigh_irrigation_flood_hand = as.factor(neigh_irrigation_flood_hand),
    neigh_irrigation_overhead_spray = as.factor(neigh_irrigation_overhead_spray),
    neigh_yard_upkeep_good = as.factor(neigh_yard_upkeep_good),
    neigh_yard_upkeep_poor = as.factor(neigh_yard_upkeep_poor),
    neigh_yard_upkeep_professionally_maintained = as.factor(neigh_yard_upkeep_professionally_maintained),
    # neigh_landscape_mesic = as.factor(neigh_landscape_mesic),
    # neigh_landscape_mixed = as.factor(neigh_landscape_mixed),
    # neigh_landscape_xeric = as.factor(neigh_landscape_xeric),
    neigh_landscape_turf_present = as.factor(neigh_landscape_turf_present) #,
    # neigh_traffic_collector_street = as.factor(neigh_traffic_collector_street),
    # neigh_traffic_cul_de_sac = as.factor(neigh_traffic_cul_de_sac),
    # neigh_traffic_dirt_road = as.factor(neigh_traffic_dirt_road),
    # neigh_traffic_freeway_expressway = as.factor(neigh_traffic_freeway_expressway),
    # neigh_traffic_highway = as.factor(neigh_traffic_highway),
    # neigh_traffic_major_city_road = as.factor(neigh_traffic_major_city_road),
    # neigh_traffic_none = as.factor(neigh_traffic_none),
    # neigh_traffic_paved_local_street = as.factor(neigh_traffic_paved_local_street)
  )

# empty strings to NA (skip the sample_date field [1])
neighborhood_characteristics[,-1][neighborhood_characteristics[,-1] == ''] <- NA

# review unique values
unique_values(neighborhood_characteristics)

writeAttributes(neighborhood_characteristics) # write data frame attributes to a csv in current dir to edit metadata

neighborhood_characteristics_desc <- "General characteristics, such as those relating to perceived social class, types of buildings (if present), landscape quality and features, and traffic of the immediate area surrounding the parcel."

factorsToFrame(neighborhood_characteristics)

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
neighborhood_characteristics_DT <- createDTFF(dfname = neighborhood_characteristics,
                                              description = neighborhood_characteristics_desc,
                                              dateRangeField = 'sample_date')


# structures --------------------------------------------------------------

structures <- get_structures('parcel') %>% 
  mutate(structure_use = replace(structure_use, grepl("agave", structure_use, ignore.case = T), 'house'))

# empty strings to NA (skip the sample_date field [1])
structures[,-1][structures[,-1] == ''] <- NA

# review unique values
unique_values(structures)

writeAttributes(structures) # write data frame attributes to a csv in current dir to edit metadata

structures_desc <- "Type and height of any structures within the parcel."

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
structures_DT <- createDTFF(dfname = structures,
                            description = structures_desc,
                            dateRangeField = 'sample_date')


# sampling_events ---------------------------------------------------------

sampling_events <- get_sampling_events('parcel')

# empty strings to NA (skip the sample_date field [1])
sampling_events[,-1][sampling_events[,-1] == ''] <- NA

# review unique values
unique_values(sampling_events)

writeAttributes(sampling_events) # write data frame attributes to a csv in current dir to edit metadata

sampling_events_desc <- "Date of survey and general characteristics of the parcel, including the elevation, slope, a description of the plot, and a description of weather on the date of sampling."

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
sampling_events_DT <- createDTFF(dfname = sampling_events,
                                 description = sampling_events_desc,
                                 dateRangeField = 'sample_date')




# end data processing -----------------------------------------------------




# title and abstract ----
title <- 'Ecological Survey of Central Arizona: a survey of key ecological indicators in parcels of residential areas in the greater Phoenix metropolitan area, ongoing since 2010'

# abstract from file or directly as text
abstract <- as(set_TextType("abstract.md"), "abstract")


# people ----

danChilders <- addCreator('d', 'childers')
nancyGrimm <- addCreator('n', 'grimm')
sharonHall <- addCreator('shar', 'hall')

creators <- c(as(danChilders, 'creator'),
              as(nancyGrimm, 'creator'),
              as(sharonHall, 'creator'))

# Elizabeth Cook had to add her by hand for some reason
stevanEarl <- addMetadataProvider('s', 'earl')
royErickson <- addMetadataProvider('r', 'erickson')
quincyStewart <- addMetadataProvider('q', 'stewart')
sallyWittlinger <- addMetadataProvider('s', 'wittlinger')

metadataProvider <-c(as(stevanEarl, 'metadataProvider'),
                     as(royErickson, 'metadataProvider'),
                     as(quincyStewart, 'metadataProvider'),
                     as(sallyWittlinger, 'metadataProvider'))

# keywords ----

# CAP IRTs for reference: https://sustainability.asu.edu/caplter/research/
# be sure to include these as appropriate

keywordSet <-
  c(new("keywordSet",
        keywordThesaurus = "LTER controlled vocabulary",
        keyword =  c("urban",
                     "trees",
                     "shrubs",
                     "vegetation",
                     "plants",
                     "plant biomass",
                     "plant communities",
                     "plant cover",
                     "plant height",
                     "plant species composition",
                     "plant species",
                     "community composition",
                     "land use",
                     "long term monitoring",
                     "land cover",
                     "buildings")),
    new("keywordSet",
        keywordThesaurus = "LTER core areas",
        keyword =  c("land use and land cover change",
                     "disturbance patterns",
                     "adapting to city life")),
    new("keywordSet",
        keywordThesaurus = "Creator Defined Keyword Set",
        keyword =  c("survey200",
                     "survey 200",
                     "parcel",
                     "phoenix",
                     "maricopa county",
                     "ecological survey of central arizona",
                     "suburban",
                     "neighborhood",
                     "yard",
                     "mesic",
                     "xeric",
                     "city")),
    new("keywordSet",
        keywordThesaurus = "CAPLTER Keyword Set List",
        keyword =  c("cap lter",
                     "cap",
                     "caplter",
                     "central arizona phoenix long term ecological research",
                     "arizona",
                     "az",
                     "arid land"))
    )

# methods and coverages ----
# methods <- set_methods("esca_methods.md")
# stole these from knb-lter-653

# if relevant, pulling dates from a DB is nice
# begindate <- dbGetQuery(con, "SELECT MIN(sample_date) AS date FROM database.table;")
# begindate <- begindate$date

begindate <- as.character(min(sampling_events$sample_date))
enddate <- as.character(max(sampling_events$sample_date))
geographicDescription <- "CAP LTER study area"
coverage <- set_coverage(begin = begindate,
                         end = enddate,
                         geographicDescription = geographicDescription,
                         west = -112.783, east = -111.579,
                         north = +33.8267, south = +33.2186)


# see esca_taxonomic_coverage.R in this repo for taxonomic coverage
coverage@taxonomicCoverage <- c(escaTaxa)

# construct the dataset ----

# address, publisher, contact, and rights come from a sourced file

# XML DISTRUBUTION
  xml_url <- new("online",
                 onlineDescription = "CAPLTER Metadata URL",
                 url = paste0("https://sustainability.asu.edu/caplter/data/data-catalog/view/", packageIdent, "/xml/"))
metadata_dist <- new("distribution",
                 online = xml_url)

# DATASET
dataset <- new("dataset",
               title = title,
               creator = creators,
               pubDate = pubDate,
               metadataProvider = metadataProvider,
               # associatedParty = associatedParty,
               intellectualRights = rights,
               abstract = abstract,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               # methods = methods,
               distribution = metadata_dist,
               dataTable = c(parcel_characteristics_DT,
                             human_indicators_DT,
                             landscape_irrigation_DT,
                             shrubs_cacti_succulents_DT,
                             trees_DT,
                             number_perennials_DT,
                             landuse_DT,
                             neighborhood_characteristics_DT,
                             structures_DT,
                             sampling_events_DT))

# ls(pattern= "_DT") # can help to pull out DTs

# assembly line flow that would be good to incorporate - build the list of DTs at creation
# data_tables_stored <- list()
# data_tables_stored[[i]] <- data_table
# dataset@dataTable <- new("ListOfdataTable",
#                      data_tables_stored)

# construct the eml ----

# ACCESS
allow_cap <- new("allow",
                 principal = "uid=CAP,o=LTER,dc=ecoinformatics,dc=org",
                 permission = "all")
allow_public <- new("allow",
                    principal = "public",
                    permission = "read")
lter_access <- new("access",
                   authSystem = "knb",
                   order = "allowFirst",
                   scope = "document",
                   allow = c(allow_cap,
                             allow_public))

# CUSTOM UNITS
# standardUnits <- get_unitList()
# unique(standardUnits$unitTypes$id) # unique unit types

custom_units <- rbind(
  data.frame(id = "voltSecond",
             unitType = "unknown",
             parentSI = "unknown",
             multiplierToSI = "unknown",
             description = "Lachat output area under the curve as volts*second")
# data.frame(id = "nephelometricTurbidityUnit",
#            unitType = "unknown",
#            parentSI = "unknown",
#            multiplierToSI = 1,
#            description = "(NTU) ratio of the amount of light transmitted straight through a water sample with the amount scattered at an angle of 90 degrees to one side")
)
unitList <- set_unitList(custom_units)

# note schemaLocation is new, not yet tried!
eml <- new("eml",
           schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
           packageId = packageIdent,
           scope = "system",
           system = "knb",
           access = lter_access,
           dataset = dataset)
           # additionalMetadata = as(unitList, "additionalMetadata"))


# write the xml to file   ----
write_eml(eml, "knb-lter-cap.653.1.xml")


# amazon ------------------------------------------------------------------

# data file to S3
dataToAmz <- function(fileToUpload) {
  
  put_object(file = fileToUpload,
             object = paste0('/datasets/cap/', basename(fileToUpload)),
             bucket = 'gios-data') 
  
}

# example
dataToAmz('653_parcel_characteristics_f5c0678aa01036e86cb7fb56227ac15d.csv')
dataToAmz('653_human_indicators_c968d0c7398e98a9271c809cea18abd6.csv')
dataToAmz('653_landscape_irrigation_35a9d7fa8daadb5aa7fe47f5a17ac6a8.csv')
dataToAmz('653_shrubs_cacti_succulents_253d88e8880ed53e660d47c85e4ad7b1.csv')
dataToAmz('653_trees_a686b035fedfd297dc90b5fe3e259aad.csv')
dataToAmz('653_number_perennials_b6eee68753731513651d5cf409e60b30.csv')
dataToAmz('653_landuse_ad137b437868606df296e98fa1c68b73.csv')
dataToAmz('653_neighborhood_characteristics_218993658e95b51b42203d033a913211.csv')
dataToAmz('653_structures_5258dd2dede9da7c79272c951eb5c274.csv')
dataToAmz('653_sampling_events_ac1700b99889c774f06f1351d765c675.csv')


# metadata file to S3
emlToAmz <- function(fileToUpload) {
  
  put_object(file = fileToUpload,
             object = paste0('/metadata/', basename(fileToUpload)),
             bucket = 'gios-data') 
  
}

# example
emlToAmz('~/localRepos/cap-data/cap-data-eml/knb-lter-cap.653.1.xml')
