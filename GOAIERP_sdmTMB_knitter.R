#This document puts the GOAIERP Surface Trawl Survey data finto the same format that we use for RACE and DFO data, 
#with the aim of using the same sdmTMB workflow on these.
#The code then knits the markdown containing the sdmTMB workflow, plots and diagnostics
#It is based on CPUE data from Jamal

library(sdmTMB)
library(rbgm)
library(viridis)
library(kableExtra)
library(tidyverse)
library(sf)
library(data.table)
library(maps)
library(mapdata)
library(lubridate)

select <- dplyr::select

# Settings for sdmTMB.
cutoff <- 20

# #Workflow:

# 1. read all data as modified in the exploratory script
# 2. assume that there are no other hauls than those that some catch is reported for, and get levels with hauljoin
# 3. pick a species / stage
# 4. attach (rbind) empty hauls and make the CPUE 0 for those
# 5. pipe it to the sdmTMB script

# import data
load("data/goaierp_data.Rdata")

# change from species to Atlantis groups
goaierp_to_ag <- read.csv('data/goaierp_species.csv', header=TRUE)
goaierp_to_ag <- goaierp_to_ag %>% filter(Atlantis.group!='?' & !is.na(Atlantis.group))

goaierp_data <- goaierp_data %>% left_join(goaierp_to_ag, by='CommonName')

# add within haul by group and life stage
goaierp_data <- goaierp_data %>% group_by(Year,HaulJoin,EQLatitude,EQLongitude,Atlantis.group,LHSCode) %>% 
  summarise(CPUE_kg_km2=sum(CPUE_kg_km2), CPUE_num_km2=sum(CPUE_num_km2)) %>% ungroup()

# import Atlantis BGM for projection
atlantis_bgm <- read_bgm("data/GOA_WGS84_V4_final.bgm")
atlantis_sf <- atlantis_bgm %>% box_sf()
atlantis_crs <- atlantis_bgm$extra$projection
atlantis_bbox <- atlantis_sf %>% st_bbox()

# add coastline 
coast <- map("worldHires", regions = c("USA","Canada"), plot = FALSE, fill = TRUE)
coast_sf <- coast %>% st_as_sf() %>% st_transform(crs=atlantis_crs) %>% st_combine()

# load depth grid and calculate, for each point, distance from shore
load("data/atlantis_grid_depth.Rdata")

atlantis_grid_dist <- atlantis_grid_depth %>% 
  select(-depth,-insideY,-insideX) %>% 
  st_as_sf(coords=c("x","y"), crs = atlantis_crs) %>%
  mutate(distance = as.vector(st_distance(geometry,coast_sf))/1000) %>%
  mutate(lon=st_coordinates(geometry)[,1]/1000,lat=st_coordinates(geometry)[,2]/1000) %>%
  st_set_geometry(NULL)

# turn goaierp data to sf
goaierp_sf <- goaierp_data %>% st_as_sf(coords=c("EQLongitude","EQLatitude"), crs=4326) %>% st_transform(crs=atlantis_crs)

# get distance from shore for each data point, to use as predictor instead of depth
goaierp_sf <- goaierp_sf %>% mutate(Distance.km = as.vector(st_distance(geometry,coast_sf))/1000)

# view
goaierp_sf %>% ggplot()+
  geom_sf(aes(color=Distance.km))+
  geom_sf(data=coast_sf)+
  scale_color_viridis()+
  coord_sf(xlim=c(atlantis_bbox$xmin,atlantis_bbox$xmax),ylim=c(atlantis_bbox$ymin,atlantis_bbox$ymax))+
  facet_wrap(~Year)

# set coordinates in km, instead of m, for numerical stability in sdmTMB. Shift from lat lon to projected coordinates as more appropriate for sdmTMB
goaierp_data <- goaierp_sf %>%
  mutate(EQLongitude=st_coordinates(geometry)[,1]/1000, EQLatitude=st_coordinates(geometry)[,2]/1000) %>%
  st_set_geometry(NULL) %>%
  select(Year,HaulJoin,EQLatitude,EQLongitude,Distance.km,Atlantis.group,LHSCode,CPUE_kg_km2,CPUE_num_km2)

# list of hauls and species
all_hauls <- goaierp_data %>% select(HaulJoin) %>% distinct() %>% pull()
all_groups <- goaierp_data %>% select(Atlantis.group) %>% distinct() %>% pull()

# goaierp_knitter <- function(this_group){
#   data_species <- goaierp_data %>% filter(Atlantis.group == this_group)
#   
#   all_stages <- data_species %>% select(LHSCode) %>% distinct() %>% pull() # stages are different for different species
#   
#   lhs_knitter <- function(this_stage){
#     data_species <- data_species %>% filter(LHSCode == this_stage)
#     this_species_hauls <- data_species %>% select(HaulJoin) %>% distinct() %>% pull()
#     empty_hauls <- setdiff(all_hauls, this_species_hauls)
#     
#     empty_template <- goaierp_data %>% filter(HaulJoin %in% empty_hauls) %>%
#       select(Year,HaulJoin,EQLatitude,EQLongitude,Distance.km,Atlantis.group,LHSCode) %>%
#       mutate(Atlantis.group = NA,
#              LHSCode = NA,
#              CPUE_kg_km2 = 0,
#              CPUE_num_km2 = 0) %>%
#       distinct() %>%
#       select(Year,HaulJoin,EQLatitude,EQLongitude,Distance.km,Atlantis.group,LHSCode,CPUE_kg_km2,CPUE_num_km2)
#     # bind the empty hauls and the catch data for the species
#     data_species <- rbind(data_species, empty_template)
#     
#     # select columns and rename them
#     
#     data_species <- data_species %>% select(Year,HaulJoin,EQLatitude,EQLongitude,Distance.km,Atlantis.group,LHSCode,CPUE_kg_km2,CPUE_num_km2) %>%
#       set_names(c(
#         'year',
#         'hauljoin',
#         'lat',
#         'lon',
#         'distance',
#         'name',
#         'stage',
#         'biom_kgkm2',
#         'num_km2'))
#     
#     # drop NA catches
#     data_species <- data_species %>% filter(!is.na(biom_kgkm2) & !is.na(num_km2))
#     
#     # pass this on to the sdmTMB markdown
#     rmarkdown::render(
#       'GOAIERP_sdmTMB.Rmd', 
#       output_file = paste0('output/', this_group, '_', this_stage, '_', cutoff, '.html')
#     )
#     
#     # apply to all stages for this species
#   }
#   purrr::map(all_stages, possibly(lhs_knitter,NA))
# }
# 
# # apply to all species in the data
# purrr::map(all_groups, possibly(goaierp_knitter,NA))


##########################################################################################################
# same but without life stages

goaierp_knitter_nostage <- function(this_group){
  data_species <- goaierp_data %>% filter(Atlantis.group == this_group)
  
  data_species <- data_species %>% group_by(across(Year:Atlantis.group)) %>% summarise(CPUE_kg_km2=sum(CPUE_kg_km2),
                                                                                       CPUE_num_km2=sum(CPUE_num_km2))
  
  # add empty hauls
  this_species_hauls <- data_species %>% select(HaulJoin) %>% distinct() %>% pull()
  empty_hauls <- setdiff(all_hauls, this_species_hauls)
  
  empty_template <- goaierp_data %>% filter(HaulJoin %in% empty_hauls) %>%
    select(Year,HaulJoin,EQLatitude,EQLongitude,Distance.km,Atlantis.group) %>%
    mutate(Atlantis.group = NA,
           CPUE_kg_km2 = 0,
           CPUE_num_km2 = 0) %>%
    distinct() %>%
    select(Year,HaulJoin,EQLatitude,EQLongitude,Distance.km,Atlantis.group,CPUE_kg_km2,CPUE_num_km2)
  # bind the empty hauls and the catch data for the species
  data_species <- rbind(data_species, empty_template)
  
  # select columns and rename them
  
  data_species <- data_species %>% select(Year,HaulJoin,EQLatitude,EQLongitude,Distance.km,Atlantis.group,CPUE_kg_km2,CPUE_num_km2) %>%
    set_names(c(
      'year',
      'hauljoin',
      'lat',
      'lon',
      'distance',
      'name',
      'biom_kgkm2',
      'num_km2'))
  
  # drop NA catches
  data_species <- data_species %>% filter(!is.na(biom_kgkm2) & !is.na(num_km2))
  
  # pass this on to the sdmTMB markdown
  rmarkdown::render(
    'GOAIERP_sdmTMB_nostages.Rmd', 
    output_file = paste0('output/no_life_stages/', this_group, '_', cutoff, '.html')
  )
  
  # apply to all stages for this species
}

purrr::map(all_groups, possibly(goaierp_knitter_nostage,NA))

