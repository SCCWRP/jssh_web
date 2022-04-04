# The reason this file is called Arc Dat proc is because it is done with data from Arc Online (or Santa Cruz GIS portal server's REST service or something like that)
# Either way the source of raw data is a more dynamic source rather than static files, with the exception of habitat
# It seems like the habitat data is not truly updated in the REST service
# Worst case, the data can be found here
# https://www.co.santa-cruz.ca.us/Departments/GeographicInformationSystems(GIS).aspx
# you have to click the tile that says "Data" and search for "Steelhead Monioring Data"
# There are 6 layers:
# Site, Site Annual Data, Segment, Reach, Stream, and Habitat
# I was able to pull these from the REST service with the exception of Habitat.
# The REST service URL is this: https://services1.arcgis.com/jJfZghspGKh8J9Jm/arcgis/rest/services/JSSH/FeatureServer

# if trying to pull from the REST service doesnt work, one may go to their website and search and manually download the data as csv files, or shapefiles

library(httr)
library(sf)
library(tmap)
library(arcpullr)
library(glue)
library(tidyverse)
library(readxl)
library(raster)
library(maptools)
library(rgdal)
library(rmapshaper)
#library(proj4shortcut)
library(MuMIn)
library(nlme)
library(lubridate)
#prj <- geo_wgs84

sitedata2022 <- read.csv('data/SiteDataJSSH2022.csv')
siteannual2022 <- read.csv('data/SiteAnnualDataJSSH2022.csv')
reach2022 <- read.csv('data/ReachDataJSSH2022.csv')
stream2022 <- read.csv('data/StreamDataJSSH2022.csv')
habitat2022 <- read.csv('data/HabitatDataJSSH2022.csv')
segment2022 <- read.csv('data/SegmentDataJSSH2022.csv')



# Trying to match the naming that Marcus used in the original dat_proc script
fishdat <- siteannual2022 %>%
  # Below modification to the SiteAnnualData is based on what i see in dat_proc
  mutate(
    Watershed = as.character(Watershed), 
    Watershed = case_when(
      grepl('^SLR-main', SiteID) ~ 'SLR-main',
      Watershed %in% c('SLR', 'San Lorenzo') ~ 'SLR-trib',
      Watershed %in% c('APT', 'Aptos') ~ 'APT', 
      Watershed %in% c('PAJ', 'Pajaro') ~ 'PAJ', 
      Watershed %in% c('SOQ', 'Soquel') ~ 'SOQ'
    ),
    Watershed = factor(Watershed, levels = c('SLR-main', 'SLR-trib', 'SOQ', 'APT', 'PAJ')),
    SampleDate = as.Date(gsub('00:00:00$', '', SampleDate)),
    Year = ifelse(is.na(Year), year(SampleDate), Year)
  ) %>% 
  dplyr::select(-SiteYearID, -Dry_rating, -DensTtl, -Dens_A1, -Dens_A2, -YearLabel, -GlobalID, -Sp_Dwarf_SP, -Sp_CbznScp, -SiteSort) 
  #%>%
  #st_as_sf(coords=c('X','Y'))

trndst_prep <- fishdat %>% 
  filter(Year > 1981) %>% 
  filter(!(Year == 1994 & Watershed %in% c('PAJ', 'SOQ')))

# separate crds tibble for later joining
crds <- fishdat %>% dplyr::select('X','Y','SiteID') %>% distinct() 

# get trends by station, size class
trndst_prep <- trndst_prep %>% 
  dplyr::select(Year, SiteID, Dens_S1, Dens_S2) %>% 
  gather('Size class', 'density', Dens_S1, Dens_S2) %>% 
  mutate(density = log10(1 + density)) %>% 
  group_by(`Size class`, SiteID) %>% 
  nest %>% 
  left_join(crds, by = 'SiteID')



habnew <- habitat2022 %>% 
  dplyr::select(-SiteYearID, -StnNumStrt, -StnNumEnd, -HabDetail, -AssessDate, -YearLabel, -GlobalID, -OBJECTID) %>%
  left_join(
    fishdat_new %>% dplyr::select(SiteID, Year, X, Y, Watershed) %>% distinct, 
    by = c('SiteID','Year')
  )

habnew <- habnew %>% 
  pivot_longer(
    as.vector( names(habnew)[ which( !names(habnew) %in% c('SiteID', 'Year', 'X', 'Y', 'HabType', 'Watershed') ) ] ), 
    names_to = 'habvar', 
    values_to = 'habval'
  ) %>%
  filter(!is.na(X)) %>%
  filter(!is.na(Y)) %>%
  st_as_sf(coords=c('X','Y'))

data(fishdat)
data(habitat)























