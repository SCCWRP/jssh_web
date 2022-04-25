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


# ----------------------------------------------------------- fishdat ---------------------------------------------------------------
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



# separate crds tibble for later joining
crds <- fishdat %>% dplyr::select('X','Y','SiteID') %>% distinct() 


# --------------------------------------------------------- trndst_prep ---------------------------------------------------------------
trndst_prep <- fishdat %>% 
  filter(Year > 1981) %>% 
  filter(!(Year == 1994 & Watershed %in% c('PAJ', 'SOQ')))



# get trends by station, size class
trndst_prep <- trndst_prep %>% 
  dplyr::select(Year, SiteID, Dens_S1, Dens_S2) %>% 
  gather('Size class', 'density', Dens_S1, Dens_S2) %>% 
  mutate(density = log10(1 + density)) %>% 
  group_by(`Size class`, SiteID) %>% 
  nest %>% 
  left_join(crds, by = 'SiteID')

save(trndst_prep, file = 'data/trndst_prep.RData', compress = 'xz')


# ---------------------------------------------------------- habitat aka habnew ---------------------------------------------------------------
habitat <- habitat2022 %>% 
  dplyr::select(-SiteYearID, -StnNumStrt, -StnNumEnd, -HabDetail, -AssessDate, -YearLabel, -GlobalID, -OBJECTID) %>%
  left_join(
    fishdat %>% dplyr::select(SiteID, Year, X, Y, Watershed) %>% distinct, 
    by = c('SiteID','Year')
  )

habitat <- habitat %>% 
  pivot_longer(
    as.vector( names(habitat)[ which( !names(habitat) %in% c('SiteID', 'Year', 'X', 'Y', 'HabType', 'Watershed') ) ] ), 
    names_to = 'habvar', 
    values_to = 'habval'
  ) %>%
  filter(!is.na(X)) %>%
  filter(!is.na(Y)) %>%
  st_as_sf(coords=c('X','Y'))

save(habitat, file = 'data/habitat.RData')

# ------------------------------------------------------------------ trndhab_prep ------------------------------------------------------------

# prep habitat site data for trends


# prep habitat data, long format 
trndhab_prep <- habitat



# get trends by station, size class
trndhab_prep <- trndhab_prep %>% 
  dplyr::select(-Watershed) %>% 
  filter(HabType %in% c('run', 'riffle', 'pool')) %>% 
  group_by(SiteID, HabType, habvar) %>% 
  nest %>% 
  left_join(crds, by = 'SiteID')

save(trndhab_prep, file = 'data/trndhab_prep.RData', compress = 'xz')

# ----------------------------------------------------------- allfctprs -----------------------------------------------------------------------

# combined salmonid, habitat data, only where years intersect
saldat <- fishdat
#st_geometry(saldat) <- NULL
saldat <- saldat %>% 
  dplyr::select(Year, SiteID, Watershed, Dens_S1, Dens_S2) %>% 
  group_by(Year, SiteID, Watershed) %>% 
  summarise(
    Dens_S1 = mean(Dens_S1, na.rm = T), 
    Dens_S2 = mean(Dens_S2, na.rm = T)
  ) %>% 
  ungroup %>% 
  mutate(SiteID = as.character(SiteID))

habdat <- habitat
st_geometry(habdat) <- NULL
habdat <- habdat %>% 
  dplyr::select(-Watershed) %>% 
  group_by(Year, SiteID, HabType) %>% 
  filter(!habvar %in% 'StnSthd') %>% 
  nest %>% 
  filter(HabType %in% c('pool', 'riffle', 'run'))

# combind
dat <- inner_join(saldat, habdat, by = c('Year', 'SiteID'))

# habitat variables to select, master obj
habvrs <- list(
  'Canopy cover (%)' = 'StnCan',
  'Deciduous canopy cover (%)' = 'StnDecid', 
  'Average depth (ft)' = 'StnDpthAvg',
  'Maximum depth (ft)' = 'StnDpthMax',
  'Embeddedness (%)' = 'StnEmbed',
  'Escape cover (ratio)' = 'StnEsCov',
  'Fines (%)' = 'StnFines',
  'Station length (ft)' = 'StnLgth',
  'Station width (ft)' = 'StnWdth'
)

# get the object
allfctprs <- dat %>% 
  unnest(data) %>% 
  group_by(Year, Watershed, HabType, habvar) %>% 
  summarise(
    Dens_S1 = mean(Dens_S1, na.rm = T), 
    Dens_S2 = mean(Dens_S2, na.rm = T), 
    habval = mean(habval, na.rm = T)
  ) %>% 
  gather('densvar', 'densval', Dens_S1, Dens_S2) %>% 
  group_by(Watershed, HabType, densvar, habvar) %>% 
  nest %>% 
  mutate(
    modsel = pmap(list(densvar, habvar, data), function(densvar, habvar, data){
      
      # format data with names
      names(data)[names(data) %in% 'habval'] <- habvar
      names(data)[names(data) %in% 'densval'] <- densvar
      data <- na.omit(data)
      
      # formula
      frm <- paste0('log10(1 + ', densvar, ') ~ Year +', habvar) %>%
        formula
      
      # global
      modoutglo <- lm(frm, data = data, na.action = na.pass)
      
      # selected, summary
      modoutsel <-  modoutglo %>%
        dredge %>%
        get.models(subset = 1) %>%
        .[[1]]
      
      return(modoutsel)
      
    }), 
    modcof = map(modsel, coefficients),
    modimp = map(modsel, function(modsel){
      
      cof <- modsel %>% 
        summary %>% 
        coefficients %>% 
        rownames
      
      out <- F
      if(any(!cof %in% c('(Intercept)', 'Year'))) out <- T
      
      return(out)
      
    })
    
  ) %>% 
  unnest(modimp) %>% 
  filter(modimp) %>% 
  dplyr::select(-data, -habvar, -modcof, -modimp) %>% 
  arrange(densvar, Watershed, HabType)

save(allfctprs, file = 'data/allfctprs.RData', compress = 'xz')

fishdat <- fishdat %>%
  dplyr::select(-OBJECTID) %>%
  dplyr::mutate(SiteID = as.factor(SiteID)) %>%
  st_as_sf(coords=c('X','Y'))

save(fishdat, file = 'data/fishdat.RData')


# -------------------------------------------------  floest and fishmtch ------------------------------------------------------------ 
# This is basically how floest is built
# library(readxl)
# library(tidyverse)
# 
# flo <- read_excel("~/Desktop/JSSH/floest_wide.xlsx")
# 
# floest <- flo %>% 
#   pivot_longer(names(flo)[which(!names(flo) %in% c('Site', 'month') )], names_to = 'year',values_to = 'flo') %>% 
#   mutate(date = glue::glue('{month}/15/{year}') %>% as.Date(format = '%m/%d/%Y')) %>%
#   select(Site, date, flo)
# 
# save(floest, file = 'data/floest.RData')

# That code was kept in a separate script. The flow estimate data was sent from 
# c.hammersmark@cbecoeng.com, g.downs@cbecoeng.com, and Kristen from Santa Cruz County kristen.kittleson@santacruzcounty.us
# The person to contact would probably be Kristen with Chris and Gavin CC'd on the email.
# Chris and Gavin put together the flow estimates using linear regression models and send massive excel files that would not be good
# to keep in the repository. Essentially there is that results sheet (June_Results or September_Results) in the excel file with the site, and the recommended gauge (Big trees or Soquel)
# and the flow estimate values from both Big trees and Soquel. you have to select the values from the one they recommend you to use, which is also
# in that same excel file. There are two excel files, one for september and one for June
# Basically after seeing the excel file it should be apparent how to build floest from that. I had to manipulate the excel file from Chris and Gavin
# To get the data in long format, which is what the app wants. Bascially they have it such that the columns are the years, and the values in the columns
# are the actual data values, which is why i had to do a pivot_longer on it
# also you can see i put the date to be the 15th of the month (6/15/YYYY etc) this is because the value is an estimate for the flow for the month, so
# we just arbitralily choose the middle of the month for the date
# The day itself is actually not so important. just the month and year
# To future me, or whoever takes over this project, i hope this helps

# for the fishmtch dataframe, according to Marcus' original "dat_proc" script, it is just a lookup table. I doubt it needs any updating, so this time around
# I didnt do anything with that one

# Robert Butler (robertb@sccwrp.org) April 25, 2022





