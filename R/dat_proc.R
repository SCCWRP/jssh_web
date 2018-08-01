######
# processing JSSH (juvenile steelhead and stream habitat) survey data
# from Austin Robey, https://www.arcgis.com/home/item.html?id=6d9b81bf5b2b4b309f5ff097e0edcfaa#overview

library(tidyverse)
library(readxl)
library(raster)
library(maptools)
library(rgdal)
library(sf)
library(rmapshaper)
library(proj4shortcut)
library(MuMIn)
library(nlme)

prj <- geo_wgs84

dsn <- 'L:/Santa Cruz_fish trends_MB/Data/RawData/Steelhead_Monitoring_Data/2b6251fed87a403f880eb87ee4ed0951.gdb'
# ogrListLayers(dsn)

##
# steelhead annual site data

fishdat <- readOGR(dsn = dsn, layer = 'Site_Annual_Data') %>% 
  spTransform(prj) %>% 
  st_as_sf

save(fishdat, file = 'data/fishdat.RData')

##
# habitat 

data(fishdat)

# get coords, referenced by SiteID, to join
SiteIDloc <- fishdat %>% 
  dplyr::select(SiteID, Watershed) %>% 
  mutate(SiteID = as.character(SiteID))
crds <- st_coordinates(SiteIDloc) %>% 
  as.tibble
st_geometry(SiteIDloc) <- NULL
SiteIDloc <- SiteIDloc %>%
  bind_cols(crds) %>% 
  unique %>% 
  group_by(SiteID, Watershed) %>% 
  summarise(
    X = mean(X, na.rm = T), 
    Y = mean(Y, na.rm = T)
    ) %>% 
  ungroup

habitat <- sf::st_read(dsn = dsn, layer = 'Tb_Habitat') %>% 
  mutate(SiteID = as.character(SiteID)) %>% 
  dplyr::select(-SiteYearID, -StnNumStrt, -StnNumEnd, -HabDetail, -AssessDate) %>% 
  gather('habvar', 'habval', -Year, -SiteID, -HabType) %>% 
  group_by(Year, SiteID, HabType, habvar) %>% 
  summarise(habval = mean(habval, na.rm = T)) %>% 
  left_join(., SiteIDloc, by = 'SiteID') %>% 
  filter(!is.na(X)) %>% 
  st_as_sf(coords = c('X', 'Y'), crs = st_crs(fishdat)) %>% 
  ungroup

save(habitat, file = 'data/habitat.RData')

## 
# all streams, only useful for mapping

stream <- readOGR(dsn = dsn, layer = 'Stream') %>% 
  spTransform(prj) %>% 
  st_as_sf

save(stream, file = 'data/stream.RData')
  
##
# segments with a monitoring site

segment <- readOGR(dsn = dsn, layer = 'Segment') %>% 
  spTransform(prj) %>% 
  st_as_sf

save(segment, file = 'data/segment.RData')

##
# prepping site, size class density for trend eval

# remove 1981 all watersheds
# remove 1994 from PAJ and SAQ
trndst_prep <- fishdat %>% 
  filter(Year > 1981) %>% 
  filter(!(Year == 1994 & Watershed %in% c('PAJ', 'SOQ')))

# separate crds tibble for later joing
crds <- st_coordinates(trndst_prep) %>% 
  as.tibble
st_geometry(trndst_prep) <- NULL
crds <- trndst_prep %>% 
  dplyr::select(SiteID) %>% 
  bind_cols(crds) %>% 
  unique %>% 
  group_by(SiteID) %>% 
  summarize(
    X = mean(X), 
    Y = mean(Y)
  ) %>% 
  ungroup

# get trends by station, size class
trndst_prep <- trndst_prep %>% 
  dplyr::select(Year, SiteID, Dens_S1, Dens_S2) %>% 
  gather('Size class', 'density', Dens_S1, Dens_S2) %>% 
  mutate(density = log10(1 + density)) %>% 
  group_by(`Size class`, SiteID) %>% 
  nest %>% 
  left_join(crds, by = 'SiteID')

save(trndst_prep, file = 'data/trndst_prep.RData', compress = 'xz')

##
# prep habitat site data for trends

data(habitat)

# prep habitat data, long format 
trndhab_prep <- habitat

# separate crds tibble for later joing
crds <- st_coordinates(trndhab_prep) %>% 
  as.tibble
st_geometry(trndhab_prep) <- NULL
crds <- trndhab_prep %>% 
  dplyr::select(SiteID) %>% 
  bind_cols(crds) %>% 
  unique %>% 
  group_by(SiteID) %>% 
  summarize(
    X = mean(X), 
    Y = mean(Y)
  ) %>% 
  ungroup

# get trends by station, size class
trndhab_prep <- trndhab_prep %>% 
  select(-Watershed) %>% 
  filter(HabType %in% c('run', 'riffle', 'pool')) %>% 
  group_by(SiteID, HabType, habvar) %>% 
  nest %>% 
  left_join(crds, by = 'SiteID')

save(trndhab_prep, file = 'data/trndhab_prep.RData', compress = 'xz')

######
# get allfctpers, a nested list of models for watershed, habitat type, density measure comparisons of density changes
# by year and selected habitat variable.  models are pairwise evaluations of year plus a habitat variable.  includes 
# only those where a variable other than or in addition to year was significant.  Site values for density and habitat
# were averaged within years for each watershed
 
data(fishdat)
data(habitat)

# combined salmonid, habitat data, only where years intersect
saldat <- fishdat
st_geometry(saldat) <- NULL
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
  unnest %>% 
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
      frm <- paste0('log10(', densvar, ') ~ Year*', habvar) %>%
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

######
# alltops, linear mixed models of salmonid density by habitat variables and year
# each watershed, habitat type group is modelled separately, station id is a random effect
# all combinations of habitat variables are tested, top five with minimum AIC are selected

data(fishdat)
data(habitat)

# combined salmonid, habitat data, only where years intersect
saldat <- fishdat
st_geometry(saldat) <- NULL
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

# habitat combinations to model, year is always included
cmbs <- habitat %>% 
  filter(!habvar %in% 'StnSthd') %>% 
  pull(habvar) %>% 
  unique
cmbs <- map(1:length(cmbs), ~ combn(cmbs, m = .x, simplify = F)) %>% 
  do.call('c', .) %>% 
  map(paste, collapse = ' + ') %>% 
  map(~ paste0('Year + ', .x)) %>% 
  unlist

# all models, filtered by top five AIC within each watershed, density class, and habitat type groups
alltops <- inner_join(saldat, habdat, by = c('Year', 'SiteID')) %>% 
  unnest %>% 
  spread(habvar, habval) %>% 
  gather('densvar', 'densval', Dens_S1, Dens_S2) %>% 
  group_by(Watershed, HabType, densvar) %>% 
  nest %>% 
  crossing(cmbs) %>% 
  mutate(
    mods = pmap(list(densvar, cmbs, data), function(densvar, cmbs, data){
      
      names(data)[names(data) %in% 'densval'] <- densvar
      
      # formula as text
      frm <- paste0('log10(1 + ', densvar, ') ~ ', cmbs) 

      # model as text
      tomod <- paste0('lme(', frm, ', random = ~1 |SiteID, data = na.omit(data))')
      
      # parse to mod
      modout <- try({eval(parse(text = tomod))})
      
      if(inherits(modout, 'try-error')) 
        return(NA)
      
      return(modout)
      
    }), 
    modaic = map(mods, function(x) ifelse(anyNA(x), x, AIC(x)))
  ) %>% 
  unnest(modaic) %>% 
  dplyr::select(-data) %>% 
  group_by(Watershed, HabType, densvar) %>% 
  filter(!is.na(modaic)) %>%
  top_n(-5) %>% 
  arrange(modaic) %>% 
  ungroup

save(alltops, file = 'data/alltops.RData', compress = 'xz')
