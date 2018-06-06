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

habitat <- sf::st_read(dsn = dsn, layer = 'Tb_Habitat')

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

