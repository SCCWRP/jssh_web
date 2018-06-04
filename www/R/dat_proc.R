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
