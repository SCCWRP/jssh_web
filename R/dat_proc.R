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
library(lubridate)

prj <- geo_wgs84

# # old gdb
# dsn <- 'L:/Santa Cruz_fish trends_MB/Data/RawData/Steelhead_Monitoring_Data/2b6251fed87a403f880eb87ee4ed0951.gdb'

# new gdb with 2018
dsn <- 'L:/Santa Cruz_fish trends_MB/Data/RawData/SteelheadData2018/062df19c7aa94541b7afca9a7c8bb28d.gdb'

# ogrListLayers(dsn)

# fishdat -----------------------------------------------------------------

##
# steelhead annual site data

fishdat <- readOGR(dsn = dsn, layer = 'Site_Annual_Data') %>% 
  spTransform(prj) %>% 
  st_as_sf %>% 
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
    SampleDate = gsub('00:00:00$', '', SampleDate),
    SampleDate = ymd(SampleDate),
    Year = ifelse(is.na(Year), year(SampleDate), Year)
    ) %>% 
  rename(
    Sp_BayPF = SP_BayPF
  ) %>% 
  dplyr::select(-SiteYearID, -Dry_rating, -DensTtl, -Dens_A1, -Dens_A2, -YearLabel, -GlobalID, -Sp_Dwarf_SP, -Sp_CbznScp, -SiteSort) %>% 
  group_by(Year)

##
# 2019 data update

# get geometry
fishgeo <- read_excel('T:/04_STAFF/MARCUS/02_DOCUMENTS/Santa_Cruz_fish_trends_MB/JSSH Data including Pajaro_2019.xls', 
                  sheet = 'SiteKey') %>% 
  dplyr::select(SiteID, X, Y)

fishnew <- read_excel('T:/04_STAFF/MARCUS/02_DOCUMENTS/Santa_Cruz_fish_trends_MB/JSSH Data including Pajaro_2019.xls', 
                      sheet = 'SiteDensityData') %>% 
  dplyr::select(-Stream, -Dry_rating, -`# STH snorkel`, -`# STH captured`, -`% YOY`, -Ttl_Dens, -YOY_Den, -Yr_Den, 
                -`Black Sal`, -Anchovy, -`Dwarf SP`, -`Cabzn Sculp`) %>% 
  filter(!is.na(SampleDate)) %>% 
  left_join(fishgeo, by = 'SiteID') %>% 
  mutate(
    X = case_when(
      SiteID %in% 'PAJ-corr-0' ~ -121.803495,
      SiteID %in% 'PAJ-cass-3' ~ -121.739362063986, 
      SiteID %in% 'SLR-bean-14c-2' ~ -122.01131693599, 
      T ~ X
    ), 
    Y = case_when(
      SiteID %in% 'PAJ-corr-0' ~ 36.99027747,
      SiteID %in% 'PAJ-cass-3' ~ 36.9883953639739,
      SiteID %in% 'SLR-bean-14c-2' ~ 37.0786908549217, 
      T ~ Y
    )
  ) %>% 
  mutate(
    SiteID = case_when(
      SiteID %in% 'SLR-bean-14c-2' ~ 'SLR-bean-14c2', 
      T ~ SiteID
    ),
    Watershed = as.character(Watershed), 
    Watershed = case_when(
      grepl('^SLR-main', SiteID) ~ 'SLR-main',
      Watershed %in% c('SLR', 'San Lorenzo') ~ 'SLR-trib',
      Watershed %in% c('APT', 'Aptos') ~ 'APT', 
      Watershed %in% c('PAJ', 'Pajaro') ~ 'PAJ', 
      Watershed %in% c('SOQ', 'Soquel') ~ 'SOQ'
    ),
    Watershed = factor(Watershed, levels = c('SLR-main', 'SLR-trib', 'SOQ', 'APT', 'PAJ')),
    SampleDate = as.Date(SampleDate, origin = "1899-12-30"),
    Year = year(SampleDate)
  ) %>% 
  rename(
    Dens_S1 = Sz1_Den, 
    Dens_S2 = Sz2_Den,
    SthdRT = STHRT, 
    Tp_Smlt = Smlt
  ) %>% 
  rename_at(vars(-c(Watershed, SiteID, SampleDate, Dens_S1, Dens_S2, Year, X, Y)), list(~paste0('Sp_', .))) %>% 
  mutate_at(vars(matches('^SP')), list(~ifelse(. %in% 'yes', 1, 0))) %>% 
  st_as_sf(coords = c('X', 'Y'), crs = prj)

# combine with old fishdat
fishdat <- rbind(fishdat, fishnew)

save(fishdat, file = 'data/fishdat.RData')

# habitat -----------------------------------------------------------------

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

habitat <- sf::st_read(dsn = dsn, layer = 'Habitat') %>% 
  mutate(SiteID = as.character(SiteID)) %>% 
  dplyr::select(-SiteYearID, -StnNumStrt, -StnNumEnd, -HabDetail, -AssessDate, -YearLabel, -GlobalID) %>% 
  gather('habvar', 'habval', -Year, -SiteID, -HabType) %>% 
  group_by(Year, SiteID, HabType, habvar) %>% 
  summarise(habval = mean(habval, na.rm = T)) %>% 
  left_join(., SiteIDloc, by = 'SiteID') %>% 
  filter(!is.na(X)) %>% 
  st_as_sf(coords = c('X', 'Y'), crs = st_crs(fishdat)) %>% 
  ungroup

##
# update with 2019 data

habnew <- read_excel('T:/04_STAFF/MARCUS/02_DOCUMENTS/Santa_Cruz_fish_trends_MB/JSSH Data including Pajaro_2019.xls', 
                      sheet = 'StationHabitatData', na = 'na') %>% 
  dplyr::select(-StnNumStrt, -StnNumEnd, -HabType2, -HabDetail) %>% 
  rename(
    SiteID = `SiteID *`,
    SampleDate = AssessDate
    ) %>% 
  filter(!is.na(SampleDate)) %>%
  filter(!is.na(HabType)) %>% 
  mutate(
    SiteID = case_when(
      SiteID %in% 'SLR-bean-14c-2' ~ 'SLR-bean-14c2', 
      T ~ SiteID
    ),
    SampleDate = as.Date(SampleDate, origin = "1899-12-30"),
    Year = year(SampleDate)
  ) %>% 
  gather('habvar', 'habval', -Year, -SiteID, -HabType, -SampleDate) %>% 
  group_by(Year, SiteID, HabType, habvar) %>% 
  summarise(habval = mean(habval, na.rm = T)) %>% 
  left_join(SiteIDloc, by = 'SiteID') %>% 
  st_as_sf(coords = c('X', 'Y'), crs = prj) %>% 
  ungroup

# join with habitat
habitat <- rbind(habitat, habnew)

save(habitat, file = 'data/habitat.RData')

# stream ------------------------------------------------------------------

## 
# all streams, only useful for mapping

stream <- readOGR(dsn = dsn, layer = 'Stream') %>% 
  spTransform(prj) %>% 
  st_as_sf

save(stream, file = 'data/stream.RData')

# trndst_prep -------------------------------------------------------------

##
# prepping site, size class density for trend eval

# remove 1981 all watersheds
# remove 1994 from PAJ and SAQ
trndst_prep <- fishdat %>% 
  filter(Year > 1981) %>% 
  filter(!(Year == 1994 & Watershed %in% c('PAJ', 'SOQ')))

# separate crds tibble for later joing
crds <- st_coordinates(trndst_prep) %>% 
  as_tibble
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

# trndhab_prep ------------------------------------------------------------

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
  dplyr::select(-Watershed) %>% 
  filter(HabType %in% c('run', 'riffle', 'pool')) %>% 
  group_by(SiteID, HabType, habvar) %>% 
  nest %>% 
  left_join(crds, by = 'SiteID')

save(trndhab_prep, file = 'data/trndhab_prep.RData', compress = 'xz')

# allfctpers --------------------------------------------------------------

##
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

# rchdat ------------------------------------------------------------------

##
# 1/2 mile reach segment habitat data
# reach shapefile data

# old stream names for habitat data
oldstr <- c("Aptos", "Bean Creek", "Bear Creek", "Boulder Creek", "Branciforte Creek", 
  "Browns", "Carbonera Creek", "Casserly", "Corralitos", "Fall Creek", 
  "Lompico Creek", "Newell Creek", "Shingle Mill", "SLR", "SLR- Waterman Gap", 
  "SLR Waterman Gap", "Soquel", "Valencia", "Zayante", "Zayante Creek"
)

# new stream names for habitat data, created to match with reach sf object
newstr <- c("main", "bean", "bear", "bldr", "bran", "brwn", "carb", "cass", "corr", 
  "fall", "lomp", "newl", "shng", "main", "main", "main", "main", "main", "zayt", "zayt")

# reaches on SLR main
slrmain <- as.character(c(seq(0, 12), '12a', '12b'))

# habitat level codes
habcodes <- read_excel('../../Data/RawData/HABITAT_SEG_MASTER_2006-2018_narcodes.xlsx',
                       sheet = 'Sheet3'
                       ) %>% 
  rename(
    level4 = UNIT, 
    level1 = CODE_LVL1, 
    level2 = CODE_LVL2, 
    level3 = CODE_LVL3, 
    `HAB abbrev` = CODE_LV4
  ) %>% 
  mutate(
    level4 = gsub('\\s\\(.*$', '', level4), 
    level1 = tools::toTitleCase(tolower(level1)),
    level2 = tools::toTitleCase(tolower(level2)),
    level3 = tools::toTitleCase(tolower(level3))
  ) %>% 
  dplyr::select(`HAB abbrev`, level4)

# format reach hab data
rchdat <- read_excel('../../Data/RawData/HABITAT_SEG_MASTER_2006-2018_narcodes.xlsx',
                     na = c('', 'na', 'NA', 'n/a'),
                     sheet = 'Sheet1'
                     ) %>% 
  unique %>% 
  mutate(
    Reach = gsub('\\.|\\,\\s*', ',', Reach),
    Watershed = case_when(
      Watershed == 'Pajaro' ~ 'PAJ',
      Watershed == 'Aptos' ~ 'APT',
      Watershed == 'Soquel' ~ 'SOQ',
      Watershed == 'SLR' & Reach %in% slrmain ~ 'SLR-main',
      Watershed == 'SLR' & !Reach %in% slrmain ~ 'SLR-trib',
      T ~ NA_character_
    ),
    Watershed = factor(Watershed, levels = c('SLR-main', 'SLR-trib', 'SOQ', 'APT', 'PAJ')),
    Stream = factor(Stream, levels = oldstr, labels = newstr),
    Stream = as.character(Stream),
    Stream = case_when(
      Watershed %in% 'SOQ' & Reach %in% c('14a', '14b', '14c', '14d') ~ 'west', 
      Watershed %in% 'SOQ' & Reach %in% c('9a', '9b', '10', '11', '12a', '12b') ~ 'east',
      Watershed %in% 'APT' & is.na(Stream) ~ 'main',
      Watershed %in% 'SLR-trib' & Reach %in% c('21a upper', '21b') ~ 'bran',
      Watershed %in% 'SLR-trib' & Reach %in% c('17a', '17b') ~ 'bldr',
      Watershed %in% 'SLR-trib' & Reach %in% c('15') ~ 'fall',
      Watershed %in% 'SLR-trib' & Reach %in% c('13d') ~ 'zayt',
      Watershed %in% 'SLR-trib' & Reach %in% c('14b') ~ 'bean',
      Watershed %in% 'SLR-trib' & Reach %in% c('16') ~ 'newl',
      Watershed %in% 'SLR-main' & is.na(Stream) ~ 'main',
      T ~ Stream
    ), 
    Year = factor(Year),
    `avg. embedd.` = gsub('n/a', '', `avg. embedd.`),
    `avg. embedd.` = as.numeric(`avg. embedd.`),
    `Gen Hab Type` = case_when(
      `Gen Hab Type` %in% c('other', 'Other') ~ 'other',
      T ~ `Gen Hab Type`
    ), 
    `Gen Hab Type` = tools::toTitleCase(tolower(`Gen Hab Type`)),
    `HAB abbrev` = case_when(
      `HAB abbrev` %in% c('LSB0') ~ 'LSBo', 
      `HAB abbrev` %in% c('Run') ~ 'RUN', 
      `HAB abbrev` %in% c('SLBk') ~ 'LSBk',
      `HAB abbrev` %in% c('SLBo') ~ 'LSBo',
      T ~ `HAB abbrev`
    )
  ) %>% 
  filter(!is.na(Watershed)) %>% 
  filter(!is.na(Reach)) %>% 
  filter(!is.na(`Hab. #`)) %>% 
  unite('ReachID', Watershed, Stream, Reach, sep = '-', remove = F) %>% 
  dplyr::select(-`Hab. type`, -`CDFW level IV CODE`, -`CDFW level II`, -`Artificial (True/False)`, -`Wood (True/False)`, -`Boulder (True/False)`, -`Bedrock (True/False)`, -`Dominant Feature (Primary Habitat Feature)`) %>%
  filter(!Year %in% '2018') %>% 
  left_join(habcodes, by = 'HAB abbrev') %>% 
  dplyr::select(-`HAB abbrev`) %>% 
  rename(`HAB abbrev` = level4)

save(rchdat, file = 'data/rchdat.RData', compress = 'xz')

# reach -------------------------------------------------------------------

# sf object of reach, is an incomplete match with rchdat 
reach <- readOGR(dsn = dsn, layer = 'Reach') %>% 
  spTransform(prj) %>% 
  st_as_sf %>% 
  rename(Reach = ReachNum) %>% 
  mutate(
    Watershed = as.character(Watershed),
    Watershed = case_when(
      Watershed == 'SLR' & Reach %in% slrmain ~ 'SLR-main',
      Watershed == 'SLR' & !Reach %in% slrmain ~ 'SLR-trib', 
      TRUE ~ Watershed
    ),
    Stream = gsub('^.*\\-', '', Stream)
  ) %>% 
  dplyr::select(-CreekMile, -ReachID) %>% 
  unite('ReachID', Watershed, Stream, Reach, sep = '-', remove = F)

save(reach, file = 'data/reach.RData', compress = 'xz')

# floest ------------------------------------------------------------------

##
# import and organize modelled flow data
# june/july estimates for every year at select locations
# based on best regression fit to two USGS flow gages

##
# June flow

raw <- read_excel('../../Data/RawData/flow_ests/16-1022_JSSH_Results_2019-01-18.xlsx', sheet = 'June Results') 

rwsel <- c(4:38)

recs <- raw %>%
  dplyr::select(Site, X__8) %>% 
  .[rwsel, ] %>% 
  rename(
    rec = X__8
  )

# soquel records
soq <- raw %>% 
  dplyr::select(Site, `Estimated Monthly Average Flow for June Using Soquel Creek Gage Correlations (cfs)`:X__56)
nmssoq <- soq[1, , drop = T ] %>% 
  unlist %>% 
  na.omit() %>% 
  as.numeric() %>% 
  c('Site', .)
soq <- soq[rwsel, ]
names(soq) <- nmssoq
soq <- soq %>% 
  gather('yr', 'flo', -Site) %>% 
  mutate(
    rec = 'Soquel',
    mo = 6
  )

# big tree records
big <- raw %>% 
  dplyr::select(Site, `Estimated Monthly Average Flow for June Using Big Trees Gage Correlations (cfs)`:X__32)
nmssoq <- big[1, , drop = T ] %>% 
  unlist %>% 
  na.omit() %>% 
  as.numeric() %>% 
  c('Site', .)
big <- big[rwsel, ]
names(big) <- nmssoq
big <- big %>% 
  gather('yr', 'flo', -Site) %>% 
  mutate(
    rec = 'Big Trees',
    mo = 6
  )

soqbig <- bind_rows(soq, big)

junest <- inner_join(recs, soqbig, by = c('Site', 'rec'))

##
# September flow

raw <- read_excel('../../Data/RawData/flow_ests/16-1022_JSSH_Results_2019-01-18.xlsx', sheet = 'September Results') 

rwsel <- c(4:38)

recs <- raw %>%
  dplyr::select(Site, X__8) %>%
  .[rwsel, ] %>%
  rename(
    rec = X__8
  )

# soquel records
soq <- raw %>% 
  dplyr::select(Site, `Estimated Monthly Average Flow (cfs) for Septemeber Using Soquel Creek Gage Correlations`:X__56)
nmssoq <- soq[1, , drop = T ] %>% 
  unlist %>% 
  na.omit() %>% 
  as.numeric() %>% 
  c('Site', .)
soq <- soq[rwsel, ]
names(soq) <- nmssoq
soq <- soq %>% 
  gather('yr', 'flo', -Site) %>% 
  mutate(
    rec = 'Soquel',
    mo = 9
  )

# big tree records
big <- raw %>% 
  dplyr::select(Site, `Estimated Monthly Average Flow (cfs) for Septemeber Using Big Trees Gage Correlations`:X__32)
nmssoq <- big[1, , drop = T ] %>% 
  unlist %>% 
  na.omit() %>% 
  as.numeric() %>% 
  c('Site', .)
big <- big[rwsel, ]
names(big) <- nmssoq
big <- big %>% 
  gather('yr', 'flo', -Site) %>% 
  mutate(
    rec = 'Big Trees',
    mo = 9
  )

soqbig <- bind_rows(soq, big)

sepest <- inner_join(recs, soqbig, by = c('Site', 'rec'))


##
# get usgs flow gauge data, june and september

raw <- read_excel('../../Data/RawData/flow_ests/16-1022_JSSH_Results_2019-01-18.xlsx', sheet = 'USGS Soquel daily data', skip = 1) 

usgsjun <- raw %>%
  dplyr::select(Date, `Daily Average Flow (cfs)`) %>% 
  rename(
    date = Date, 
    flo = `Daily Average Flow (cfs)`
  )

usgssep <- raw %>%
  dplyr::select(Date__1, `Daily Average Flow (cfs)__1`) %>% 
  rename(
    date = Date__1, 
    flo = `Daily Average Flow (cfs)__1`
  )

usgs <- rbind(usgsjun, usgssep) %>% 
  na.omit %>% 
  mutate(
    yr = year(date),
    mo = month(date)
  ) %>% 
  group_by(yr, mo) %>% 
  summarise(flo = mean(flo, na.rm = T)) %>% 
  ungroup %>% 
  mutate(
    Site = 'USGS Soquel',
    rec = 'USGS Soquel', 
    yr = as.character(yr)
    ) %>% 
  dplyr::select(Site, rec, yr, flo, mo)

## 
# all flow combined

floest <- bind_rows(junest, sepest, usgs) %>% 
  mutate(
    dy = 15
  ) %>% 
  unite('date', yr, mo, dy, sep = '-') %>% 
  mutate(
    date = ymd(date)
  ) %>% 
  dplyr::select(Site, date, flo) %>% 
  filter(!Site %in% 'Upper Soquel Cr. Weir Sq02') # this site is junk

save(floest, file = 'data/floest.RData', compress = 'xz')

# fishmtch ----------------------------------------------------------------

##
# matching flow locations with fish sites, just a lookup table

data(floest)

# flo location, fish site matchings (soquel only)
fishmtch_soq <- read_excel('../../Data/RawData/flow_ests/JSSH_KK_CorrTable_2019-03-28_mbedits.xlsx', sheet = 'Soquel Watershed') %>% 
  gather('mo', 'SiteFlow', -`Fish Sampling Site`, -Group) %>% 
  rename(SiteID = `Fish Sampling Site`) %>% 
  mutate(
    mo = case_when(
      mo %in% 'June Flow Site' ~ 6, 
      T ~ 9
    )
  ) %>% 
  mutate(Watershed = 'SOQ')

# San lorenzo
fishmtch_slr <- read_excel('../../Data/RawData/flow_ests/JSSH_KK_CorrTable_2019-03-28_mbedits.xlsx', 
                           sheet = 'San Lorenzo Watershed',
                           skip = 1
                           ) %>% 
  mutate(
    mo = 9, 
    Watershed = 'SLR',
    Represents = gsub('(^SLR)', '\\1-', Represents),
    Represents = gsub('([0-9]+)', '-\\1', Represents),
    Represents = gsub('BEAN', 'bean', Represents),
    Represents = gsub('Zayt|ZAYT', 'zayt', Represents),
    Represents = gsub('Lomp', 'lomp', Represents),
    Represents = gsub('a\\-1$', 'a1', Represents)
    ) %>% 
  rename(
    SiteFlow = Site,
    SiteID = Represents
    ) %>% 
  filter(SiteFlow %in% floest$Site) %>% # there are five sites in the lookup table that aren't in floest
  filter(!duplicated(SiteFlow)) %>% 
  filter(!duplicated(SiteID)) %>% 
  dplyr::select(SiteID, Group, mo, SiteFlow, Watershed) %>% 
  filter(SiteID %in% fishdat$SiteID) # two are not in fishdat

# Pajaro
fishmtch_paj <- read_excel('../../Data/RawData/flow_ests/JSSH_KK_CorrTable_2019-03-28_mbedits.xlsx', 
                       sheet = 'Pajaro Watershed',
                       skip = 1
  ) %>% 
  mutate(
    mo = 9, 
    Watershed = 'PAJ'
    ) %>% 
  rename(
    SiteFlow = Site,
    SiteID = Represents
  ) %>% 
  filter(SiteFlow %in% floest$Site) %>% # there are five sites in the lookup table that aren't in floest
  filter(!duplicated(SiteFlow)) %>% 
  filter(!duplicated(SiteID)) %>% 
  dplyr::select(SiteID, Group, mo, SiteFlow, Watershed)

# combine all
fishmtch <- rbind(fishmtch_slr, fishmtch_soq, fishmtch_paj)

save(fishmtch, file = 'data/fishmtch.RData', compress = 'xz')

