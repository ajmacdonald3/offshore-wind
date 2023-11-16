library(tidyverse) # for data manipulation
library(lubridate) # for working with dates
library(sf) # mapping
library(rnaturalearth) # mapping
library(viridis) # colour palettes for plotting

# set timezone to UTC/GMT
Sys.setenv(TZ = "GMT")

# source functions
source("./analysis/useful_functions.R")

# set up basic things for maps (need rgdal package installed) 
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf") # need rgeos package installed
#lakes <- ne_download(scale = "medium", type = 'lakes', category = 'physical',
#                     returnclass = "sf", destdir = "./map-data/lakes") # only need this first time downloading
lakes <- ne_load(type = "lakes", scale = "medium", category = 'physical',
                 returnclass = "sf",
                 destdir = paste0("./map-data/lakes")) # use this if already downloaded shapefiles

# load study area shapefile
study.area <- read_sf("map-data/study-area-ns/Study_Area_NS_NÉ_Zone_détude.shp")

study.area <- st_transform(study.area, 4326)

# load cleaned detection data
sesa.clean <- readRDS("data/clean.data_sesa.rds")

# calculate transitions between stations
sesa.transit <- sesa.clean %>%
  do(add_deploy_loc(.)) %>%
  do(site_transit_min(.))

# classify transitions as connected, or not_connected
sesa.direct <- sesa.transit %>%
  filter(!dist.min == 0) %>% 
  mutate(state = ifelse(rate >= 9, "connected", "not_connected")) %>% # 9 m/s from Anderson et al. (2019)
  filter(state == "connected") %>%
  select(motusTagID, ts.x, lat.x, lon.x, lat.y, lon.y, month.y, year.y)

# create unique id for each flight
sesa.flights <- sesa.direct %>% 
  rowid_to_column("ID")

# split so start and end points have own rows but still linked by id
start.flight <- sesa.flights %>% 
  select(ID, motusTagID, ts.x, lat.x, lon.x) %>% 
  rename(ts = ts.x,
         lat = lat.x,
         lon = lon.x)

end.flight <- sesa.flights %>% 
  select(ID, motusTagID, ts.x, lat.y, lon.y)%>% 
  rename(ts = ts.x,
         lat = lat.y,
         lon = lon.y)

sesa.flights.long <- bind_rows(start.flight, end.flight)

# convert points to linestrings
sesa.flights.sf <- sesa.flights.long %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  group_by(ID) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING") %>% 
  st_set_crs(4326) %>%
  st_transform(st_crs(study.area)) %>% 
  ungroup()

# identify whether a flight crosses the study area
sesa.flights.wind <- sesa.flights.sf %>% 
  mutate(wind.area = st_intersects(., study.area, sparse = FALSE))

area.flights <- sesa.flights.wind %>% 
  filter(wind.area == TRUE)

# get bounding box for data and set limits for map
bbox <- area.flights %>% 
  st_bbox()

xmin <- bbox[1] - 2
ymin <- bbox[2] - 1
xmax <- bbox[3] + 2
ymax <- bbox[4] + 1

# make sure everything in same crs
world.sf <- world %>% 
  st_transform(st_crs(study.area))

lakes.sf <- lakes %>% 
  st_transform(st_crs(study.area))

# plot map of flights that cross study area
png(filename = "figures/sesa-flights.png",
    width=6, height=8, units="in", res=600)

ggplot() +
  geom_sf(data = world, colour = "white", fill = "grey80") +
  geom_sf(data = lakes, fill = "white", colour = NA) +
  geom_sf(data = area.flights,
          aes(group = ID),
          colour = "black", size = 0.5) +
  geom_sf(data = study.area,
          colour = "red", size = 1, fill = NA) +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)

dev.off()
