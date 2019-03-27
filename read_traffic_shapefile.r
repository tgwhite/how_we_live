

library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(ggmap)
library(tidyverse)
library(data.table)
library(sf)
library(USAboundaries)
library(scales)
library(tmap)
library(readxl)

setwd('~/how_we_live/data')
list.files()
county_to_county_commutes = read_excel('county_to_county_commutes.xlsx', sheet = 'cleaned')
california_commutes = filter(county_to_county_commutes, residence_state == 'California')

# by county, compute how many go to los angeles, and how many to san francisco 
sf_la_fips = filter(ca_counties, name %in% c('San Francisco', 'Los Angeles'))
sf_la_fips
head(county_to_county_commutes)


los_angeles_sf_commutes = inner_join(
  california_commutes %>% 
    mutate(workplace_state_fips_country_code = as.numeric(workplace_state_fips_country_code),
           workplace_county_fips = as.numeric(workplace_county_fips),
           residence_county_fips = as.numeric(residence_fips),
           residence_state_fips = as.numeric(residence_state_fips)
           ), 
  sf_la_fips %>% 
    as.data.frame() %>% 
    select(-geometry) %>%
    mutate(statefp = as.numeric(statefp), countyfp = as.numeric(countyfp)), 
               by = c('workplace_state_fips_country_code' = 'statefp', 'workplace_county_fips' = 'countyfp'))

formatted_counties = 
  ca_counties %>% mutate(statefp = as.numeric(statefp), countyfp = as.numeric(countyfp)) %>% as.data.frame() %>% select(-geometry) %>%
  select(statefp, countyfp, residence_county_name = name) %>% 
  mutate(statefp = as.numeric(statefp), countyfp = as.numeric(countyfp))

los_angeles_sf_commutes = inner_join(los_angeles_sf_commutes, formatted_counties, by = c('residence_state_fips' = 'statefp', 'residence_county_fips' = 'countyfp'))
d
d$residence_county_name %>% table()  



head(los_angeles_sf_commutes %>% as.data.frame())
dim(los_angeles_sf_commutes)
table(los_angeles_sf_commutes$residence_fips)
table(los_angeles_sf_commutes$residence_county_name)
a = ca_counties %>% mutate(statefp = as.numeric(statefp), countyfp = as.numeric(countyfp)) %>% as.data.frame() %>% select(-geometry) %>%
  select(statefp, countyfp, residence_county_name = name)
str(a)

setwd("~/how_we_live/shapefiles")

setwd('AADT2016')

# this approach had issues reading in multipoint geometry
# traffic_shapefile <- readOGR(dsn = ".", layer = "AADT2016")
# traffic_shapefile <- readOGR(dsn = "AADT2016", p4s = "+init=epsg:27700")

traffic_shapefile = st_read(dsn = "AADT2016")
traffic_shapefile$ahead_aadt = as.numeric(as.character(traffic_shapefile$Ahead_AADT))

quintiles = quantile(traffic_shapefile$ahead_aadt, probs = c(0, .25, 0.5, 0.75, 1), na.rm = T)
quartiles_pretty = paste(lag(comma(quintiles)), '-', comma(quintiles)) %>% tail(4)
traffic_shapefile$traffic_quartile = cut(traffic_shapefile$ahead_aadt, breaks = quintiles, include.lowest = T, right = F, labels = quartiles_pretty, ordered_result = T)

setwd('..')
HSR_Route_shapefile = st_read(dsn = 'HSR_Route_web')
HSR_Stations_shapefile = st_read(dsn = 'HSR_Stations_v2_web')

setwd('..')

ns_routes = filter(traffic_shapefile, Route %in% c(5, 99, 101))
la_to_sf = filter(traffic_shapefile, Route %in% c(5, 120, 205, 580, 80))
la_to_san_jose = filter(traffic_shapefile, Route %in% c(5, 152, 280))

us_cities_shapefile = us_cities()
us_states_shapefile = us_states()
us_counties_shapefile = us_counties()

california = filter(us_states_shapefile, name == 'California')
ca_counties = filter(us_counties_shapefile, state_name == 'California')
head(ca_counties)

ggplot(ns_routes, aes(Lat_N_or_E, Ahead_AADT %>% as.character() %>% as.numeric(), colour = factor(Route))) + 
  geom_point() + stat_smooth() +
  scale_x_continuous(limits = c(30, 50))

# tmap::qtm(ca_counties)

california_cities = filter(us_cities_shapefile, state_abbr == 'CA')
big_ca_cities = filter(us_cities_shapefile, state_abbr == 'CA') %>% arrange(-population) %>% head(5)

# ggplot() + 
#   geom_sf(data = california, aes(), size = 1) +
#   geom_sf(data = ns_routes, aes(colour = as.numeric(as.character(Ahead_AADT)))) + 
#   geom_sf_label(data = big_ca_cities, aes(label = city)) +
#   scale_colour_gradient(low = 'white', high = 'orange')

ca_traffic_plot = ggplot() + 
  geom_sf(data = california, aes(), fill = 'black', colour = 'black') +
  geom_sf(data = traffic_shapefile %>% filter(!is.na(traffic_quintile)), aes(colour = traffic_quartile, fill = traffic_quartile)) +
  scale_colour_hue(name = 'Average Daily Traffic') +
  scale_fill_hue(name = 'Average Daily Traffic') + 
  labs(title = 'California Daily Car Traffic Patterns', subtitle = '2016', x = '\nTaylor G. White\nSource: California Department of Transportation')

ggsave('ca_traffic.png', plot = ca_traffic_plot, height = 8, width = 8, units = 'in', dpi = 400)

ca_traffic_hsr = ggplot() + 
  geom_sf(data = california, aes(), fill = 'black', colour = 'black') +
  geom_sf(data = traffic_shapefile %>% filter(!is.na(traffic_quartile)), aes(colour = traffic_quartile, fill = traffic_quartile)) +
  geom_sf(data = HSR_Route_shapefile, aes(), colour = 'white', size = 1) + 
  scale_colour_hue(name = 'Average Daily Traffic') +
  scale_fill_hue(name = 'Average Daily Traffic') + 
  labs(title = 'CA Car Traffic vs. High Speed Rail Route', subtitle = '2016', x = '\nTaylor G. White\nSource: California Department of Transportation')


ggsave('ca_traffic_hsr.png', plot = ca_traffic_hsr, height = 8, width = 8, units = 'in', dpi = 400)
