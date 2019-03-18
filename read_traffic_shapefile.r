

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
setwd("~/how_we_live")
list.files()

setwd('AADT2016')

# this approach had issues reading in multipoint geometry
# traffic_shapefile <- readOGR(dsn = ".", layer = "AADT2016")
# traffic_shapefile <- readOGR(dsn = "AADT2016", p4s = "+init=epsg:27700")

traffic_shapefile = st_read(dsn = "AADT2016")
traffic_shapefile$ahead_aadt = as.numeric(as.character(traffic_shapefile$Ahead_AADT))
plot(traffic_shapefile)

ns_routes = filter(traffic_shapefile, Route %in% c(5, 99, 101))
plot(ns_routes)

la_to_sf = filter(traffic_shapefile, Route %in% c(5, 120, 205, 580, 80))
plot(la_to_sf)

la_to_san_jose = filter(traffic_shapefile, Route %in% c(5, 152, 280))
plot(la_to_san_jose)

head(traffic_shapefile)


# extract polygon data and update file
# polygon_data <- 
#   county_shapefiles@data %>%
#   mutate(
#     polygon_center_lon = INTPTLON %>% as.character() %>% as.numeric(),
#     polygon_center_lat = INTPTLAT %>% as.character() %>% as.numeric()
#   ) %>%
#   data.table(key = "GEOID")

# county_shapefiles_df <- 
#   fortify(la_to_san_jose, region = "GEOID")

ggplot(ns_routes, aes(Lon_S_or_W, Lat_S_or_W)) +
  geom_point(aes(color = as.numeric(as.character(Ahead_AADT)))) + 
  # scale_color_gradient(guide = F) + 
  theme(
    axis.text.y = element_blank()
  )


ggplot(ns_routes, aes(Lat_N_or_E, Ahead_AADT %>% as.character() %>% as.numeric(), colour = factor(Route))) + 
  geom_point() + stat_smooth() +
  scale_x_continuous(limits = c(30, 50))

us_cities = us_cities()
us_states = us_states()
california = filter(us_states, name == 'California')

us_cities_fortified$geometry %>% class()

california_cities = filter(us_cities, state_abbr == 'CA')
big_ca_cities = filter(us_cities, state_abbr == 'CA') %>% arrange(-population) %>% head(5)

ggplot() + 
  geom_sf(data = california, aes(), size = 1) +
  geom_sf(data = ns_routes, aes(colour = as.numeric(as.character(Ahead_AADT)))) + 
  geom_sf_label(data = big_ca_cities, aes(label = city)) +
  scale_colour_gradient(low = 'white', high = 'orange')

quintiles = quantile(traffic_shapefile$ahead_aadt, probs = c(0, .25, 0.5, 0.75, 1), na.rm = T)
quartiles_pretty = paste(lag(comma(quintiles)), '-', comma(quintiles)) %>% tail(4)
traffic_shapefile$traffic_quartile = cut(traffic_shapefile$ahead_aadt, breaks = quintiles, include.lowest = T, right = F, labels = quartiles_pretty, ordered_result = T)

ca_traffic_plot = ggplot() + 
  geom_sf(data = california, aes(), fill = 'black', size = 1) +
  geom_sf(data = traffic_shapefile %>% filter(!is.na(traffic_quintile)), aes(colour = traffic_quartile, fill = traffic_quartile)) +
  scale_colour_hue(name = 'Average Daily Traffic') +
  scale_fill_hue(name = 'Average Daily Traffic') + 
  labs(title = 'California Daily Car Traffic Patterns', subtitle = '2016', x = '\nTaylor G. White\nSource: California Department of Transportation')

ggsave('ca_traffic.png', plot = ca_traffic_plot, height = 8, width = 8, units = 'in', dpi = 400)


sum(is.na(traffic_shapefile$traffic_quintile))
