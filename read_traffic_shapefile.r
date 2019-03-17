

library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(ggmap)
library(plyr)
library(dplyr)
library(data.table)
library(sf)

setwd("~/how_we_live")
list.files()

setwd('AADT2016')

# this approach had issues reading in multipoint geometry
# traffic_shapefile <- readOGR(dsn = ".", layer = "AADT2016")
# traffic_shapefile <- readOGR(dsn = "AADT2016", p4s = "+init=epsg:27700")

traffic_shapefile = st_read(dsn = "AADT2016")
plot(traffic_shapefile)

class(traffic_shapefile)
head(traffic_shapefile)

ns_routes = filter(traffic_shapefile, Route %in% c(5, 99, 101))
plot(ns_routes)

la_to_sf = filter(traffic_shapefile, Route %in% c(5, 120, 205, 580, 80))
plot(la_to_sf)

la_to_san_jose = filter(traffic_shapefile, Route %in% c(5, 152, 280))
plot(la_to_san_jose)

head(traffic_shapefile)


# extract polygon data and update file
polygon_data <- 
  county_shapefiles@data %>%
  mutate(
    polygon_center_lon = INTPTLON %>% as.character() %>% as.numeric(),
    polygon_center_lat = INTPTLAT %>% as.character() %>% as.numeric()
  ) %>%
  data.table(key = "GEOID")

county_shapefiles_df <- 
  fortify(la_to_san_jose, region = "GEOID")

ggplot(ns_routes, aes(Lon_S_or_W, Lat_S_or_W)) +
  geom_point(aes(color = as.numeric(as.character(Ahead_AADT)))) + 
  # scale_color_gradient(guide = F) + 
  theme(
    axis.text.y = element_blank()
  )


ggplot(ns_routes, aes(Lat_N_or_E, Ahead_AADT %>% as.character() %>% as.numeric(), colour = factor(Route))) + 
  geom_point() + stat_smooth() +
  scale_x_continuous(limits = c(30, 50))

