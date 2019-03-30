
# to do -- use sqlite as storage to work around download/rate limit issues

library(tidyverse)
library(reshape2)
library(data.table)
library(tigris)
library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(ggmap)
library(sf)
library(USAboundaries)
library(scales)

setwd("~/how_we_live/Data")
all_cleaned_county_data_fin = readRDS('all_cleaned_county_data_fin.rds')

# change in price = new people alone + people make more money alone + new people AND more money 

selected_vars = c(
  "Per Capita Personal Income",
  "Net Migration Flow",
  "Resident Population",
  "New Private Housing Structures Authorized by Building Permits",
  "Homeownership Rate" ,
  "Burdened Households" ,
  "All-Transactions House Price Index"
)

wide_data = 
  all_cleaned_county_data_fin %>% 
  filter(
    title_clean %in% selected_vars & title != 'Net Migration Flow for East Baton Rouge Parish, LA'
  ) %>%
  data.table() %>%
  dcast.data.table(
    year + state_name + county_name ~ title_for_col, value.var = c('percent_change', 'value', 'delta')
  ) %>%
  mutate(
    # new units is the delta units, so use value here
    value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits = ifelse(value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits == 0, 0.1, value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits),
    delta_people_per_delta_units = (delta_x_Resident_Population * 1000) / value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits,
    county_clean = str_extract(county_name, '.*, ') %>% str_replace('( Parish, )|( County, )|( County/city, )|( City, )', '')
  )


# get us counties shapes

to_counties = c('De Soto', 'Lagrange', 'La Salle', 'La Porte', 'Fond Du Lac')
names(to_counties) = c('DeSoto', 'LaGrange', 'LaSalle', 'LaPorte', 'Fond du Lac')
names(to_counties)

us_counties_shapefile = us_counties() %>%
  rename(
    county_clean = name
  ) %>%
  mutate(
    county_clean = recode(county_clean, to_counties)
  )

filter(us_counties_shapefile, state_name == 'Indiana')$county_clean %>% unique()
filter(wide_data, state_name == 'Indiana')$county_clean %>% unique()
# this will do a join for each year of data and each county
joined_data_shapes = left_join(us_counties_shapefile, wide_data)
filter(joined_data_shapes, state_name == 'Indiana')$county_clean %>% unique()

ggplot() + 
  geom_sf(data = joined_data_shapes %>% filter(year == 2016), aes(fill = value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits)) 
  

distinct_counties_states_data = select(wide_data, state_name, county_clean) %>% unique() %>% mutate(x = TRUE)
distinct_shape_counties = select(us_counties_shapefile, state_name, county_clean) %>% unique()

b = full_join(distinct_shape_counties, distinct_counties_states_data)

filter(b, is.na(x) & state_name != 'Puerto Rico')
filter(distinct_counties_states_data, state_name=='Wisconsin')
filter(distinct_shape_counties, state_name != 'Puerto Rico') %>% dim()
