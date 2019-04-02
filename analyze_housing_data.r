
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

# get county shapes
from_counties = c('DeSoto', 'LaGrange', 'LaSalle', 
                  'LaPorte', 'Fond du Lac', 'DuPage', 'LaMoure', "Doña Ana", 
                  "DeKalb", "Lafourche", "Lac qui Parle", "Carson City", "DeWitt")
to_counties = c('De Soto', 'Lagrange', 'La Salle', 'La Porte', 'Fond Du Lac', 'Du Page', 
                'La Moure', 'Dona Ana', "De Kalb", "LaFourche", "Lac Qui Parle", "Carson", "De Witt")

us_counties_shapefile = us_counties() %>%
  rename(
    county_clean = name
  ) %>%
  mutate(
    county_clean = plyr::mapvalues(county_clean, from_counties, to_counties)
  )

us_states_shapefile = us_states()

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
unique(wide_data$state_name)

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
    decade = year - (year %% 10),
    # new units is the delta units, so use value here
    value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits = ifelse(value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits == 0, 0.1, value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits),
    delta_people_per_delta_units = (delta_x_Resident_Population * 1000) / value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits,
    county_clean = str_extract(county_name, '.*, ') %>% str_replace('( Parish, )|( County, )|( County/city, )|( City, )', '')
  ) %>%
  arrange(
    state_name, county_clean, year
  )


# calculate cumulative sums of houses built and migration by decade 

joined_data_shapes_dt = inner_join(us_counties_shapefile, wide_data) %>% data.table()

cumulative_stats = joined_data_shapes_dt[, {
  
  starting_prices = value_x_All_Transactions_House_Price_Index[!is.na(value_x_All_Transactions_House_Price_Index)][1]
  ending_prices = tail(value_x_All_Transactions_House_Price_Index[!is.na(value_x_All_Transactions_House_Price_Index)], 1)
  
  starting_pop = value_x_Resident_Population[!is.na(value_x_Resident_Population)][1]
  ending_pop = tail(value_x_Resident_Population[!is.na(value_x_Resident_Population)], 1)
  
  total_units_authorized = sum(value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits, na.rm = T)
  delta_pop = ending_pop - starting_pop
  # delta_pop = ifelse(delta_pop == 0, 1, delta_pop)
  
  starting_income = value_x_Per_Capita_Personal_Income[!is.na(value_x_Per_Capita_Personal_Income)][1]
  ending_income = tail(value_x_Per_Capita_Personal_Income[!is.na(value_x_Per_Capita_Personal_Income)], 1)
  
  units_per_capita = total_units_authorized / (ending_pop*1000)
  pct_obs_housing = length(value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits[!is.na(value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits)])/length(year)
  
  starting_pop_density = (starting_pop * 1000) / aland[1]
  ending_pop_density = (ending_pop * 1000) / aland[1]
  
  
  list(
    starting_pop_density = starting_pop_density,
    ending_pop_density = ending_pop_density, 
    density_change = (ending_pop_density - starting_pop_density)/starting_pop_density,
    
    starting_pop = starting_pop, 
    ending_pop = ending_pop,
    starting_income = starting_income, 
    ending_income = ending_income,
    delta_income = ending_income - starting_income,
    pct_change_income = (ending_income - starting_income)/starting_income,
    delta_pop = delta_pop,
    pct_change_pop = delta_pop/starting_pop,
    starting_prices = starting_prices,
    ending_prices = ending_prices,
    price_change = (ending_prices - starting_prices)/starting_prices,
    net_migration = sum(value_x_Net_Migration_Flow, na.rm = T),
    total_units_authorized = total_units_authorized, 
    units_per_delta_pop = total_units_authorized / (delta_pop * 1000),
    units_per_capita = units_per_capita,
    log_units_per_capita = log(ifelse(units_per_capita == 0, 1, units_per_capita)),
    pct_obs_housing = pct_obs_housing,
    ratio_pop_units = (delta_pop*1000)/total_units_authorized,
    diff_pop_units = delta_pop*1000 - total_units_authorized,
    diff_units_delta_pop_per_starting_pop = (delta_pop*1000 - total_units_authorized) / (starting_pop*1000)
  )
}, by = list(state_name, county_clean, decade)] %>%
  mutate(
    units_per_delta_pop = ifelse(is.infinite(units_per_delta_pop), NA, units_per_delta_pop),
    ratio_pop_units = ifelse(is.infinite(ratio_pop_units), NA, ratio_pop_units),
    ratio_pop_units_cat = cut(ratio_pop_units, breaks = quantile(ratio_pop_units, probs = seq(0, 1, by = 0.2), na.rm = T), include.lowest = T, right = F, ordered_result = T)
  )

cum_stats_shapes = left_join(us_counties_shapefile, cumulative_stats %>% as.data.frame())
cum_stats_shapes$ratio_pop_units_cat %>% table() %>% sum()
nrow(cum_stats_shapes)
as.integer(cum_stats_shapes$ratio_pop_units_cat)

exclude_states = c('Hawaii', 'Alaska', "Puerto Rico")
include_states = unique(cum_stats_shapes$state_name)
include_states = include_states[!include_states %in% exclude_states]

population_vs_housing = ggplot() + 
  scale_fill_viridis_d(name = '') +
  labs(
    title = 'Population Added Per Housing Unit Authorized',
    subtitle = '2010 Decade'
  ) +
  geom_sf(data = cum_stats_shapes %>% filter(decade == 2010, state_name %in% include_states), aes(fill = ratio_pop_units_cat)) +
  geom_sf(data = us_states_shapefile %>% filter(name %in% include_states), aes(), colour = 'white', size = .75, alpha = 0) 
population_vs_housing
ggsave('population_vs_housing.png', plot = population_vs_housing, height = 8, width = 9, units = 'in', dpi = 400)

scale_type(cum_stats_shapes$ratio_pop_units_cat)
cumulative_stats_long = melt(cumulative_stats, id = c('state_name', 'county_clean', 'decade', 'price_change'))

b = lm(total_units_authorized ~ starting_pop_density*starting_income + delta_pop + delta_income, data = cumulative_stats, weights = pct_obs_housing)
summary(b)
View(cumulative_stats)

price_change_model = lm(price_change ~ starting_prices* starting_pop_density + starting_income + diff_units_delta_pop_per_starting_pop, data = cumulative_stats)
summary(price_change_model)
filter(cumulative_stats, is.infinite(units_per_delta_pop))

ggplot(cumulative_stats, aes(diff_units_delta_pop_per_starting_pop, price_change)) + 
  geom_point() + 
  stat_smooth(method = 'lm')

ggplot(cumulative_stats, aes(starting_pop, total_units_authorized)) + 
  geom_point(aes(colour = price_change, alpha = pct_obs_housing)) + 
  stat_smooth(method = 'lm') +
  scale_colour_gradient2(low = 'blue', high = 'red', mid = 'white') + 
  coord_cartesian(xlim = c(200, 2500), ylim = c(0, 1e5)) +
  facet_wrap(~decade, scales = 'free')

ggplot(cumulative_stats, aes(delta_pop, total_units_authorized)) + 
  geom_point(aes(colour = price_change, alpha = pct_obs_housing)) + 
  stat_smooth(method = 'lm') +
  scale_colour_gradient2(low = 'blue', high = 'red', mid = 'white') + 
  # coord_cartesian(xlim = c(200, 2500), ylim = c(0, 1e5)) +
  facet_wrap(~decade)

f?scale_fill_gradient2
a = lm(price_change ~ pct_change_pop + pct_change_income + log_units_per_capita, data = cumulative_stats)
summary(a)
filter(cumulative_stats, is.infinite(log(units_per_capita)))
ggplot(cumulative_stat)

ggplot(cumulative_stats_long, aes(value, price_change)) +
  geom_point() + stat_smooth(method = 'lm') +
  facet_wrap(~variable, scales = 'free')

ggplot(cumulative_stats_long, aes(value)) + 
  facet_wrap(~variable, scales = 'free') + 
  geom_histogram()

a = cumulative_stats %>% filter(state_name == 'California', decade == 2010)
View(a)
# get us counties shapes

head(us_counties_shapefile)


# this will do a join for each year of data and each county

joined_cumulative_stats_shapes = left_join(us_counties_shapefile, cumulative_stats)
head(joined_cumulative_stats_shapes)
joined_cumulative_stats_shapes$population_

ggplot() + 
  geom_sf(data = joined_data_shapes %>% filter(year == 2016), aes(fill = value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits)) 
  