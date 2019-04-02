
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
library(quantreg)

# there are some issues with county name mappings between FRED and the shapefiles
from_counties = c('DeSoto', 'LaGrange', 'LaSalle', 
                  'LaPorte', 'Fond du Lac', 'DuPage', 'LaMoure', "Doña Ana", 
                  "DeKalb", "Lafourche", "Lac qui Parle", "Carson City", "DeWitt")
to_counties = c('De Soto', 'Lagrange', 'La Salle', 'La Porte', 'Fond Du Lac', 'Du Page', 
                'La Moure', 'Dona Ana', "De Kalb", "LaFourche", "Lac Qui Parle", "Carson", "De Witt")

# get county shapefiles and clean up names
us_counties_shapefile = us_counties() %>%
  rename(
    county_clean = name
  ) %>%
  mutate(
    county_clean = plyr::mapvalues(county_clean, from_counties, to_counties)
  )

# get states shapefile
us_states_shapefile = us_states()

# read in annual economic data from fred by county
setwd("~/how_we_live/Data")
all_cleaned_county_data_fin = readRDS('all_cleaned_county_data_fin.rds')

# change in price = new people alone + people make more money alone + new people AND more money 
names(all_cleaned_county_data_fin)
all_cleaned_county_data_fin$title_clean %>% unique()
selected_vars = c(
  "Per Capita Personal Income",
  "Net Migration Flow",
  "Resident Population",
  "New Private Housing Structures Authorized by Building Permits",
  "Homeownership Rate" ,
  "Burdened Households" ,
  "All-Transactions House Price Index",
  "Unemployment Rate"
)

# convert to wide and separate out change, value, deltas
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

# join us counties for area data (to compute densities)
joined_data_shapes_dt = inner_join(us_counties_shapefile, wide_data) %>% data.table()
names(wide_data)
cumulative_stats = joined_data_shapes_dt[, {
  
  starting_ur = value_x_Unemployment_Rate[!is.na(value_x_Unemployment_Rate)][1]
  ending_ur = tail(value_x_Unemployment_Rate[!is.na(value_x_Unemployment_Rate)], 1)
  delta_ur = ending_ur - starting_ur
  pct_change_ur = delta_ur/starting_ur
  
  starting_prices = value_x_All_Transactions_House_Price_Index[!is.na(value_x_All_Transactions_House_Price_Index)][1]
  ending_prices = tail(value_x_All_Transactions_House_Price_Index[!is.na(value_x_All_Transactions_House_Price_Index)], 1)
  
  starting_pop = (value_x_Resident_Population[!is.na(value_x_Resident_Population)][1])*1000
  ending_pop = (tail(value_x_Resident_Population[!is.na(value_x_Resident_Population)], 1))*1000
  
  total_units_authorized = sum(value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits, na.rm = T)
  delta_pop = (ending_pop - starting_pop)
  
  starting_income = value_x_Per_Capita_Personal_Income[!is.na(value_x_Per_Capita_Personal_Income)][1]
  ending_income = tail(value_x_Per_Capita_Personal_Income[!is.na(value_x_Per_Capita_Personal_Income)], 1)
  
  units_per_capita = total_units_authorized / (ending_pop)
  pct_obs_housing = length(value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits[!is.na(value_x_New_Private_Housing_Structures_Authorized_by_Building_Permits)])/length(year)
  
  starting_pop_density = (starting_pop) / aland[1]
  ending_pop_density = (ending_pop) / aland[1]
  
  
  list(
    starting_ur = starting_ur, 
    ending_ur = ending_ur,
    delta_ur = delta_ur, 
    pct_change_ur = pct_change_ur,
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
    units_per_delta_pop = total_units_authorized / (delta_pop),
    units_per_capita = units_per_capita,
    log_units_per_capita = log(ifelse(units_per_capita == 0, 1, units_per_capita)),
    pct_obs_housing = pct_obs_housing,
    ratio_pop_units = (delta_pop)/total_units_authorized,
    diff_pop_units = delta_pop - total_units_authorized,
    diff_units_delta_pop_per_starting_pop = (delta_pop - total_units_authorized) / (starting_pop)
  )
}, by = list(state_name, county_clean, decade)] %>%
  # compute quintiles by decade
  group_by(decade) %>%
  mutate(
    diff_units_delta_pop_per_starting_pop_cat = cut(diff_units_delta_pop_per_starting_pop, labels = F, breaks = quantile(diff_units_delta_pop_per_starting_pop, probs = seq(0, 1, by = 0.2), na.rm = T), include.lowest = T, right = F, ordered_result = T),
    diff_units_delta_pop_negative = sign(diff_units_delta_pop_per_starting_pop) == -1,
    ratio_pop_units = ifelse(is.infinite(ratio_pop_units), NA, ratio_pop_units),
    ratio_pop_units_cat = cut(ratio_pop_units, labels = F, breaks = quantile(ratio_pop_units, probs = seq(0, 1, by = 0.2), na.rm = T), include.lowest = T, right = F, ordered_result = T),
    ratio_pop_units_cat_labels = cut(ratio_pop_units, breaks = quantile(ratio_pop_units, probs = seq(0, 1, by = 0.2), na.rm = T), include.lowest = T, right = F, ordered_result = T),
    # set NA values to the midpoint (these mostly don't have units authorized data)
    ratio_pop_units_cat = ifelse(is.na(ratio_pop_units_cat), 3, ratio_pop_units_cat),
    ratio_pop_units_cat_pretty = paste0((ratio_pop_units_cat-1)*20, '-', ratio_pop_units_cat*20, '%'),
    units_per_delta_pop = ifelse(is.infinite(units_per_delta_pop), NA, units_per_delta_pop)
    # ratio_pop_units = ifelse(is.infinite(ratio_pop_units), NA, ratio_pop_units),
  ) %>%
  ungroup()

# rejoin shapefile to get the geometries back
cum_stats_shapes = left_join(us_counties_shapefile, cumulative_stats %>% as.data.frame())

# map population change vs. housing
exclude_states = c('Hawaii', 'Alaska', "Puerto Rico")
include_states = unique(cum_stats_shapes$state_name)
include_states = include_states[!include_states %in% exclude_states]

# get only 2010 data
decade_2010 = cum_stats_shapes %>% filter(decade == 2010, state_name %in% include_states)

pretty_labels = decade_2010 %>% st_set_geometry(NULL) %>% select(ratio_pop_units_cat, ratio_pop_units_cat_labels) %>% unique() %>%
  arrange(ratio_pop_units_cat) %>%
  filter(!is.na(ratio_pop_units_cat_labels)) %>%
  mutate(
    val1 = str_extract(ratio_pop_units_cat_labels, '.*,') %>% str_replace(',', '') %>% str_replace('^(.{1})', '') %>% as.numeric() %>% comma(accuracy = 2),
    val2 = str_extract(ratio_pop_units_cat_labels, ',.*') %>% str_replace(',', '') %>% str_replace('(.{1})$', '') %>% as.numeric() %>% comma(accuracy = 2),
    val_clean = paste(val1, val2, sep = ' to '),
    ratio_pop_units_fin = factor(val_clean, levels = val_clean)
  ) %>%
select(ratio_pop_units_cat, ratio_pop_units_fin) 
decade_2010 = left_join(decade_2010, pretty_labels)

population_vs_housing = ggplot() + 
  scale_fill_viridis_d(name = 'Population Change Per Unit') +
  labs(
    title = 'Population Added Per Housing Unit Authorized for Construction',
    subtitle = 'By County, 2010-2017',
    x = '\nTaylor G. White (github: @tgwhite)\nSource: St. Louis Federal Reserve'
  ) +
  theme_dark() + 
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  geom_sf(data = decade_2010, aes(fill = ratio_pop_units_fin)) +
  geom_sf(data = us_states_shapefile %>% filter(name %in% include_states), aes(), colour = 'white', size = .5, alpha = 0) 

ggsave('population_vs_housing.png', plot = population_vs_housing, height = 10, width = 12, units = 'in', dpi = 600)

diff_units_delta_pop_per_starting_pop_cat

ggplot() +
  geom_sf(data = decade_2010, aes(fill = diff_units_delta_pop_per_starting_pop_cat)) 

ggplot() +
  geom_sf(data = decade_2010, aes(fill = price_change)) 
filter(decade_2010, is.na(price_change))

ggplot(decade_2010, aes(diff_units_delta_pop_per_starting_pop, price_change)) + geom_point() + stat_smooth() + 
  coord_cartesian(xlim = c(-1, 1))

ggplot(cumulative_stats, aes(diff_units_delta_pop_per_starting_pop, price_change)) +
  facet_wrap(~decade) + 
  geom_point() + stat_smooth() + 
  coord_cartesian(xlim = c(-1, 1)) + 
  geom_quantile(quantiles = 0.5, colour = 'red')

### modeling

# cumulative_stats_long = melt(cumulative_stats, id = c('state_name', 'county_clean', 'decade', 'pct_change_pop'))


## units authorized model
options(na.action = na.exclude)
units_authorized_model = lm(total_units_authorized ~ decade + starting_ur*starting_pop_density*starting_income + delta_pop + delta_income, data = cumulative_stats, weights = pct_obs_housing)
summary(units_authorized_model)

cumulative_stats$predicted_units_authorized = predict(units_authorized_model)
cumulative_stats$error_prediction_units_authorized  = residuals(units_authorized_model)
cumulative_stats$error_predicted_authorized_over_starting_pop = with(cumulative_stats, error_prediction_units_authorized/starting_pop)
cumulative_stats$over_underbuilding_percent_cat = cut(cumulative_stats$error_predicted_authorized_over_starting_pop, ordered_result = T, right = F, include.lowest = T, breaks = quantile(cumulative_stats$error_predicted_authorized_over_starting_pop, probs = seq(0, 1, by = 0.2), na.rm = T))

cum_stats_shapes = left_join(us_counties_shapefile, cumulative_stats %>% as.data.frame())

ggplot() +
  geom_sf(data = filter(cum_stats_shapes, decade == 2010, state_name %in% include_states), aes(fill = over_underbuilding_percent_cat))


filter(cumulative_stats, state_name == 'California', decade == 2010) %>% 
  select(county_clean, total_units_authorized, predicted_units_authorized, error_prediction_units_authorized) %>%
  mutate(abs_error = abs(error_prediction_units_authorized)) %>% 
  arrange(abs_error) %>%
  head(15)

plot(predict(units_authorized_model), cumulative_stats$total_units_authorized)
hist(residuals(units_authorized_model))
plot(residuals(units_authorized_model))
plot(predict(units_authorized_model), residuals(units_authorized_model))

# find independent variation in diff_units_delta_pop_per_starting_pop

ggplot(cumulative_stats, aes(pct_change_pop, diff_units_delta_pop_per_starting_pop)) + geom_point()
a = lm(diff_units_delta_pop_per_starting_pop ~ predict(units_authorized_model) + pct_change_pop, data = cumulative_stats)
summary(a)
anova(a)

independent_model = lm(diff_units_delta_pop_per_starting_pop ~ pct_change_pop + pct_change_income + starting_prices*starting_pop_density + starting_income, data = cumulative_stats)
cumulative_stats$indepdendent_diff_units_preds = predict(independent_model)
cumulative_stats$independent_diff_units_errors = residuals(independent_model)
cumulative_stats$independent_pred_positive = sign(cumulative_stats$independent_diff_units_errors) == 1

cor(select(cumulative_stats, pct_change_pop, diff_units_delta_pop_per_starting_pop, pct_change_income, price_change, delta_ur, error_predicted_authorized_over_starting_pop) %>% na.omit())

# price change model
price_change_model = lm(price_change ~ delta_ur + decade + pct_change_pop + pct_change_income + starting_prices + starting_pop_density + starting_income + error_predicted_authorized_over_starting_pop, data = cumulative_stats)
summary(price_change_model)
ggplot(cumulative_stats, aes(delta_ur, price_change)) + 
  geom_point() + 
  facet_wrap(~decade) + stat_smooth(method = 'lm')
filter(cumulative_stats, delta_ur > 8)

plot(price_change_model)
ggplot(cumulative_stats, aes(error_prediction_units_authorized/starting_pop, price_change)) + geom_point() + stat_quantile() + 
  coord_cartesian(xlim = c(-1, 1))
summary(cumulative_stats$diff_units_delta_pop_per_starting_pop)

summary(cumulative_stats$diff_units_delta_pop_per_starting_pop)
a = anova(price_change_model) %>% pull(`Sum Sq`) 
a/sum(a)

plot(predict(price_change_model), cumulative_stats$price_change)

price_change_model2 = rq(price_change ~ decade + pct_change_pop + pct_change_income + starting_prices + starting_pop_density + starting_income + diff_units_delta_pop_per_starting_pop, data = cumulative_stats)
summary(price_change_model2)
summary(price_change_model)

# find independent variation in 
summary(independent_model)
plot(independent_model)
plot(predict(independent_model), cumulative_stats$diff_units_delta_pop_per_starting_pop)


# population change model 
ca = filter(cumulative_stats, state_name == 'California')
ggplot(ca, aes(decade, pct_change_pop, colour = county_clean)) + geom_line() + scale_colour_hue(guide = F)

population_change_model = lm(delta_pop ~ decade + starting_income*starting_pop_density + delta_income + starting_pop_density, data = cumulative_stats)

cumulative_stats$predicted_delta_pop = predict(population_change_model)
cumulative_stats$error_predicted_delta_pop = residuals(population_change_model)

cumulative_stats %>%
  select(delta_pop, predicted_delta_pop, error_predicted_delta_pop) %>%
  mutate(
    predicted_decrease = predicted_delta_pop < 0, 
    sign_agreement = sign(delta_pop) == sign(predicted_delta_pop)
  ) %>%
  group_by(
    predicted_decrease
  ) %>%
  summarize(
    pct_sign_agreement = mean(sign_agreement),
    RMSE = (mean(error_predicted_delta_pop^2))^(1/2)
  )

hist(cumulative_stats$delta_pop %>% log())

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
