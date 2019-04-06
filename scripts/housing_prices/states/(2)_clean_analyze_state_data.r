
library(tidyverse)
# library(reshape2)
library(data.table)
library(tigris)
library(USAboundaries)
library(sqldf)
library(corrplot)

selected_vars = c(
  'Home Vacancy Rate', 'Per Capita Personal Income', 'Real Total Gross Domestic Product',
  'Rental Vacancy Rate', 'Resident Population', 'State Government Tax Collections, Property Taxes',
  'All-Transactions House Price Index', 'New Private Housing Units Authorized by Building Permits'
)
filter(state_economic_data, title_clean == 'Real Total Gross Domestic Product') %>% pull(units) %>% unique()

# get shapefile for any plots
us_states_shapefile = us_states()


setwd("~/how_we_live/Data")

fred_sqlite = dbConnect(SQLite(), dbname= "fred_sqlite.sqlite")

state_economic_data = dbGetQuery(fred_sqlite, 'select * from state_economic_data') %>%
  mutate(
    date = as.Date(date, origin = '1970-01-01'),
    title_clean = str_extract(title, '(.* in )|(.* for )') %>% str_replace('( in )|( for )', ''),
    title_for_col = paste0("x_", str_replace_all(title_clean, '[ \\-%,]', '_'))
  ) %>% 
  filter(title_clean %in% selected_vars) %>%
  arrange(state_name, title_clean, date)

dbDisconnect(fred_sqlite)

state_economic_data_dt = data.table(state_economic_data)

# select(state_economic_data, title_clean) %>% unique() %>% View()

lagged_values = state_economic_data_dt[, {
  lagged_value = dplyr::lag(value, 1)
  delta = value - lagged_value
  lagged_delta = dplyr::lag(delta, 1)
  pct_change = delta/lagged_value
  lagged_pct_change = dplyr::lag(pct_change, 1)
  
  list(
    date = date, 
    year = year(date),
    lag_year = lag(year(date), 1),
    value = value, lagged_value = lagged_value, 
    delta = delta, lagged_delta = lagged_delta,
    pct_change = pct_change, lagged_pct_change = lagged_pct_change
  )
}, by = list(state_name, title_clean, title_for_col)] %>%
  mutate(
    year_diff = year - lag_year
  ) %>% 
  data.table()

# check the lags to make sure there aren't gaps
stopifnot(nrow(filter(lagged_values, year_diff > 1)) == 0)

head(lagged_values)
wide_data = dcast.data.table(lagged_values, state_name + year ~ title_for_col,
                             value.var = c('value', 'lagged_value', 'delta', 'lagged_delta', 'pct_change', 'lagged_pct_change')) %>%
  mutate(
    # each variable is in thousands 
    value_prop_tax_per_capita = value_x_State_Government_Tax_Collections__Property_Taxes / (value_x_Resident_Population),
    
    # gdp is in millions, population in thousands
    value_real_gdp_per_capita = value_x_Real_Total_Gross_Domestic_Product * 1000 / value_x_Resident_Population
  )

# get correlations by calculated value
values_only = select(wide_data, year, starts_with('value_'))
pct_change_only = select(wide_data, year, starts_with('pct_change'))
deltas_only = select(wide_data, year, starts_with('delta_'))

values_corr = cor(values_only %>% na.omit())
pct_change_corr = cor(pct_change_only %>% na.omit())
deltas_corr = cor(deltas_only %>% na.omit())
write.csv(values_corr, 'values_corr.csv')
shell('explorer .')
View(values_corr)
View(pct_change_corr)
View(deltas_corr)

# there is a connection between per capita income and rental vacancy rates. 
# as income goes up, housing prices go up and vacancy rates go down
# however, vacancy rates do not explain prices directly

## levels models 
options(na.option = na.exclude)
house_price_level_model = lm(value_x_All_Transactions_House_Price_Index ~ value_x_Per_Capita_Personal_Income*value_x_Resident_Population + value_x_Home_Vacancy_Rate*value_x_Rental_Vacancy_Rate + year + value_x_Real_Total_Gross_Domestic_Product, data = values_only)

summary(house_price_level_model)
