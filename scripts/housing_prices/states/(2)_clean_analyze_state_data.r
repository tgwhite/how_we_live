
library(tidyverse)
library(reshape2)
library(data.table)
library(tigris)
library(USAboundaries)
library(sqldf)
library(corrplot)
library(randomForest)
library(plm)
library(lmtest)
library(sandwich)
library(xgboost)

selected_vars = c(
  'Home Vacancy Rate', 'Per Capita Personal Income', 'Real Total Gross Domestic Product',
  'Rental Vacancy Rate', 'Resident Population', 'State Government Tax Collections, Property Taxes',
  'All-Transactions House Price Index', 'New Private Housing Units Authorized by Building Permits'
)

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

us_economic_data = dbGetQuery(fred_sqlite, 'select * from us_economic_data') %>%
  mutate(
    date = as.Date(date, origin = '1970-01-01'),
    year = year(date),
    title_for_col = paste0("x_", str_replace_all(title, '[ \\-%,]', '_'))
  ) %>%
  data.table()

dbDisconnect(fred_sqlite)


us_economic_data_lags = us_economic_data[, {
  
  lagged_value = dplyr::lag(value, 1)
  lagged2_value = dplyr::lag(value, 2)
  
  delta = value - lagged_value
  lagged_delta = dplyr::lag(delta, 1)
  pct_change = delta/lagged_value
  lagged_pct_change = dplyr::lag(pct_change, 1)
  
  list(
    date = date, 
    year = year(date),
    lag_year = dplyr::lag(year(date), 1),
    value = value, lagged_value = lagged_value, lagged2_value = lagged2_value,
    delta = delta, lagged_delta = lagged_delta,
    pct_change = pct_change, lagged_pct_change = lagged_pct_change
  )
  
}, by = list(title, title_for_col)] %>%
  mutate(
    year_diff = year - lag_year
  ) %>% 
  data.table()

stopifnot(nrow(filter(us_economic_data_lags, year_diff > 1)) == 0)


us_economic_data_wide = dcast.data.table(us_economic_data_lags, year ~ title_for_col, 
                                         value.var = c('value', 'lagged_value', 'lagged2_value', 'delta', 'lagged_delta', 'pct_change', 'lagged_pct_change'))


state_economic_data_dt = data.table(state_economic_data)

# select(state_economic_data, title_clean) %>% unique() %>% View()

state_lagged_values = state_economic_data_dt[, {
  lagged_value = dplyr::lag(value, 1)
  lagged_value2 = dplyr::lag(value, 2)
  
  delta = value - lagged_value
  lagged_delta = dplyr::lag(delta, 1)
  
  pct_change = delta/lagged_value
  lagged_pct_change = dplyr::lag(pct_change, 1)
  lagged_pct_change2 = dplyr::lag(pct_change, 2)
  
  list(
    date = date, 
    year = year(date),
    lag_year = dplyr::lag(year(date), 1),
    value = value, lagged_value = lagged_value, lagged_value2 = lagged_value2,
    delta = delta, lagged_delta = lagged_delta,
    pct_change = pct_change, lagged_pct_change = lagged_pct_change, lagged_pct_change2=lagged_pct_change2
  )
}, by = list(state_name, title_clean, title_for_col)] %>%
  mutate(
    year_diff = year - lag_year
  ) %>% 
  data.table()

# check the lags to make sure there aren't gaps
stopifnot(nrow(filter(state_lagged_values, year_diff > 1)) == 0)

state_wide_data = dcast.data.table(state_lagged_values, state_name + year ~ title_for_col,
                             value.var = c('value', 'lagged_value', 'delta', 'lagged_delta', 
                                           'pct_change', 'lagged_pct_change', 'lagged_pct_change2', 'lagged_value2')) %>%
  mutate(
    # each variable is in thousands 
    value_prop_tax_per_capita = value_x_State_Government_Tax_Collections__Property_Taxes / (value_x_Resident_Population),
    
    # gdp is in millions, population in thousands
    value_real_gdp_per_capita = value_x_Real_Total_Gross_Domestic_Product * 1000 / value_x_Resident_Population
  ) %>%
  left_join(us_economic_data_wide)

saveRDS(state_wide_data, 'state_wide_data.rds')

# get correlations by calculated value
values_only = select(state_wide_data, year, starts_with('value_'))
pct_change_only = select(state_wide_data, year, starts_with('pct_change'))
deltas_only = select(state_wide_data, year, starts_with('delta_'))


# select(wide_data, value_x_10_Year_Treasury_Constant_Maturity_Minus_2_Year_Treasury_Constant_Maturity, pct_change_x_Real_Total_Gross_Domestic_Product) %>% na.omit() %>% cor() %>% View()

values_corr = cor(values_only %>% na.omit())
pct_change_corr = cor(pct_change_only %>% na.omit())
deltas_corr = cor(deltas_only %>% na.omit())
write.csv(values_corr, 'values_corr.csv')

# there is a connection between per capita income and rental vacancy rates. 
# as income goes up, housing prices go up and vacancy rates go down
# however, vacancy rates do not explain prices directly

## levels models 
options(na.action = na.exclude)

values_model = lm(value_x_All_Transactions_House_Price_Index ~ value_x_Per_Capita_Personal_Income + value_x_Resident_Population + value_x_Home_Vacancy_Rate + value_x_Rental_Vacancy_Rate + year + value_x_Real_Total_Gross_Domestic_Product, data = values_only)
values_gdp_per_cap = lm(value_x_All_Transactions_House_Price_Index ~ value_real_gdp_per_capita + value_x_Resident_Population + value_x_Home_Vacancy_Rate + value_x_Rental_Vacancy_Rate + year, data = values_only)

# keep per capita income and gdp level
anova(values_model, values_gdp_per_cap)

vacancy_interaction_model = lm(value_x_All_Transactions_House_Price_Index ~ value_x_Per_Capita_Personal_Income + value_x_Resident_Population + value_x_Home_Vacancy_Rate*value_x_Rental_Vacancy_Rate + year + value_x_Real_Total_Gross_Domestic_Product, data = values_only)
income_population_interaction_model  = lm(value_x_All_Transactions_House_Price_Index ~ value_x_Per_Capita_Personal_Income * value_x_Resident_Population + value_x_Home_Vacancy_Rate + value_x_Rental_Vacancy_Rate + year + value_x_Real_Total_Gross_Domestic_Product, data = values_only)
full_interaction_model = lm(value_x_All_Transactions_House_Price_Index ~ value_x_Per_Capita_Personal_Income * value_x_Resident_Population + value_x_Home_Vacancy_Rate * value_x_Rental_Vacancy_Rate + year + value_x_Real_Total_Gross_Domestic_Product, data = values_only)

anova(values_model, vacancy_interaction_model)
anova(values_model, income_population_interaction_model) # no evidence that the housing/population interaction adds anything
anova(values_model, vacancy_interaction_model, full_interaction_model) # keep the vacancy interaction model

head(values_only)

summary(vacancy_interaction_model)

# logged housing price index is more normally distributed, lets try that
values_log = lm(log(value_x_All_Transactions_House_Price_Index) ~ value_x_Per_Capita_Personal_Income + value_x_Resident_Population + value_x_Home_Vacancy_Rate + value_x_Rental_Vacancy_Rate + year + value_x_Real_Total_Gross_Domestic_Product, data = values_only)

# log everything that's super right tailed 
values_logs_interaction = lm(log(value_x_All_Transactions_House_Price_Index) ~ log(value_x_Per_Capita_Personal_Income) + log(value_x_Resident_Population) + value_x_Home_Vacancy_Rate * value_x_Rental_Vacancy_Rate + year + log(value_x_Real_Total_Gross_Domestic_Product), data = values_only)
summary(values_logs_interaction)

anova(values_log, values_logs_interaction) # strong improvement

plot(values_logs_interaction) # diagnostic plots look excellent 
hist(residuals(values_logs_interaction))

anova_log_interactions = anova(values_logs_interaction)

# income explains the vast majority of the variation in housing prices
# the rental vacancy rate is the second strongest predictor, followed closely by GDP level
variance_explained_dat = tibble(
  variable = row.names(anova_log_interactions), 
  r_squared = anova_log_interactions$"Sum Sq"/sum(anova_log_interactions$"Sum Sq")
  ) %>% 
  arrange(-r_squared)

# variable                                              r_squared
# <chr>                                                     <dbl>
#   1 log(value_x_Per_Capita_Personal_Income)                 0.526  
# 2 Residuals                                               0.412  
# 3 log(value_x_Real_Total_Gross_Domestic_Product)          0.0230 
# 4 value_x_Rental_Vacancy_Rate                             0.0192 
# 5 value_x_Home_Vacancy_Rate                               0.00898
# 6 year                                                    0.00591
# 7 log(value_x_Resident_Population)                        0.00329
# 8 value_x_Home_Vacancy_Rate:value_x_Rental_Vacancy_Rate   0.00203

# per capita income explains most of the variation, followed by gdp and rental vacancy rates


# how does this model compare to those computed with a random forest?
training_data = filter(values_only, year <= 2014) %>% na.omit()

values_logs_interaction_train = lm(log(value_x_All_Transactions_House_Price_Index) ~ log(value_x_Per_Capita_Personal_Income) + log(value_x_Resident_Population) + value_x_Home_Vacancy_Rate * value_x_Rental_Vacancy_Rate + year + log(value_x_Real_Total_Gross_Domestic_Product), data = training_data)
summary(values_logs_interaction_train)

head(state_wide_data)

lagged_prices_only = lm(log(value_x_All_Transactions_House_Price_Index) ~ log(lagged_value_x_All_Transactions_House_Price_Index), data = state_wide_data)
prices_state = lm(log(value_x_All_Transactions_House_Price_Index) ~ log(lagged_value_x_All_Transactions_House_Price_Index) + state_name, data = state_wide_data)
lagged_prices_state_homevacancy = lm(log(value_x_All_Transactions_House_Price_Index) ~ log(lagged_value_x_All_Transactions_House_Price_Index) + lagged_value_x_Home_Vacancy_Rate + state_name, data = state_wide_data)

full_model = lm(log(value_x_All_Transactions_House_Price_Index) ~ log(lagged_value_x_All_Transactions_House_Price_Index) + log(lagged_value_x_Per_Capita_Personal_Income) + log(lagged_value_x_Resident_Population) + lagged_value_x_Home_Vacancy_Rate * lagged_value_x_Rental_Vacancy_Rate + log(lagged_value_x_Real_Total_Gross_Domestic_Product) + year + state_name, data = state_wide_data)
full_model_no_interaction  = lm(log(value_x_All_Transactions_House_Price_Index) ~ 
                                  log(lagged_value_x_All_Transactions_House_Price_Index) + 
                                  log(lagged_value_x_Per_Capita_Personal_Income) + 
                                  log(lagged_value_x_Resident_Population) + 
                                  lagged_value_x_Home_Vacancy_Rate + 
                                  lagged_value_x_Rental_Vacancy_Rate + 
                                  log(lagged_value_x_Real_Total_Gross_Domestic_Product) + 
                                  year + state_name, data = state_wide_data)

full_model_no_int_spreads_1  = lm(log(value_x_All_Transactions_House_Price_Index) ~ 
                                  log(lagged_value_x_All_Transactions_House_Price_Index) + 
                                  log(lagged_value_x_Per_Capita_Personal_Income) + 
                                  log(lagged_value_x_Resident_Population) + 
                                  lagged_value_x_Home_Vacancy_Rate + 
                                  lagged_value_x_Rental_Vacancy_Rate + 
                                  log(lagged_value_x_Real_Total_Gross_Domestic_Product) + 
                                  lagged_value_x_10_Year_Treasury_Constant_Maturity_Minus_2_Year_Treasury_Constant_Maturity +
                                  year + state_name, data = state_wide_data)

full_model_no_int_spreads_2  = lm(log(value_x_All_Transactions_House_Price_Index) ~ 
                                    log(lagged_value_x_All_Transactions_House_Price_Index) + 
                                    log(lagged_value_x_Per_Capita_Personal_Income) + 
                                    log(lagged_value_x_Resident_Population) + 
                                    lagged_value_x_Home_Vacancy_Rate + 
                                    lagged_value_x_Rental_Vacancy_Rate + 
                                    log(lagged_value_x_Real_Total_Gross_Domestic_Product) + 
                                    lagged2_value_x_10_Year_Treasury_Constant_Maturity_Minus_2_Year_Treasury_Constant_Maturity +
                                    year + state_name, data = state_wide_data)


head(state_wide_data)
summary(full_model_no_int_spreads_2)
names(state_wide_data)

state_panel_data = pdata.frame(state_wide_data, index = c('state_name', 'year'))

full_model_delta = plm(pct_change_x_All_Transactions_House_Price_Index ~ 
                        lagged_pct_change_x_All_Transactions_House_Price_Index +
                        # log(lagged_value_x_All_Transactions_House_Price_Index) +
                        log(lagged_value_x_Per_Capita_Personal_Income) +
                        lagged_pct_change_x_Per_Capita_Personal_Income +
                        lagged_value_x_Home_Vacancy_Rate +
                        # log(lagged_value_x_Resident_Population) +
                        lagged_pct_change_x_Resident_Population +
                        lagged2_value_x_10_Year_Treasury_Constant_Maturity_Minus_2_Year_Treasury_Constant_Maturity +
                        as.numeric(year)
                      
                        , data = state_panel_data, effect = 'individual'
                        )
summary(full_model_delta)
ca = filter(state_panel_data, state_name == 'California')
ggplot(ca, aes(lagged2_value_x_10_Year_Treasury_Constant_Maturity_Minus_2_Year_Treasury_Constant_Maturity, pct_change_x_Real_Total_Gross_Domestic_Product)) +
  geom_point() + 
  stat_smooth()
ggplot(state_panel_data, aes(lagged2_value_x_10_Year_Treasury_Constant_Maturity_Minus_2_Year_Treasury_Constant_Maturity))
ggplot(state_wide_data, aes(year, pct_change_x_All_Transactions_House_Price_Index, group = state_name)) + geom_line()
names(state_wide_data)
summary(full_model_delta)

head(us_economic_data)
ggplot(us_economic_data, aes(year, value)) + geom_line()
ggplot(state_wide_data, aes(lagged_pct_change_x_All_Transactions_House_Price_Index, pct_change_x_All_Transactions_House_Price_Index)) +
  geom_point()

ggplot(state_wide_data, aes(lagged2_value_x_10_Year_Treasury_Constant_Maturity_Minus_2_Year_Treasury_Constant_Maturity, pct_change_x_Real_Total_Gross_Domestic_Product)) +
  geom_point() + 
  stat_smooth()


n_distinct(state_wide_data$lagged2_value_x_10_Year_Treasury_Constant_Maturity_Minus_2_Year_Treasury_Constant_Maturity)

hist(state_wide_data$delta_x_All_Transactions_House_Price_Index)
qqnorm(state_wide_data$delta_x_All_Transactions_House_Price_Index[])
a = state_wide_data$delta_x_All_Transactions_House_Price_Index
a[a == 0] = 0.25
qqnorm(log(a))
hist(log(a))
hist(a)

group_by(state_wide_data, spreads_less_one) %>%
  summarize(
    mean_growth = mean(pct_change_x_Real_Total_Gross_Domestic_Product, na.rm = T),
    mean_prices = mean(pct_change_x_All_Transactions_House_Price_Index, na.rm = T)
  )


# lagged spreads in one year doesn't explain prices
# interaction model doesn't help
anova(lagged_prices_only, full_model, full_model_no_interaction, full_model_no_int_spreads_1)

# spreads lagged two years help!
anova(lagged_prices_only, full_model, full_model_no_interaction, full_model_no_int_spreads_2) 

summary(full_model_no_int_spreads_2)

anova_full_model_no_int_spreads_2 = anova(full_model_no_int_spreads_2)

variance_explained_dat = tibble(
  variable = row.names(anova_full_model_no_int_spreads_2), 
  r_squared = anova_full_model_no_int_spreads_2$"Sum Sq"/sum(anova_full_model_no_int_spreads_2$"Sum Sq")
) %>% 
  arrange(-r_squared)

plot(full_model_no_interaction) # diagnostics look excellent


head(state_wide_data)

vars_to_keep = c('value_x_All_Transactions_House_Price_Index', 
                 'lagged_value_x_All_Transactions_House_Price_Index',
                 'lagged_value_x_Per_Capita_Personal_Income', 
                 'lagged_value_x_Resident_Population',
                 'lagged_value_x_Home_Vacancy_Rate', 
                 'lagged_value_x_Rental_Vacancy_Rate',
                 'lagged_value_x_Real_Total_Gross_Domestic_Product',
                 'year', 'state_name'
                 )
split_year = 2004
training_data = filter(state_wide_data, year <= split_year)[,vars_to_keep] %>% na.omit()

test_data = filter(state_wide_data, year > split_year)[,vars_to_keep] %>% na.omit()

# test missingness
test_data_all_vals = filter(state_wide_data, year > split_year)[,vars_to_keep]
missing_values_by_year = group_by(test_data_all_vals, year) %>% summarize_all(.funs = function(x){sum(is.na(x))})

# per capita personal income and gdp are holding back the model....
### one solution -- predict those out a year ahead
### OR 

# the test data only goes to 2017....

full_model_no_interaction  = lm(log(value_x_All_Transactions_House_Price_Index) ~ log(lagged_value_x_All_Transactions_House_Price_Index) + log(lagged_value_x_Per_Capita_Personal_Income) + log(lagged_value_x_Resident_Population) + lagged_value_x_Home_Vacancy_Rate + lagged_value_x_Rental_Vacancy_Rate + log(lagged_value_x_Real_Total_Gross_Domestic_Product) + year + state_name, data = training_data)

model_matrix_mls = model.matrix(value_x_All_Transactions_House_Price_Index ~ ., data = training_data)

housing_values_boost = xgboost(data = model_matrix_mls, label = select(training_data, value_x_All_Transactions_House_Price_Index) %>% as.matrix(), nrounds = 1000)

rf_model_matrix = cbind(model_matrix_mls, select(training_data, value_x_All_Transactions_House_Price_Index) %>% as.matrix())
housing_values_rf = randomForest(value_x_All_Transactions_House_Price_Index ~ ., data = data.frame(rf_model_matrix), ntree = 1000)

# now predict test data
test_data_model_matrix = model.matrix(value_x_All_Transactions_House_Price_Index ~ ., data = test_data)

rf_model_matrix_test = cbind(test_data_model_matrix, select(test_data, value_x_All_Transactions_House_Price_Index) %>% as.matrix())


prediction_data = test_data %>%
  mutate(
    predicted_boost = predict(housing_values_boost, newdata = test_data_model_matrix),
    predicted_rf = predict(housing_values_rf, newdata = data.frame(rf_model_matrix_test)),
    predicted_lm = exp(predict(full_model_no_interaction, newdata = test_data)),
    error_boost = value_x_All_Transactions_House_Price_Index - predicted_boost,
    error_rf = value_x_All_Transactions_House_Price_Index - predicted_rf,
    error_lm = value_x_All_Transactions_House_Price_Index - predicted_lm,
    percent_error_lm = error_lm/value_x_All_Transactions_House_Price_Index
  )

# get root mean squared errors for each model
select(prediction_data, contains('error')) %>% summarize_all(.funs = function(x){mean(x^2)^(1/2)})

select(prediction_data, value_x_All_Transactions_House_Price_Index, contains('predicted')) %>% cor()

predicted_long = select(prediction_data, state_name, year, value_x_All_Transactions_House_Price_Index, contains('predicted')) %>%
  melt(
    id = c('state_name', 'year', 'value_x_All_Transactions_House_Price_Index')
  )

ggplot(predicted_long, aes(value, value_x_All_Transactions_House_Price_Index)) + 
  facet_wrap(~variable) + 
  geom_point() + 
  stat_smooth()

ca = filter(prediction_data, state_name == 'California')

library(plotly)
the_plot = ggplot(ca) +
  geom_line(aes(year, value_x_All_Transactions_House_Price_Index), colour = 'black') +
  geom_line(aes(year, predicted_lm), colour = 'orange') +
  geom_line(aes(year, predicted_boost), colour = 'purple') +
  geom_line(aes(year, predicted_rf), colour = 'forestgreen') 
ggplotly(the_plot)


ggplot(all_ca) + 
  geom_point(aes(year, delta_x_All_Transactions_House_Price_Index)) + 
  geom_point(data = ca, aes(year, error_lm), colour = 'red') +
  scale_x_continuous(limits = c(1975, 2019)) + 
  labs(
    y = 'Year on Year Change in Housing Price Index',
    x = 'Year',
    title = 'Predicting House Prices in California',
    subtitle = 'Actual Year on Year Changes in Black, Prediction Errors in Red'
  )
