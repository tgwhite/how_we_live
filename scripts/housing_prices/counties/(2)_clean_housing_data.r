
# to do -- use sqlite as storage to work around download/rate limit issues

library(tidyverse)
library(sqldf)
library(reshape2)
library(data.table)

setwd("~/how_we_live/Data")


fred_sqlite = dbConnect(SQLite(), dbname= "fred_sqlite.sqlite")
dbListTables(fred_sqlite)

us_county_indicators = dbGetQuery(fred_sqlite, 'select * from us_county_indicators')

# clean up the titles! 
us_county_indicators = mutate(
  us_county_indicators, 
  date = as.Date(date, origin = '1970-01-01'),
  title_clean = str_extract(title, '(.* in )|(.* for )') %>% str_replace('( in )|( for )', '')
)


# check to make sure all series found for all states
states_titles = group_by(us_county_indicators, state_name, title_clean) %>% summarize(obs = n())

titles_df = data.frame(title_clean = unique(states_titles$title_clean))

joined = full_join(states_titles, titles_df)

stopifnot(filter(joined, is.na(obs)) %>% nrow() == 0)

# arizona and california got inserted twice on an initial run
us_county_indicators_clean = unique(us_county_indicators)
# dbDisconnect(fred_sqlite)


# there are still duplicates somehow! 
wide_indicators = dcast(us_county_indicators_clean, date + state_name + county_name ~ title_clean, value.var = 'value')
sapply(wide_indicators, function(x){any(x > 1)}) # All-Transactions House Price Index

# get the duplicates
a = wide_indicators[wide_indicators$`All-Transactions House Price Index` > 1,]

b = inner_join(us_county_indicators_clean %>% filter(title_clean == 'All-Transactions House Price Index'), a) %>%
  arrange(date)

# this got looped in with Idaho somehow! 
# All-Transactions House Price Index for Honolulu County, HI

# All-Transactions House Price Index was not found in the hawaii download. 
d = filter(us_county_indicators_clean, state_name == 'Hawaii', county_name == 'Honolulu County/city, HI')

hawaii_in_idaho = filter(us_county_indicators_clean, state_name == 'Idaho' & str_detect(title, 'Honolulu')) %>%
  mutate(
    county_name = 'Honolulu County, HI',
    state_name = 'Hawaii' 
    # no need to change the county id apparently!! 27889 is already there
  )

all_cleaned_county_data = 
  bind_rows(
    filter(us_county_indicators_clean, !(state_name == 'Idaho' & str_detect(title, 'Honolulu'))),
    hawaii_in_idaho
  ) %>%
  mutate(
    title_for_col = paste0("x_", str_replace_all(title_clean, '[ \\-%]', '_'))
  )

all_cleaned_county_dt = arrange(all_cleaned_county_data, state_name, county_name, title_clean, date) %>% data.table()

lagged_values = all_cleaned_county_dt[, {
  list(
    date = date, 
    year = year(date),
    lag_year = lag(year(date), 1),
    value = value,
    lagged_value = dplyr::lag(value, 1)
  )
}, by = list(state_name, county_name, title_clean)] %>%
  mutate(
    year_diff = year - lag_year,
    delta = value - lagged_value, 
    percent_change = delta / lagged_value
  )


missing_data_issues = filter(lagged_values, year_diff > 1)

warning(paste('the percentage of missing data is:', nrow(missing_data_issues)/nrow(lagged_values)))

all_cleaned_county_data_fin = left_join(all_cleaned_county_data, lagged_values)

# there are multiple series that match this title for the given county in LA, excluding this data
# all_cleaned_county_data_fin = filter(all_cleaned_county_data_fin, title != 'Net Migration Flow for East Baton Rouge Parish, LA')

dbWriteTable(fred_sqlite, 'us_county_indicators_clean', all_cleaned_county_data_fin, overwrite = T)
dbDisconnect(fred_sqlite)

saveRDS(all_cleaned_county_data_fin, 'all_cleaned_county_data_fin.rds')
# # this works now
# wide_indicators = dcast(all_cleaned_county_data, date + state_name + county_name ~ title_for_col, value.var = 'value')
# head(wide_indicators)
# tail(wide_indicators)
