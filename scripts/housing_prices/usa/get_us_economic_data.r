
library(tidyverse)
library(fredr)
library(sqldf)

continue_download = F
clean_existing_data = F

setwd("~/how_we_live/Data")


fred_sqlite = dbConnect(SQLite(), dbname= "fred_sqlite.sqlite")

fredr_set_key('0437e7baffc7066bacb86efa56cc37c9')

economy_wide_predictors = c('T10Y2Y', 'DGS10')
economy_wide_stacked = map(economy_wide_predictors, function(series){
  series_info = fredr_series_search_text(series)  
  the_download = fredr_series_observations(series, frequency = 'a', aggregation_method = 'eop') %>%
    left_join(series_info, by = c('series_id' = 'id'))
  return(the_download)
}) %>% 
  bind_rows()

dbWriteTable(fred_sqlite, 'us_economic_data', economy_wide_stacked)

dbDisconnect(fred_sqlite)
