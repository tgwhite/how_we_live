
library(tidyverse)
library(randomForest)
library(xgboost)
list.files()
setwd("~/how_we_live/data")
us_housing_economic_data = read.csv('us_housing_economic_data.csv', na.strings ='.') %>%
  mutate(
    DATE = as.Date(DATE, format = '%m/%d/%Y')
  )
str(us_housing_economic_data)
?xgboost
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
dim(train$data)
class(train$data)[1]
head(train$data)
train
?read.csv
