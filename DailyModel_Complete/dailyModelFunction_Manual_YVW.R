# Daily Manual Dynamic Regression Model Builder
# YVW Placement RMIT University
# Authors: Charles Stewart - s3628786
#          Julian De Angelis - s362s3814844
# Date Published: 27/04/22

# Required Libraries
library(forecast)
library(readr)
library(lubridate)
library(stringr)
library(dplyr)

# Data input must be pre cleaned and sorted

dailyForecastYVW <- function(data, initialPredictDate, predictPeriod, trainPeriod) {
  
  # This function returns forecasted daily bulk volume figures
  # Input parameters
  #   data = Previous bulk water values and weather data for full range
  #   initialPredictDate = The date of the first forecast value
  #                      = Default value is 01/01/22
  #   trainPeriod = Number of days used for training of dynamic regression model
  #               = Default value fo 90 days, calculated as best train length in previous testing.
  #   predictPeriod = Number of days to forecast ahead, Walk forward validation is used to calculate past first day.
  #                 = Default value is for 7 days
  
  # Current weather data is pulled from six stations within the YVW network
  # A forecasting model is then built using dynamic regression
  # Parameter used are explained below
  
  
  # Subsets for given training periods
  data <- data %>% filter(Date >= ymd(as.Date(initialPredictDate)-trainPeriod)) %>% filter(Date <= ymd(as.Date(initialPredictDate)+predictPeriod-1))
  data[data$Date >= ymd(as.Date(initialPredictDate)), "BilledVolume"] <- 0
  
  # Walk forward validation XREG model
  # Walk through predicts a day ahead, saves this prediction, then rebuilds the model again for the next day
  
  for (i in (1:predictPeriod)) {
    currentModel <- auto.arima(data$BilledVolume[i:(trainPeriod+i-1)], xreg=data.matrix(data[i:(trainPeriod+i-1),3:9]))
    nextForecast <- forecast(currentModel, xreg=data.matrix(data[(trainPeriod+i),3:9]))
    data$BilledVolume[trainPeriod+i] <- nextForecast$mean[1]
    data$Predictions[trainPeriod+i] <- nextForecast$mean[1]
  }
  
  data[data$Date >= ymd(as.Date(initialPredictDate)), "BilledVolume"] <- 0
  
  # Return orgional dataset with filled in prediction values for prediction period
  return(data)
}

