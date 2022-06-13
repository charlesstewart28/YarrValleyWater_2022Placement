# Monthly Manual Dynamic Regression Model Builder for Non Resendtial Water Water
# YVW Placement RMIT University
# Authors: Charles Stewart - s3628786
#          Julian De Angelis - s362s3814844
# Date Published: 09/06/22

# Required Libraries
library(forecast)
library(readr)
library(lubridate)
library(stringr)
library(dplyr)
library(mondate)
library(zoo)



monthlyNonResForecastYVW <- function(data, initialPredictMonth, initialPredictYear, predictPeriod, trainPeriod) {
  
  # This function returns forecaster monthly Non Residential volume figures
  # Input parameters
  #   data = Previous bulk water, weather and connections data for full range
  #   initialPredictMonth = The month of the first forecast value
  #   initialPredictYear = The year of the first forecast value
  #   trainPeriod = Number of month used for training of dynamic regression model
  #   predictPeriod = Number of months to forecast ahead, Walk forward validation is used to calculate past first day.
  
  # Current weather data is pulled from six stations within the YVW network
  # A forecasting model is then built using dynamic regression
  
  initialPredict <- format(as.yearmon(as.character(paste(initialPredictYear,initialPredictMonth, sep='')),'%Y%m'),)
  initialPredict <- as.yearmon(initialPredict)
  
  # Subsets for given training periods
  
  data <- data %>% filter(Month >= as.yearmon(initialPredict - (trainPeriod/12))) %>% filter(Month <= as.yearmon(initialPredict) + ((predictPeriod-1)/12))
  data[data$Month >= as.yearmon(initialPredict), "NonRes"] <- 0
  
  # Walk forward validation XREG model
  # Walk through predicts a month ahead, saves this prediction, then rebuilds the model again for the next month
  
  for (i in (1:predictPeriod)) {
    currentModel <- auto.arima(data$NonRes[i:(trainPeriod+i-1)], xreg=data.matrix(data[i:(trainPeriod+i-1),5:7]))
    nextForecast <- forecast(currentModel, xreg=data.matrix(data[(trainPeriod+i),5:7]))
    data$NonRes[trainPeriod+i] <- nextForecast$mean[1]
    data$Predictions[trainPeriod+i] <- nextForecast$mean[1]
  }
  
  data[data$Month >= as.yearmon(initialPredict), "NonRes"] <- 0
  
  # Return orgional dataset with filled in prediction values for prediction period
  return(data)
  
}