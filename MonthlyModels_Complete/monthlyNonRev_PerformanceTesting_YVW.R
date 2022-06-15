# Monthly Non Rev Model Performance Testing
# YVW Placement RMIT University
# Authors: Charles Stewart - s3628786
#          Julian De Angelis - s362s3814844
# Latest Update: 12/06/22

# Required Libraries

source('monthlyModelFunction_BulkWater_Manual_YVW.R')
source('monthlyModelFunction_NonResWater_Manual_YVW.R')
source('monthlyModelFunction_ResWater_Manual_YVW.R')
library(ggplot2)
library(forecast)
library(readr)
library(lubridate)
library(stringr)
library(dplyr)
library(MLmetrics)
library(gridExtra)


# Read in monthly full data

monthlyFullData <- read_csv("monthlyFullData_May2021Jan2022_YVW.csv")

monthlyFullData$Month <- as.yearmon(monthlyFullData$Month)
monthlyFullData$Predictions <- NA
predictPeriod <- 9

# Forecast 9 month period - bulk water
monthlyFullData <- monthlyBulkForecastYVW(monthlyFullData, initialPredictMonth = '05', initialPredictYear = '2021', predictPeriod = 9, trainPeriod = 48)
colnames(monthlyFullData)[12] <- 'BulkForecast'

# Forecast 9 month period - res water
monthlyFullData$Predictions <- NA
monthlyFullData <- monthlyResForecastYVW(monthlyFullData, initialPredictMonth = '05', initialPredictYear = '2021', predictPeriod = 9, trainPeriod = 48)
colnames(monthlyFullData)[13] <- 'ResForecast'

# Forecast 9 month period - non res water
monthlyFullData$Predictions <- NA
monthlyFullData <- monthlyNonResForecastYVW(monthlyFullData, initialPredictMonth = '05', initialPredictYear = '2021', predictPeriod = 9, trainPeriod = 48)
colnames(monthlyFullData)[14] <- 'NonResForecast'

# Read back in bulk, res and non res
tempDF <- read_csv("monthlyFullData_May2021Jan2022_YVW.csv")

monthlyFullData[c(49:57),c(2:4)] <- tempDF[c(50:58), c(2:4)]

monthlyFullData$NonRevPred <- NA

for (i in 49:57) {
  monthlyFullData$NonRevPred[i] <- monthlyFullData$BulkForecast[i] - sum(monthlyFullData[i,c(8,9,10,13,14)])
}

# Produce RMSE and MAPE scores
NonRevPerformance <- data.frame(matrix(ncol = 2, nrow = 1))
colnames(NonRevPerformance) <- c('MAPE', 'RMSE')
NonRevPerformance[1,1] <- MAPE(monthlyFullData$NonRevPred[49:57], monthlyFullData$NonRev[49:57])
NonRevPerformance[1,2] <- RMSE(monthlyFullData$NonRevPred[49:57], monthlyFullData$NonRev[49:57])

png('monthlyNonRev_PerformanceTesting_YVW.png', height = 20*nrow(NonRevPerformance), width=80*ncol(NonRevPerformance))




