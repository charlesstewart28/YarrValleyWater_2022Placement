# Monthly Non Res Model Performance Testing Across Training Periods
# YVW Placement RMIT University
# Authors: Charles Stewart - s3628786
#          Julian De Angelis - s362s3814844
# Latest Update: 07/06/22

# Required Libraries

source('monthlyModelFunction_NonResWater_Manual_YVW.R')
library(ggplot2)
library(forecast)
library(readr)
library(lubridate)
library(stringr)
library(dplyr)
library(MLmetrics)
library(gridExtra)


# Read in monthly non residential data

monthlyData <- read.csv('monthlyData.csv')
monthlyData$Month <- as.yearmon(monthlyData$Month)
monthlyData$Predictions <- NA

# Set up initial month to predict

initialDates <- data.frame(c('08', '11', '12', '09', '01'), c('2017', '2018', '2019', '2020', '2021'))
colnames(initialDates) <- c('Month','Year')
initialDates$Date<- NA

for (i in 1:length(initialDates$Month)) {
  initialDates$Date[i] <- format(as.yearmon(as.character(paste(initialDates$Year[i],initialDates$Month[i], sep='')),'%Y%m'),)
}

initialDates$Date <- as.yearmon(initialDates$Date)

predictPeriod <- 12

# Predicting for 12 months using 6 months training

pred6 <- data.frame(matrix(ncol=10,nrow=12))
colnames(pred6) <- c('BilledVolumeP1', 'P1',
                     'BilledVolumeP2', 'P2',
                     'BilledVolumeP3', 'P3',
                     'BilledVolumeP1', 'P4',
                     'BilledVolumeP5', 'P5')


for (i in 1:length(initialDates$Month)) {
  pred6[,((2*i)-1)] <- monthlyData %>% filter(Month >= as.yearmon(initialDates$Date[i]))  %>% filter(Month <= as.yearmon(initialDates$Date[i] + (predictPeriod-1)/12)) %>% select(NonRes)
  tempForecastSet <- monthlyNonResForecastYVW(monthlyData, initialPredictMonth = initialDates$Month[i], initialPredictYear = initialDates$Year[i], predictPeriod = predictPeriod, trainPeriod = 6)
  pred6[,(2*i)] <- tail(tempForecastSet$Predictions, 12)
}



# Predicting for 12 months using 12 months training

pred12 <- data.frame(matrix(ncol=10,nrow=12))
colnames(pred12) <- c('BilledVolumeP1', 'P1',
                      'BilledVolumeP2', 'P2',
                      'BilledVolumeP3', 'P3',
                      'BilledVolumeP1', 'P4',
                      'BilledVolumeP5', 'P5')


for (i in 1:length(initialDates$Month)) {
  pred12[,((2*i)-1)] <- monthlyData %>% filter(Month >= as.yearmon(initialDates$Date[i]))  %>% filter(Month <= as.yearmon(initialDates$Date[i] + (predictPeriod-1)/12)) %>% select(NonRes)
  tempForecastSet <- monthlyNonResForecastYVW(monthlyData, initialPredictMonth = initialDates$Month[i], initialPredictYear = initialDates$Year[i], predictPeriod = predictPeriod, trainPeriod = 12)
  pred12[,(2*i)] <- tail(tempForecastSet$Predictions, 12)
}



# Predicting for 12 months using 18 months training

pred18 <- data.frame(matrix(ncol=10,nrow=12))
colnames(pred18) <- c('BilledVolumeP1', 'P1',
                      'BilledVolumeP2', 'P2',
                      'BilledVolumeP3', 'P3',
                      'BilledVolumeP1', 'P4',
                      'BilledVolumeP5', 'P5')


for (i in 2:length(initialDates$Month)) {
  pred18[,((2*i)-1)] <- monthlyData %>% filter(Month >= as.yearmon(initialDates$Date[i]))  %>% filter(Month <= as.yearmon(initialDates$Date[i] + (predictPeriod-1)/12)) %>% select(NonRes)
  tempForecastSet <- monthlyNonResForecastYVW(monthlyData, initialPredictMonth = initialDates$Month[i], initialPredictYear = initialDates$Year[i], predictPeriod = predictPeriod, trainPeriod = 18)
  pred18[,(2*i)] <- tail(tempForecastSet$Predictions, 12)
}



# Predicting for 12 months using 24 months training

pred24 <- data.frame(matrix(ncol=10,nrow=12))
colnames(pred24) <- c('BilledVolumeP1', 'P1',
                      'BilledVolumeP2', 'P2',
                      'BilledVolumeP3', 'P3',
                      'BilledVolumeP1', 'P4',
                      'BilledVolumeP5', 'P5')


for (i in 2:length(initialDates$Month)) {
  pred24[,((2*i)-1)] <- monthlyData %>% filter(Month >= as.yearmon(initialDates$Date[i]))  %>% filter(Month <= as.yearmon(initialDates$Date[i] + (predictPeriod-1)/12)) %>% select(NonRes)
  tempForecastSet <- monthlyNonResForecastYVW(monthlyData, initialPredictMonth = initialDates$Month[i], initialPredictYear = initialDates$Year[i], predictPeriod = predictPeriod, trainPeriod = 24)
  pred24[,(2*i)] <- tail(tempForecastSet$Predictions, 12)
}




# Predicting for 12 months using 36 months training

pred36 <- data.frame(matrix(ncol=10,nrow=12))
colnames(pred36) <- c('BilledVolumeP1', 'P1',
                      'BilledVolumeP2', 'P2',
                      'BilledVolumeP3', 'P3',
                      'BilledVolumeP1', 'P4',
                      'BilledVolumeP5', 'P5')


for (i in 3:length(initialDates$Month)) {
  pred36[,((2*i)-1)] <- monthlyData %>% filter(Month >= as.yearmon(initialDates$Date[i]))  %>% filter(Month <= as.yearmon(initialDates$Date[i] + (predictPeriod-1)/12)) %>% select(NonRes)
  tempForecastSet <- monthlyNonResForecastYVW(monthlyData, initialPredictMonth = initialDates$Month[i], initialPredictYear = initialDates$Year[i], predictPeriod = predictPeriod, trainPeriod = 36)
  pred36[,(2*i)] <- tail(tempForecastSet$Predictions, 12)
}


# Predicting for 12 months using 48 months training

pred48 <- data.frame(matrix(ncol=10,nrow=12))
colnames(pred48) <- c('BilledVolumeP1', 'P1',
                      'BilledVolumeP2', 'P2',
                      'BilledVolumeP3', 'P3',
                      'BilledVolumeP1', 'P4',
                      'BilledVolumeP5', 'P5')


for (i in 4:length(initialDates$Month)) {
  pred48[,((2*i)-1)] <- monthlyData %>% filter(Month >= as.yearmon(initialDates$Date[i]))  %>% filter(Month <= as.yearmon(initialDates$Date[i] + (predictPeriod-1)/12)) %>% select(NonRes)
  tempForecastSet <- monthlyNonResForecastYVW(monthlyData, initialPredictMonth = initialDates$Month[i], initialPredictYear = initialDates$Year[i], predictPeriod = predictPeriod, trainPeriod = 48)
  pred48[,(2*i)] <- tail(tempForecastSet$Predictions, 12)
}


predictions <- list(pred6, pred12, pred18, pred24, pred36, pred48)

mapePerformance <- data.frame(matrix(ncol=5,nrow=6))
colnames(mapePerformance) <- c('Aug2017', 'Nov2018', 'Dec2019', 'Sep2020','Jan2021')
rownames(mapePerformance) <- c('pred6', 'pred12', 'pred18', 'pred24', 'pred36', 'pred48')

for (i in 1:6) {
  for (j in 1:5) {
    mapePerformance[i,j] <- MAPE(predictions[[i]][[(2*j)]], predictions[[i]][[((2*j)-1)]])
  }
}

mapeMeans <- data.frame(matrix(ncol=1, nrow=6))
colnames(mapeMeans) <- c('MapeMean')
rownames(mapeMeans) <- c('pred6', 'pred12', 'pred18', 'pred24', 'pred36', 'pred48')

mapeMeans$MapeMean <- rowMeans(mapePerformance, na.rm=TRUE)
mapePerformance$MapeMean <- rowMeans(mapePerformance, na.rm=TRUE)

png('monthlyNonResPerformanceMAPE_YVW.png', height = 50*nrow(mapePerformance), width=200*ncol(mapePerformance))
grid.table(mapePerformance)
dev.off()



RMSEPerformance <- data.frame(matrix(ncol=5,nrow=6))
colnames(RMSEPerformance) <- c('Aug2017', 'Nov2018', 'Dec2019', 'Sep2020','Jan2021')
rownames(RMSEPerformance) <- c('pred6', 'pred12', 'pred18', 'pred24', 'pred36', 'pred48')

for (i in 1:6) {
  for (j in 1:5) {
    RMSEPerformance[i,j] <- RMSE(predictions[[i]][[(2*j)]], predictions[[i]][[((2*j)-1)]])
  }
}

RMSEMeans <- data.frame(matrix(ncol=1, nrow=6))
colnames(RMSEMeans) <- c('RMSEMean')
rownames(RMSEMeans) <- c('pred6', 'pred12', 'pred18', 'pred24', 'pred36', 'pred48')

RMSEMeans$RMSEMean <- rowMeans(RMSEPerformance, na.rm=TRUE)
RMSEPerformance$RMSEMean <- rowMeans(RMSEPerformance, na.rm=TRUE)

png('monthlyNonResPerformanceRMSE_YVW.png', height = 50*nrow(RMSEPerformance), width=200*ncol(RMSEPerformance))
grid.table(RMSEPerformance)
dev.off()

