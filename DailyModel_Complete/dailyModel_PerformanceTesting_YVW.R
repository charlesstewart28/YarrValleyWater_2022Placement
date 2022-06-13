# Daily Model Performance Testing Across Training Periods
# YVW Placement RMIT University
# Authors: Charles Stewart - s3628786
#          Julian De Angelis - s362s3814844
# Latest Update: 16/05/22

# Required Libraries

source('dailyModelFunction_Manual_YVW.R')
library(ggplot2)
library(forecast)
library(readr)
library(lubridate)
library(stringr)
library(dplyr)
library(MLmetrics)
library(gridExtra)

billedWaterData <- read.csv('dailyModelData.csv')
billedWaterData <- billedWaterData[,c(1:3,8:10,12:14)]
billedWaterData$Predictions <- NA


dates <- c("2016/01/01", "2016/06/01", "2017/03/01", "2018/01/01", "2019/07/01", "2020/03/01", "2021/07/01")
initialDates <- data.frame(dates)
colnames(initialDates) <- ('InitialDate')
predictPeriod <- 7


pred30 <- data.frame(matrix(ncol=14,nrow=7))
colnames(pred30) <- c('BilledVolumeP1', 'P1',
                                'BilledVolumeP2', 'P2',
                                'BilledVolumeP3', 'P3',
                                'BilledVolumeP1', 'P4',
                                'BilledVolumeP5', 'P5',
                                'BilledVolumeP6', 'P6',
                                'BilledVolumeP7', 'P7')



for (i in 1:length(initialDates$InitialDate)) {
    pred30[,((2*i)-1)] <- billedWaterData %>% filter(Date >= ymd(as.Date(initialDates$InitialDate[i])))  %>% filter(Date <= ymd(as.Date(initialDates$InitialDate[i])+predictPeriod-1)) %>% select(BilledVolume)
    tempForecastSet <- dailyForecastYVW(billedWaterData, initialPredictDate = initialDates$InitialDate[i], predictPeriod = predictPeriod, trainPeriod =30)
    pred30[,(2*i)] <- tail(tempForecastSet$Predictions, 7)
}



pred60 <- data.frame(matrix(ncol=14,nrow=7))
colnames(pred60) <- c('BilledVolumeP1', 'P1',
                      'BilledVolumeP2', 'P2',
                      'BilledVolumeP3', 'P3',
                      'BilledVolumeP1', 'P4',
                      'BilledVolumeP5', 'P5',
                      'BilledVolumeP6', 'P6',
                      'BilledVolumeP7', 'P7')

for (i in 1:length(initialDates$InitialDate)) {
  pred60[,((2*i)-1)] <- billedWaterData %>% filter(Date >= ymd(as.Date(initialDates$InitialDate[i])))  %>% filter(Date <= ymd(as.Date(initialDates$InitialDate[i])+predictPeriod-1)) %>% select(BilledVolume)
  tempForecastSet <- dailyForecastYVW(billedWaterData, initialPredictDate = initialDates$InitialDate[i], predictPeriod = predictPeriod, trainPeriod = 60)
  pred60[,(2*i)] <- tail(tempForecastSet$Predictions, 7)
}



pred90 <- data.frame(matrix(ncol=14,nrow=7))
colnames(pred90) <- c('BilledVolumeP1', 'P1',
                      'BilledVolumeP2', 'P2',
                      'BilledVolumeP3', 'P3',
                      'BilledVolumeP1', 'P4',
                      'BilledVolumeP5', 'P5',
                      'BilledVolumeP6', 'P6',
                      'BilledVolumeP7', 'P7')

for (i in 1:length(initialDates$InitialDate)) {
  pred90[,((2*i)-1)] <- billedWaterData %>% filter(Date >= ymd(as.Date(initialDates$InitialDate[i])))  %>% filter(Date <= ymd(as.Date(initialDates$InitialDate[i])+predictPeriod-1)) %>% select(BilledVolume)
  tempForecastSet <- dailyForecastYVW(billedWaterData, initialPredictDate = initialDates$InitialDate[i], predictPeriod = predictPeriod, trainPeriod = 90)
  pred90[,(2*i)] <- tail(tempForecastSet$Predictions, 7)
}



pred120 <- data.frame(matrix(ncol=14,nrow=7))
colnames(pred120) <- c('BilledVolumeP1', 'P1',
                      'BilledVolumeP2', 'P2',
                      'BilledVolumeP3', 'P3',
                      'BilledVolumeP1', 'P4',
                      'BilledVolumeP5', 'P5',
                      'BilledVolumeP6', 'P6',
                      'BilledVolumeP7', 'P7')

for (i in 1:length(initialDates$InitialDate)) {
  pred120[,((2*i)-1)] <- billedWaterData %>% filter(Date >= ymd(as.Date(initialDates$InitialDate[i])))  %>% filter(Date <= ymd(as.Date(initialDates$InitialDate[i])+predictPeriod-1)) %>% select(BilledVolume)
  tempForecastSet <- dailyForecastYVW(billedWaterData, initialPredictDate = initialDates$InitialDate[i], predictPeriod = predictPeriod, trainPeriod = 120)
  pred120[,(2*i)] <- tail(tempForecastSet$Predictions, 7)
}


pred180 <- data.frame(matrix(ncol=14,nrow=7))
colnames(pred180) <- c('BilledVolumeP1', 'P1',
                       'BilledVolumeP2', 'P2',
                       'BilledVolumeP3', 'P3',
                       'BilledVolumeP1', 'P4',
                       'BilledVolumeP5', 'P5',
                       'BilledVolumeP6', 'P6',
                       'BilledVolumeP7', 'P7')

for (i in 1:length(initialDates$InitialDate)) {
  pred180[,((2*i)-1)] <- billedWaterData %>% filter(Date >= ymd(as.Date(initialDates$InitialDate[i])))  %>% filter(Date <= ymd(as.Date(initialDates$InitialDate[i])+predictPeriod-1)) %>% select(BilledVolume)
  tempForecastSet <- dailyForecastYVW(billedWaterData, initialPredictDate = initialDates$InitialDate[i], predictPeriod = predictPeriod, trainPeriod = 180)
  pred180[,(2*i)] <- tail(tempForecastSet$Predictions, 7)
}


pred270 <- data.frame(matrix(ncol=14,nrow=7))
colnames(pred270) <- c('BilledVolumeP1', 'P1',
                        'BilledVolumeP2', 'P2',
                        'BilledVolumeP3', 'P3',
                        'BilledVolumeP1', 'P4',
                        'BilledVolumeP5', 'P5',
                        'BilledVolumeP6', 'P6',
                        'BilledVolumeP7', 'P7')

for (i in 1:length(initialDates$InitialDate)) {
  pred270[,((2*i)-1)] <- billedWaterData %>% filter(Date >= ymd(as.Date(initialDates$InitialDate[i])))  %>% filter(Date <= ymd(as.Date(initialDates$InitialDate[i])+predictPeriod-1)) %>% select(BilledVolume)
  tempForecastSet <- dailyForecastYVW(billedWaterData, initialPredictDate = initialDates$InitialDate[i], predictPeriod = predictPeriod, trainPeriod = 270)
  pred270[,(2*i)] <- tail(tempForecastSet$Predictions, 7)
}


pred360 <- data.frame(matrix(ncol=14,nrow=7))
colnames(pred360) <- c('BilledVolumeP1', 'P1',
                       'BilledVolumeP2', 'P2',
                       'BilledVolumeP3', 'P3',
                       'BilledVolumeP1', 'P4',
                       'BilledVolumeP5', 'P5',
                       'BilledVolumeP6', 'P6',
                       'BilledVolumeP7', 'P7')

for (i in 1:length(initialDates$InitialDate)) {
  pred360[,((2*i)-1)] <- billedWaterData %>% filter(Date >= ymd(as.Date(initialDates$InitialDate[i])))  %>% filter(Date <= ymd(as.Date(initialDates$InitialDate[i])+predictPeriod-1)) %>% select(BilledVolume)
  tempForecastSet <- dailyForecastYVW(billedWaterData, initialPredictDate = initialDates$InitialDate[i], predictPeriod = predictPeriod, trainPeriod = 360)
  pred360[,(2*i)] <- tail(tempForecastSet$Predictions, 7)
}


pred540 <- data.frame(matrix(ncol=14,nrow=7))
colnames(pred540) <- c('BilledVolumeP1', 'P1',
                       'BilledVolumeP2', 'P2',
                       'BilledVolumeP3', 'P3',
                       'BilledVolumeP1', 'P4',
                       'BilledVolumeP5', 'P5',
                       'BilledVolumeP6', 'P6',
                       'BilledVolumeP7', 'P7')

for (i in 3:length(initialDates$InitialDate)) {
  pred540[,((2*i)-1)] <- billedWaterData %>% filter(Date >= ymd(as.Date(initialDates$InitialDate[i])))  %>% filter(Date <= ymd(as.Date(initialDates$InitialDate[i])+predictPeriod-1)) %>% select(BilledVolume)
  tempForecastSet <- dailyForecastYVW(billedWaterData, initialPredictDate = initialDates$InitialDate[i], predictPeriod = predictPeriod, trainPeriod = 540)
  pred540[,(2*i)] <- tail(tempForecastSet$Predictions, 7)
}


pred720 <- data.frame(matrix(ncol=14,nrow=7))
colnames(pred720) <- c('BilledVolumeP1', 'P1',
                       'BilledVolumeP2', 'P2',
                       'BilledVolumeP3', 'P3',
                       'BilledVolumeP1', 'P4',
                       'BilledVolumeP5', 'P5',
                       'BilledVolumeP6', 'P6',
                       'BilledVolumeP7', 'P7')

for (i in 3:length(initialDates$InitialDate)) {
  pred720[,((2*i)-1)] <- billedWaterData %>% filter(Date >= ymd(as.Date(initialDates$InitialDate[i])))  %>% filter(Date <= ymd(as.Date(initialDates$InitialDate[i])+predictPeriod-1)) %>% select(BilledVolume)
  tempForecastSet <- dailyForecastYVW(billedWaterData, initialPredictDate = initialDates$InitialDate[i], predictPeriod = predictPeriod, trainPeriod = 720)
  pred720[,(2*i)] <- tail(tempForecastSet$Predictions, 7)
}



predictions <- list(pred30, pred60, pred90, pred120, pred180, pred270, pred360, pred540, pred720)

mapePerformance <- data.frame(matrix(ncol=7,nrow=9))
colnames(mapePerformance) <- c('Period1', 'Period2', 'Period3', 'Period4','Period5', 'Period6', 'Period7')
rownames(mapePerformance) <- c('pred30', 'pred60', 'pred90', 'pred120', 'pred180', 'pred270', 'pred360', 'pred540', 'pred720')

for (i in 1:9) {
  for (j in 1:7) {
    mapePerformance[i,j] <- MAPE(predictions[[i]][[(2*j)]], predictions[[i]][[((2*j)-1)]])
  }
}

mapeMeans <- data.frame(matrix(ncol=1, nrow=9))
colnames(mapeMeans) <- c('MapeMean')
rownames(mapeMeans) <- c('pred30', 'pred60', 'pred90', 'pred120', 'pred180', 'pred270', 'pred360', 'pred540', 'pred720')


mapeMeans$MapeMean <- rowMeans(mapePerformance, na.rm=TRUE)
mapePerformance$MapeMean <- rowMeans(mapePerformance, na.rm=TRUE)

png('dailyPerformanceMAPE_YVW.png', height = 50*nrow(mapePerformance), width=200*ncol(mapePerformance))
grid.table(mapePerformance)
dev.off()

RMSEPerformance <- data.frame(matrix(ncol=7,nrow=9))
colnames(RMSEPerformance) <- c('Period1', 'Period2', 'Period3', 'Period4','Period5', 'Period6', 'Period7')
rownames(RMSEPerformance) <- c('pred30', 'pred60', 'pred90', 'pred120', 'pred180', 'pred270', 'pred360', 'pred540', 'pred720')

for (i in 1:9) {
  for (j in 1:7) {
    RMSEPerformance[i,j] <- RMSE(predictions[[i]][[(2*j)]], predictions[[i]][[((2*j)-1)]])
  }
}

RMSEMeans <- data.frame(matrix(ncol=1, nrow=9))
colnames(RMSEMeans) <- c('RMSEMean')
rownames(RMSEMeans) <- c('pred30', 'pred60', 'pred90', 'pred120', 'pred180', 'pred270', 'pred360', 'pred540', 'pred720')

RMSEMeans$RMSEMean <- rowMeans(RMSEPerformance, na.rm=TRUE)
RMSEPerformance$RMSEMean <- rowMeans(RMSEPerformance, na.rm=TRUE)

png('dailyPerformanceRMSE_YVW.png', height = 50*nrow(RMSEPerformance), width=200*ncol(RMSEPerformance))
grid.table(RMSEPerformance)
dev.off()