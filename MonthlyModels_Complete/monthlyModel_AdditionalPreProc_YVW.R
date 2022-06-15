# Monthly Model Pre Processing Script - Addition of additional Services - Potable, fire etc
# YVW Placement RMIT University
# Authors: Charles Stewart - s3628786
#          Julian De Angelis - s362s3814844
# Latest Update: 13/06/22

# Required Libraries and Scripts


library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(lubridate)
library(readr)
library(VIM)
library(matrixStats)
library(data.table)
library(xts)
library(ichimoku)


# Import initial monthly mode data
modelData <- read.csv('monthlyData.csv')

# Import additional data xls - Data has been formated for easier importing
additionalServices <- read_excel("additionalServices.xlsx")

# Format additional date for merging
additionalServices$Month <- format(as.yearmon(as.character(additionalServices$Month),'%Y%m'),)
additionalServices$Month <- as.yearmon(additionalServices$Month)


modelData$Month <- as.yearmon(modelData$Month)

fullMonthlyData <- merge(modelData, additionalServices)
fullMonthlyData$NonRev <- NA

for (i in 1:length(fullMonthlyData$Month)) {
  fullMonthlyData$NonRev[i] <- fullMonthlyData$monthlyBulk[i] - sum(fullMonthlyData[i,c(3,4,8,9,10)])
}

# Add 48 months training data for Bulk, Res and Non Res water
trainData <- read.csv('monthlyData.csv')
trainData$Month <- as.yearmon(trainData$Month)

trainData <- trainData %>% filter(Month < as.yearmon('May 2021')) %>% filter(Month >= as.yearmon('April 2017'))
trainData[,c(8,9,10,11)] <- NA
colnames(trainData)[c(8,9,10,11)] <- c('Potable TopUp', 'Filling Stations', 'Unmeter Fire', 'NonRev')

fullMonthlyData <- rbind(trainData, fullMonthlyData)

write_csv(fullMonthlyData, paste('monthlyFullData_May2021Jan2022_YVW', '.csv', sep = ''))

