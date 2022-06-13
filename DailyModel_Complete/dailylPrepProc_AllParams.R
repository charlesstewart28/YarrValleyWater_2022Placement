# Daily Manual Pre Proc Script
# YVW Placement RMIT University
# Authors: Charles Stewart - s3628786
#          Julian De Angelis - s362s3814844
# Latest Update: 16/05/22

# Required Libraries

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(lubridate)
library(readr)
library(VIM)
library(matrixStats)

# Reading in of data files required
GlobalDemandFile <- 'YVW_BulkDaily_201501_202203.xlsx'

# Reading in bulk demand file
bulkDemand <- read_excel(GlobalDemandFile, 1, skip = 2, col_types = c('date', 'numeric', 'numeric', 'numeric'))
bulkDemand <- bulkDemand[-c(2,3)]
colnames(bulkDemand) <- c("Date", "BilledVolume")
bulkDemand$Date <- as.Date(gsub(' UTC', '', bulkDemand$Date))

# Blank df for all weather data with required range
days <-seq(as.Date(bulkDemand$Date[1]), as.Date(bulkDemand$Date[length(bulkDemand$Date)]), by="days")
weatherdf <- data.frame(matrix(ncol=1, nrow = length(days)))
colnames(weatherdf) <- c('Date')
weatherdf <- weatherdf %>%
  mutate(Date = days)
weatherdf <- subset(weatherdf, Date != '2021-10-19'&Date != '2021-12-03'&Date != '2021-12-04')

# Names of all required weather files from each station
tempRainNames <- list.files(path='StationRainfall_YVW/', pattern='.csv', full.names=TRUE)
tempTempNames <- list.files(path='StationTemp_YVW/', pattern='.csv', full.names=TRUE)

# Loop that reads in each file and merges df with weather df

for (i in 1:length(tempRainNames)) {
  rainTemp <- read_csv(tempRainNames[i],
                     skip = 1,
                     col_names = c('Code', 'Station', 'Year', 'Month', 'Day', 'Rainfall','M1','M2'),
                     col_types = c('c','i','i','i','i','i','i','c'))
  tempStation <- as.integer(rainTemp[1,2])
  rainTemp$Date <- paste(rainTemp$Year, rainTemp$Month, rainTemp$Day, sep="-") %>% ymd() %>% as.Date()
  rainTemp <- rainTemp[rainTemp$Date > '2014-12-31' & rainTemp$Date < '2022-03-29' & rainTemp$Date != '2021-10-19' &  rainTemp$Date != '2021-12-03' & rainTemp$Date != '2021-12-04',]
  rainTemp <- rainTemp[c('Rainfall', 'Date')]
  colnames(rainTemp) <- c(paste(tempStation, ' Rainfall'), 'Date')
  
  
  tempTemp <- read_csv(tempTempNames[i],
                       skip = 1,
                       col_names = c('Code', 'Station', 'Year', 'Month', 'Day', 'MaxTemp','M1','M2'),
                       col_types = c('c','i','i','i','i','i','i','c'))
  tempTemp$Date <- paste(tempTemp$Year, tempTemp$Month, tempTemp$Day, sep="-") %>% ymd() %>% as.Date()
  tempTemp <- tempTemp[tempTemp$Date > '2014-12-31' & tempTemp$Date < '2022-03-29' & tempTemp$Date != '2021-10-19' &  tempTemp$Date != '2021-12-03' & tempTemp$Date != '2021-12-04',]
  tempTemp <- tempTemp[c('MaxTemp', 'Date')]
  colnames(tempTemp) <- c(paste(tempStation, ' MaxTemp'), 'Date')
  
  weatherdf <- merge(weatherdf, rainTemp, by = 'Date') %>%
    merge(tempTemp, by='Date')
}

modelData <- bulkDemand
# Five bulk supply zones weather weighted averaged,
#   Viewbank                    		86068		25%
#   Scoresby Research Institute 		86104		25%
#   Melbourne Airport           		86282		25%
#   Coldstream                  		86383		10%
#   Wallan (Kilmore Gap)       		  88162		15%


# Station weighting
stationWeight <- c(0.25,0.25,00.25,0.10,0.15)


# Create all rainfall columns

modelData$Rainfall <- rowWeightedMeans(as.matrix(weatherdf[,c('86068  Rainfall', '86104  Rainfall', '86282  Rainfall', '86383  Rainfall', '88162  Rainfall')]),w=stationWeight)
modelData$RainfallYesterday <- lag(modelData$Rainfall)
modelData$RainfallTwoDaysAgo <- lag(modelData$RainfallYesterday)
modelData$RainfallThreeDaysAgo <- lag(modelData$RainfallTwoDaysAgo)
modelData$RainfallTomorrow <- lead(modelData$Rainfall)

# Create all temperature columns

modelData$MaxTemp <- rowWeightedMeans(as.matrix(weatherdf[,c('86068  MaxTemp', '86104  MaxTemp', '86282  MaxTemp', '86383  MaxTemp', '88162  MaxTemp')]),w=stationWeight)
modelData$MaxTempYesterday <- lag(modelData$MaxTemp)
modelData$MaxTempTwoDaysAgo <- lag(modelData$MaxTempYesterday)
modelData$MaxTempThreeDaysAgo <- lag(modelData$MaxTempTwoDaysAgo)
modelData$MaxTempTomorrow <- lead(modelData$MaxTemp)

# Include months for seasonality and flag weekends
modelData  <-  modelData %>% 
  mutate(month = as.factor(month(Date)), weekend = as.logical(wday(Date) == 1 | wday(Date) == 7)) 

# Impute missing values
modelData <- kNN(modelData, variable = c("MaxTemp", 
                                           "Rainfall", 
                                           "MaxTempYesterday", 
                                           "RainfallYesterday",
                                           "MaxTempTwoDaysAgo",
                                           "RainfallTwoDaysAgo",
                                           "MaxTempThreeDaysAgo",
                                           "RainfallThreeDaysAgo",
                                           "MaxTempTomorrow",
                                           "RainfallTomorrow"))

# Dump model1Data df into csv
write_csv(modelData, paste('dailyModelData', '.csv', sep = ''))
