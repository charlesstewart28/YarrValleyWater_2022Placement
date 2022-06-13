# Monthly Model Pre Processing Script
# YVW Placement RMIT University
# Authors: Charles Stewart - s3628786
#          Julian De Angelis - s362s3814844
# Latest Update: 12/05/22

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



# First convert daily datat required for the monthly models
# Rain data is totalled for the month
# Temperature data is average per day

# Read in dataset
modelData <- read.csv('dailyModelData.csv')

# Convert bulk demand water data to monthly

waterData <- modelData[,1:2]
waterData <- xts(waterData$BilledVolume, as.Date(waterData$Date, "%Y-%m-%d"))
waterData = apply.monthly(waterData, sum)

# Convert rain data to monthly

rainfallData <- modelData[,c(1,3)]
rainfallData <- xts(rainfallData$Rainfall, as.Date(rainfallData$Date, "%Y-%m-%d"))
rainfallData = apply.monthly(rainfallData, sum)

# Convert temperature data to monthly

tempData <- modelData[,c(1,8)]
tempData <- xts(tempData$MaxTemp, as.Date(tempData$Date, "%Y-%m-%d"))
tempData = apply.monthly(tempData, mean)

monthlyData <- merge(waterData, rainfallData, tempData)

# Save data to new dataframe

monthlyData <- xts_df(monthlyData)
colnames(monthlyData)[1] <- 'Date'
colnames(monthlyData)[2] <- 'monthlyBulk'

# Monthly preprocessing, sorting all data for monthly model

# Reading in of data files required
ResNonResConnectionsFiles <- 'yvw-res-nonres-connections-monthly-201607-202306.xlsx'

# Reading in bulk demand file
ResNonResConnections <- read_excel(ResNonResConnectionsFiles)
colnames(ResNonResConnections) <- c('Date', 'NonRes', 'Res', 'Connections')
ResNonResConnections <- ResNonResConnections[c(3:69),]
ResNonResConnections$Date <- format(as.yearmon(as.character(ResNonResConnections$Date),'%Y%m'),)
ResNonResConnections$Date <- as.yearmon(ResNonResConnections$Date)


monthlyData$Date <- as.yearmon(monthlyData$Date)
monthlyData <- monthlyData[c(19:85),]
monthlyData <- merge(monthlyData, ResNonResConnections)
monthlyData <- monthlyData[,c(1,2,5,6,3,4,7)]

monthlyData[,c(3,4,7)] <- sapply(monthlyData[,c(3,4,7)],as.factor)
monthlyData[,c(3,4,7)] <- sapply(monthlyData[,c(3,4,7)],as.double)

colnames(monthlyData)[1] <- 'Month'

write_csv(monthlyData, paste('monthlyData', '.csv', sep = ''))




