# Daily Manual AIC/BIC Testing Script
# YVW Placement RMIT University
# Authors: Charles Stewart - s3628786
#          Julian De Angelis - s362s3814844
# Latest Update: 16/05/22

# Required Libraries and Scripts

library(forecast)
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(AICcmodavg)
library(gridExtra)

# Read in dataset
modelData <- read.csv('dailyModelData.csv')

# Set up variable selection for each model
model0Vars <- c(3:14)
model1Vars <- c(3:5,7:14)
model2Vars <- c(3:4,7:14)
model3Vars <- c(3,7:14)
model4Vars <- c(3,8:14)
model5Vars <- c(3:10,12:14)
model6Vars <- c(3:9,12:14)
model7Vars <- c(3:8,12:14)
model8Vars <- c(3:8,13:14)
model9Vars <- c(3:5,7:10,12:14)
model10Vars <- c(3:4,7:9,12:14)
model11Vars <- c(3,7:8,12:14)
model12Vars <- c(3,8:10,12:14)
model13Vars <- c(3,8:9,12:14)
model14Vars <- c(3,8,12:14)
model15Vars <- c(3,8,13:14)

varList <- list('model0' = model0Vars,
                'model1' = model1Vars,
                'model2' = model2Vars,
                'model3' = model3Vars,
                'model4' = model4Vars,
                'model5' = model5Vars,
                'model6' = model6Vars,
                'model7' = model7Vars,
                'model8' = model8Vars,
                'model9' = model9Vars,
                'model10' = model10Vars,
                'model11' = model11Vars,
                'model12' = model12Vars,
                'model13' = model13Vars,
                'model14' = model14Vars,
                'model15' = model15Vars)


# Subset for given training period - 2020-2021
trainVars <- modelData[,c(1:14)]  %>% filter(Date >= ymd('20-01-01'))  %>% filter(Date <= ymd('21-12-31'))

# Fit and obtain AIC and BIC for each model
ABICtracker <- data.frame(matrix(ncol=2,nrow=16))
colnames(ABICtracker) <- c('AIC', 'BIC')
rownames(ABICtracker) <- c('model0','model1','model2','model3','model4','model5','model6','model7','model8','model9','model10','model11','model12','model13','model14','model15')



for (x in 1:length(varList)) {
  fitModel <- auto.arima(trainVars$BilledVolume, xreg=data.matrix(trainVars[,varList[[x]]]))
  ABICtracker[x,1] <- AIC(fitModel)
  ABICtracker[x,2] <- BIC(fitModel)
}


png('dailyAICBIC_YVW.png', height = 50*nrow(ABICtracker), width=200*ncol(ABICtracker))
grid.table(ABICtracker)
dev.off()
