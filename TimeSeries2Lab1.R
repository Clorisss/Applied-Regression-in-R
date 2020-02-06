
library(tidyverse)
library(readxl)
library(corrgram)
library(tseries)
library(forecast)

setwd("~/UWTacoma/TBANLT540/TimeSeriesRegression")
RainHoustonAll <- read_excel("~/UWTacoma/TBANLT540/TimeSeriesRegression/RainHoustonLong.xlsx")

dim(RainHoustonAll)    #returns the dimensions of an object
str(RainHoustonAll)    #returns the structure of an object
sum(is.na(RainHoustonAll)) #returns how many observations have "na"
RainHoustonAll[is.na(RainHoustonAll)] <- '0' #replaces "na" with 0. This is a choice, statistically, but you can't run the regression without it
sum(is.na(RainHoustonAll))
View(RainHoustonAll)

RainHoustonAll[is.na(RainHoustonAll)] <- 0
sum(is.na(RainHoustonAll))

#creating a time series dataset for decomposition#

View(RainHoustonAll)

#Create date variables, collapse to monthly averages, compare plots
RainHoustonAll$DATE<-as.POSIXct(RainHoustonAll$DATE, format="%Y-%m-%d")
RainHoustonAll$PRCP<-as.numeric(RainHoustonAll$PRCP)

MonthlyRain<-aggregate(list(rain = RainHoustonAll$PRCP), 
          list(month = cut(RainHoustonAll$DATE, "month")), 
          mean)
MonthlyRain

MonthlyRain2<-ts(MonthlyRain$rain, frequency = 12, start = c(1948,1))
MonthlyRain2

Rain<-ts(RainHoustonAll$PRCP, frequency = 365, start = c(1948,1))

#create a plot of the time series#
plot.ts(Rain)
plot.ts(MonthlyRain2)

#identify the trend/season/random components
RainParts<-decompose(Rain)
RainMonthParts<-decompose(MonthlyRain2)
plot(RainParts)
plot(RainMonthParts)


#Modeling using exponential smoothing - Full data

RainModel1<-HoltWinters(Rain)
RainModel1
RainModel1$SSE
plot(RainModel1, col=3, col.predicted=2)
residualsHolt1<-residuals(RainModel1)
plot(residualsHolt1)
acf(residualsHolt1)
pacf(residualsHolt1)

#Modeling using exponential smoothing - Monthly data

RainModel2<-HoltWinters(MonthlyRain2)
RainModel2
RainModel2$SSE
plot(RainModel2, col=3, col.predicted=2)
residualsHolt2<-residuals(RainModel2)
plot(residualsHolt2)
acf(residualsHolt2)
pacf(residualsHolt2)

#Forecasting using exponential smooting - Full Data
RainForecast<-forecast(Rain, h=400)
plot(RainForecast)

#Forecasting using exponential smooting - Monthly Data

RainForecast2<-forecast(MonthlyRain2, h=13)
plot(RainForecast2)

#modeling using an auto.arima model - Full Data 
#plot the acf and pacf
par(mfrow=c(1,2))
acf(Rain)
pacf(Rain)

RainArima<-auto.arima(Rain)
RainArima
acf(ts(RainArima$residuals), main='ACF Residual - Full')
pacf(ts(RainArima$residuals), main='PACF Residual - Full')



#modeling using an auto.arima model - Monthly Data 
#plot the acf and pacf
acf(MonthlyRain2)
pacf(MonthlyRain2)

RainArima2<-auto.arima(MonthlyRain2)
RainArima2

acf(ts(RainArima2$residuals), main='ACF Residual - Monthly')
pacf(ts(RainArima2$residuals), main='PACF Residual- Monthly')

prediction=predict(RainArima,n.ahead=10)
prediction

prediction=predict(RainArima2,n.ahead=10)
prediction
