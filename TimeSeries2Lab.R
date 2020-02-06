
library(tidyverse)
library(readxl)
library(corrgram)
library(tseries)
library(forecast)

setwd("C:/Users/mazhi/Documents/R")
RainAtlantaAll <- read_excel("C:/Users/mazhi/Documents/R/AtlantaRain1930-2018.xlsx")

dim(RainAtlantaAll)    #returns the dimensions of an object
str(RainAtlantaAll)    #returns the structure of an object
sum(is.na(RainAtlantaAll)) #returns how many observations have "na"
RainAtlantaAll[is.na(RainAtlantaAll)] <- '0' #replaces "na" with 0. This is a choice, statistically, but you can't run the regression without it
# RainAtlantaAll[is.na(RainAtlantaAll)] <- 0
sum(is.na(RainAtlantaAll))
View(RainAtlantaAll)

#creating a time series dataset for decomposition#
#Create date variables, collapse to monthly averages (adjust for seasonaltiy), compare plots
RainAtlantaAll$DATE<-as.POSIXct(RainAtlantaAll$DATE, format="%Y-%m-%d")
View(RainAtlantaAll)
RainAtlantaAll$PRCP<-as.numeric(RainAtlantaAll$PRCP)
MonthlyRain<-aggregate(list(rain = RainAtlantaAll$PRCP), 
          list(month = cut(RainAtlantaAll$DATE, "month")), 
          mean)
View(MonthlyRain)

MonthlyRain2<-ts(MonthlyRain$rain, frequency = 12, start = c(1930,1))
view(MonthlyRain2)

DailyRain<-ts(RainAtlantaAll$PRCP, frequency = 365, start = c(1930,1))
View(DailyRain)

#create a plot of the time series#
plot.ts(DailyRain)
plot.ts(MonthlyRain2)

#identify the trend/season/random components
RainDayParts<-decompose(DailyRain)
RainMonthParts<-decompose(MonthlyRain2)
plot(RainDayParts)
plot(RainMonthParts)

# Modeling using exponential smoothing - Full data
#
RainModel1<-HoltWinters(DailyRain)
RainModel1
RainModel1$SSE
plot(RainModel1, col=3, col.predicted=2) #at the begining predict well
residualsHolt1<-residuals(RainModel1) # at begining more evenly spread out.
plot(residualsHolt1)
acf(residualsHolt1) # still autocorrelation, maybe not the best choice of this model
pacf(residualsHolt1)

#Modeling using exponential smoothing - Monthly data
RainModel2<-HoltWinters(MonthlyRain2)
RainModel2
RainModel2$SSE
plot(RainModel2, col=3, col.predicted=2) # better with monthly data,up and down,
residualsHolt2<-residuals(RainModel2) # better spread out around zero, but still under predicting
plot(residualsHolt2)
acf(residualsHolt2) # 1 spike above
pacf(residualsHolt2) # smooth daily data into monthly average, made a better model
# whatever the time series we have, take the larger number of it. if have hourly data, convert into daily
#Forecasting using exponential smooting - Full Data (400 days in the furture)
RainForecast1<-forecast(DailyRain, h=400)
plot(RainForecast1)

#Forecasting using exponential smoothing - Monthly Data (13 mons in the furture)
RainForecast2<-forecast(MonthlyRain2, h=13)
plot(RainForecast2)

#modeling using an auto.arima model - Full Data 
#plot the acf and pacf
par(mfrow=c(1,2)) # put acf and pacf together
acf(DailyRain)
pacf(DailyRain)

RainArima1<-auto.arima(DailyRain, seasonal = TRUE, trace = TRUE)
RainArima1
acf(ts(RainArima1$residuals), main='ACF Residual - Full')
pacf(ts(RainArima1$residuals), main='PACF Residual - Full')

#modeling using an auto.arima model - Monthly Data 
#plot the acf and pacf
acf(MonthlyRain2)
pacf(MonthlyRain2)

RainArima2<-auto.arima(MonthlyRain2,seasonal = TRUE, trace = TRUE)
RainArima2

acf(ts(RainArima2$residuals), main='ACF Residual - Monthly')
pacf(ts(RainArima2$residuals), main='PACF Residual- Monthly')

plot(forecast(RainArima1, h=400))
plot(forecast(RainArima2, h=13))
prediction1=predict(RainArima1,n.ahead=10)
prediction1
prediction2=predict(RainArima2,n.ahead=10)
prediction2
prediction3=predict(RainModel1,n.ahead=10)
prediction3
prediction4=predict(RainModel2,n.ahead=10)
prediction4

predict(RainArima1,n.ahead=329)
predict(RainModel1,n.ahead=329)
forecast(RainModel1, h=329)
forecast(RainArima1, h=329)
