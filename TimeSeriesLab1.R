install.packages("tidyverse")
install.packages("corrgram")
install.packages("tseries")
install.packages("readxl")
install.packages("urca")
install.packages("forecast")
install.packages("trend")
install.packages("zoo")
install.packages("reshape")

library(tidyverse)
library(readxl)
library(corrgram)
library(tseries)
library(urca)
library(forecast)
library(trend)
library(zoo)
library(reshape)


setwd("C:/Users/mazhi/Documents/R")
Unemployment<-read_excel("C:/Users/mazhi/Documents/R/MacroData.xlsx",sheet="Sheet2")
Inflation<-read_excel("C:/Users/mazhi/Documents/R/MacroData.xlsx",sheet="Sheet3")
GDP<-read_excel("C:/Users/mazhi/Documents/R/MacroData.xlsx",sheet="Sheet7")
View(GDP)


# select variables from the larger dataset
Year<-Unemployment[1,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

# First review and check the number of country row and select the row we want
View(Unemployment)
LuxUn<-Unemployment[473,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]
View(LuxUn)

View(Inflation)
LuxInf<-Inflation[1089,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]
View(LuxInf)

View(GDP)
LuxGDP<-GDP[549,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47', 'Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]
View(LuxGDP)

View(Year)

# transpose the columns to observations and set them as numeric
t_LuxGDP<-t(LuxGDP)
t_LuxGDP<-as.numeric(t_LuxGDP)
View(t_LuxGDP)

t_LuxInf<-t(LuxInf)
t_LuxInf<-as.numeric(t_LuxInf)
View(t_LuxInf)

t_LuxUnem<-t(LuxUn)
t_LuxUnem<-as.numeric(t_LuxUnem)
View(t_LuxUnem)

t_Year<-t(Year)
t_Year<-as.numeric(t_Year)
View(t_Year)

# combine the observations together
TimeSeriesLux<-cbind(t_Year, t_LuxGDP, t_LuxInf, t_LuxUnem)
View(TimeSeriesLux)

# convert vector to data frame
TimeSeriesLux<-as.data.frame(TimeSeriesLux)

# rename the columns
TimeSeriesLux <- rename(TimeSeriesLux, c(t_Year="Years"))
TimeSeriesLux <- rename(TimeSeriesLux, c(t_LuxGDP="GrossDP"))
TimeSeriesLux <- rename(TimeSeriesLux, c(t_LuxInf="Inflation"))
TimeSeriesLux <- rename(TimeSeriesLux, c(t_LuxUnem="Unemploy"))
View(TimeSeriesLux)

# tell R this is time series data instead of numbers
tsUR<-zoo(TimeSeriesLux$Unemploy, order.by = TimeSeriesLux$Years)
tsIF<-zoo(TimeSeriesLux$Inflation, order.by = TimeSeriesLux$Years)
tsGDP<-zoo(TimeSeriesLux$GrossDP, order.by = TimeSeriesLux$Years)

#create plots of the timeseries#
ggplot(data = TimeSeriesLux, aes(x = TimeSeriesLux$Years, y = TimeSeriesLux$GrossDP)) + geom_line()
ggplot(data = TimeSeriesLux, aes(x = TimeSeriesLux$Years, y = TimeSeriesLux$Inflation))+ geom_line()
ggplot(data = TimeSeriesLux, aes(x = TimeSeriesLux$Years, y = TimeSeriesLux$Unemploy))+ geom_line()


#test for stationarity because the model only works when data is stationary#
#!!! unit root test, H0:  =0 (non-stationary), H1: <0 (stationary series) !!!
adf.test(tsGDP) # p-value = 0.8189, GDP is not stationary, go bakc and fit it.
adf.test(tsIF) # p-value = 0.3471, Inflation is not stationary, go bakc and fit it.
adf.test(tsUR) # p-value = 0.3642, Unemployment is not stationary, go bakc and fit it.

# if adf.test failed, check trend stationarity
# !!! H0: is trend stationarity, H1: non stationarity !!!
kpss.test(TimeSeriesLux$GrossDP, null = "Trend") # p-value < 0.01, GDP is non stationary, action needed (differencing).
kpss.test(TimeSeriesLux$Inflation, null = "Trend") # p-value > 0.1, Inflation is trend stationary.
kpss.test(TimeSeriesLux$Unemploy, null = "Trend") # p-value > 0.1, Unemploy is trend stationary.

#check the correlograms, then we know how many times to differencing our data
#how many time period is correlated, count the ones pass the blue line.
acf(tsUR) #The last 12 years matter, best practice would be doing 12 lags.
acf(tsIF) 
acf(tsGDP)
# partial autocorrelation
pacf(tsUR)
pacf(tsIF)
pacf(tsGDP)

Box.test(tsUR)
Box.test(tsIF)
Box.test(tsGDP)

#transform the data#

#differencing (fit non stationarity when kpss.test p-value < 0.05, but will lose observation)
# GDPlDiff1=diff(tsGDP)
GDPdiff=diff(tsGDP, differences=15)

YearDiff=diff(TimeSeriesLux$Years)
kpss.test(GDPdiff, null = "Trend")

acf(GDPdiff)
pacf(GDPdiff)

URdiff=diff(tsUR)
GDPdiff=diff(tsGDP)
kpss.test(InflDiff2, null = "Trend")

acf(URdiff)
acf(InflDiff2)

pacf(URdiff)
pacf(InflDiff2)


#detrending
m<-lm(coredata(GDPdiff)~index(GDPdiff))
GDPdetrend<-zoo(resid(m),index(GDPdiff))

plot(GDPdetrend)


#model data 
Arima(tsUR, order = c(1, 0, 0),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")


Arima(tsUR, order = c(1, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(tsGDP, order = c(2, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

#Unemployment Rate
Arima(tsUR, order = c(1, 0, 2),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(tsUR, order = c(1, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

#InflationRate
Arima(InflDiff2, order = c(0, 0, 9),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 10),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 8),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 7),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 2),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 2),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

# order(AR, leave it 0 if differenced, pacf)
Arima(GDPdiff, order = c(1, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(GDPdiff, order = c(3, 0, 4),
                 include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
                 method = "ML")

GDPData <- Arima(GDPdiff, order = c(3, 0, 4),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")


UmData<-Arima(tsUR, order = c(1, 0, 1),
              include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
              method = "ML")

InfData<-Arima(InflDiff2, order = c(0, 0, 2),
               include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
               method = "ML")

GDPData<-Arima(GDPdiff, order = c(2, 0, 1),
              include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
              method = "ML")

GDPData2<-Arima(tsGDP, order = c(2, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

# forecast based on model
plot(forecast(UmData,h=10))
plot(forecast(InfData,h=10))
plot(forecast(GDPData,h=10))
plot(forecast(GDPData2,h=100))

