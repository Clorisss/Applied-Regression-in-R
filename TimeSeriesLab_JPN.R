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

# select variables from the larger dataset
Year<-Unemployment[1,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]
View(Year)

# First review and check the number of country row and select the row we want
View(Unemployment)
JPNUn<-Unemployment[494,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]
View(JPNUn)

View(Inflation)
JPNInf<-Inflation[1111,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]
View(JPNInf)

View(GDP)
JPNGDP<-GDP[571,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47', 'Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]
View(JPNGDP)

# transpose the columns to observations and set them as numeric
t_JPNGDP<-t(JPNGDP)
t_JPNGDP<-as.numeric(t_JPNGDP)
View(t_JPNGDP)

t_JPNInf<-t(JPNInf)
t_JPNInf<-as.numeric(t_JPNInf)
View(t_JPNInf)

t_JPNUnem<-t(JPNUn)
t_JPNUnem<-as.numeric(t_JPNUnem)
View(t_JPNUnem)

t_Year<-t(Year)
t_Year<-as.numeric(t_Year)
View(t_Year)

# combine the observations together
TimeSeriesJPN<-cbind(t_Year, t_JPNGDP, t_JPNInf, t_JPNUnem)
View(TimeSeriesJPN)

# convert vector to data frame
TimeSeriesJPN<-as.data.frame(TimeSeriesJPN)

# rename the columns
TimeSeriesJPN <- rename(TimeSeriesJPN, c(t_Year="Years"))
TimeSeriesJPN <- rename(TimeSeriesJPN, c(t_JPNGDP="GrossDP"))
TimeSeriesJPN <- rename(TimeSeriesJPN, c(t_JPNInf="Inflation"))
TimeSeriesJPN <- rename(TimeSeriesJPN, c(t_JPNUnem="Unemploy"))
View(TimeSeriesJPN)

#scatterplot for unemploy, inf, and GDP
ggplot(data=TimeSeriesJPN)+geom_point(mapping = aes(x=Years, y=Unemploy), size=1)
ggplot(data=TimeSeriesJPN)+geom_point(mapping = aes(x=Years, y=Inflation), size=1)
ggplot(data=TimeSeriesJPN)+geom_point(mapping = aes(x=Years, y=GrossDP), size=1)

# tell R this is time series data instead of numbers
tsJPNUR<-zoo(TimeSeriesJPN$Unemploy, order.by = TimeSeriesJPN$Years)
tsJPNIF<-zoo(TimeSeriesJPN$Inflation, order.by = TimeSeriesJPN$Years)
tsJPNGDP<-zoo(TimeSeriesJPN$GrossDP, order.by = TimeSeriesJPN$Years)

#create plots of the timeseries#
ggplot(data = TimeSeriesJPN, aes(x = TimeSeriesJPN$Years, y = TimeSeriesJPN$GrossDP)) + geom_line()
ggplot(data = TimeSeriesJPN, aes(x = TimeSeriesJPN$Years, y = TimeSeriesJPN$Inflation))+ geom_line()
ggplot(data = TimeSeriesJPN, aes(x = TimeSeriesJPN$Years, y = TimeSeriesJPN$Unemploy))+ geom_line()


#test for stationarity because the model only works when data is stationary#
#!!! unit root test, H0: =0 (non-stationary), H1: <0 (stationary series) !!!
adf.test(tsJPNGDP)
 # p-value = 0.952, GDP is not stationary, go bakc and fit it.
adf.test(tsJPNIF) 
# p-value = 0.8184, Inflation is not stationary, go bakc and fit it.
adf.test(tsJPNUR) 
# p-value = 0.5745, Unemployment is not stationary, go bakc and fit it.


# If adf test failed, check if have trend stationarity.
# If it is trend stationary, we can do detrend.
# If it is not trend, it is just non stationary, then do differencing.

# !!!KPSS H0: trend stationarity, H1: non stationarity !!!
kpss.test(TimeSeriesJPN$GrossDP, null = "Trend")
# p-value < 0.01, GDP is non stationary, action needed (differencing).
kpss.test(TimeSeriesJPN$Inflation, null = "Trend")
# p-value < 0.01, Inflation is non stationary, action needed (differencing).
kpss.test(TimeSeriesJPN$Unemploy, null = "Trend")
# p-value = 0.08277, Unemploy is trend stationary, action needed (detrend).

# check the correlograms, then we know how many times to differencing our data
# how many time period is correlated, count the ones pass the blue line.
# The number means: 8 - The last 8 years matter, best practice would be doing 8 lags.
acf(tsJPNIF) 
acf(tsJPNGDP)
acf(tsJPNUR)
# partial autocorrelation
pacf(tsJPNIF)
pacf(tsJPNGDP)
pacf(tsJPNUR)

# differencing (fit non stationarity when kpss.test p-value < 0.05, but will lose observation)
# if doing models with more than one variables, like GDP AND unemploy, if gdp is differenced 15 time, so does unemploy.
INdiff2=diff(tsJPNIF,differences = 2)
GDPdiff1=diff(tsJPNGDP,differences = 1)
URdiff1=diff(tsJPNUR, differences = 1)
# need to diff year too, get the same length
YearDiff=diff(TimeSeriesJPN$Years)
# After differencing test the stationary again
adf.test(INdiff2) # stationary
adf.test(GDPdiff1) # stationary
adf.test(URdiff1) # stationary
kpss.test(URdiff1, null = "Trend") # has trend

# detrending
# Step 1: Create a linear regression to get your mean
m<-lm(coredata(tsJPNUR)~index(tsJPNUR))
URdetrend<-zoo(resid(m),index(tsJPNUR))
plot(URdetrend)
adf.test(URdetrend)
kpss.test(URdetrend, null = "Trend")

# way 2: differencing unemploy data first and then detrend the differenced data
m<-lm(coredata(URdiff1)~index(URdiff1))
URdetrend2<-zoo(resid(m),index(URdiff1))
plot(URdetrend2)
adf.test(URdetrend2)
kpss.test(URdetrend2, null = "Trend")
# detrend does not work, unemploy still has trend for either way.

# After differencing and detrend, check acf and pacf again to determinde orders of p,q in Arima.
acf(INdiff2) # one or more spikes and rest essentially zero, it's MA, lag = 1
pacf(INdiff2)
acf(GDPdiff1) # one or more spikes and rest essentially zero, it's MA, lag = 1
pacf(GDPdiff1) 
acf(URdiff1) # one or more spikes and rest essentially zero, it's MA, lag = 2
pacf(URdiff1)
acf(URdetrend2) # one or more spikes and rest essentially zero, it's MA, lag = 1
pacf(URdetrend2)

# model data, in order there're 3 numbers: AR, Differencing(if already differenced, keep 0), moving average
# Inflation Rate
Arima(INdiff2, order = c(0, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")
Arima(tsJPNIF, order = c(0, 2, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")
# Both AIC=199.41

Arima(INdiff2, order = c(1, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")
# AIC=197.61

Arima(INdiff2, order = c(0, 0, 2),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")
Arima(tsJPNIF, order = c(0, 2, 2),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")
# Both AIC=196.85

# GDP Rate
Arima(GDPdiff1, order = c(0, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")
Arima(tsJPNGDP, order = c(0, 1, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")
# Both AIC=1233.63

Arima(GDPdiff1, order = c(1, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")
Arima(tsJPNGDP, order = c(1, 1, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")
# Both AIC=1210.68

# Unemployment Rate
Arima(URdetrend2, order = c(1, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")
# AIC=16.82

Arima(URdetrend2, order = c(0, 0, 2),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")
# AIC=16.82

Arima(URdetrend2, order = c(1, 0, 2),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")
# AIC=18.81

Arima(URdetrend2, order = c(0, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")
# AIC=14.82

Arima(tsJPNUR, order = c(0, 1, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")
# AIC=13.28

Arima(URdiff1, order = c(0, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")
# AIC=14.82

# choose the lowest AIC as the best model
GDPData <- Arima(GDPdiff1, order = c(1, 0, 1),
                 include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
                 method = "ML")
InfData <- Arima(INdiff2, order = c(0, 0, 2),
               include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
               method = "ML")
InfData1 <- Arima(tsJPNIF, order = c(0, 2, 2),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")
UnemData<-Arima(tsJPNUR, order = c(0, 1, 1),
                include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
                method = "ML")

# forecast based on model
plot(forecast(UnemData,h=10))
plot(forecast(InfData,h=10))
plot(forecast(InfData1,h=10))
plot(forecast(GDPData,h=10))
# h means number of years in the furture
plot(forecast(GDPData,h=100))

# test white noise model
Box.test(tsUR)
Box.test(tsIF)
Box.test(tsGDP)


