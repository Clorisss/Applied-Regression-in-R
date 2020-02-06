library(readxl)
library(plyr)
library(dplyr)
library(tidyverse)
library(car)
library(zoo)

install.packages("zoo")

setwd("C:/Users/mazhi/Documents/R")
RainAtlanta2018 <- read_excel("C:/Users/mazhi/Documents/R/AtlantaRain.xlsx")


dim(RainAtlanta2018)    #returns the dimensions of an object
str(RainAtlanta2018)    #returns the structure of an object
sum(is.na(RainAtlanta2018)) #returns how many observations have "na"
RainAtlanta2018[is.na(RainAtlanta2018)] <- '0' #replaces "na" with 0. This is a choice, statistically, but you can't run the regression without it
sum(is.na(RainAtlanta2018))
View(RainAtlanta2018)

RainAtlanta2018 <- RainAtlanta2018[,-c(PGTM,SNOW,SNWD,WDF2)]
RainAtlanta2018 <- select (RainAtlanta2018,-c(PGTM,SNOW,SNWD,WDF2))
View(RainAtlanta2018)


#add a Season Variable#
yq <- as.yearqtr(as.yearmon(RainAtlanta2018$DATE, "%Y/%m/%d") + 1/12)
RainAtlanta2018$Season <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))
summary(RainAtlanta2018$Season)

#create a Wind Direction factor variable#
RainAtlanta2018$NewWindDir<-RainAtlanta2018$WDF5-23
RainAtlanta2018$WindCat<-cut(RainAtlanta2018$NewWindDir, c(-22,22,67,112,157,202,247,292,337))
RainAtlanta2018$NewWindCat<-revalue(RainAtlanta2018$WindCat, c("(-22,22]"="N","(22,67]"="NE","(67,112]"="E","(112,157]"="SE","(157,202]"="S","(202,247]"="SW","(247,292]"="W","(292,337]"="NW" ))
list(RainAtlanta2018$NewWindCat)
#create a Wind Direction factor variable#
RainAtlanta2018$NewWindDir<-RainAtlanta2018$WDF5-11
RainAtlanta2018$WindCat<-cut(RainAtlanta2018$NewWindDir, c(-11,11,34,56,79,101,124,146,169,191,214,236,259,281,304,326,349))
RainAtlanta2018$NewWindCat<-revalue(RainAtlanta2018$WindCat, c("(-11,11]"="N","(11,34]"="NNE","(34,56]"="NE","(56,79]"="ENE","(79,101]"="E","(101,124]"="ESE","(124,146]"="SE","(146,169]"="SSE","(169,191]"="S","(191,214]"="SSW","(214,236]"="SW","(236,259]"="WSW","(259,281]"="W","(281,304]"="WNW","(304,326]"="NW","(326,349]"="NNW" ))
list(RainAtlanta2018$NewWindCat)
summary(RainAtlanta2018$NewWindCat)

#Create Factor variable indicating presence of Rain
RainAtlanta2018$RainFac <- ifelse(RainAtlanta2018$PRCP > 0, 1, 0)
rain<-factor(RainAtlanta2018$RainFac)
summary(rain)

#create sequential logit models
rainpredict<-glm(rain~RainAtlanta2018$Season, data=RainAtlanta2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~RainAtlanta2018$Season + RainAtlanta2018$AWND, data=RainAtlanta2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~RainAtlanta2018$Season + RainAtlanta2018$AWND + RainAtlanta2018$TAVG, data=RainAtlanta2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~RainAtlanta2018$Season + RainAtlanta2018$AWND + RainAtlanta2018$TAVG + RainAtlanta2018$TMAX, data=RainAtlanta2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~RainAtlanta2018$Season + RainAtlanta2018$AWND + RainAtlanta2018$TAVG + RainAtlanta2018$TMAX + RainAtlanta2018$TMIN, data=RainAtlanta2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~RainAtlanta2018$Season + RainAtlanta2018$AWND + RainAtlanta2018$TAVG + RainAtlanta2018$TMAX + RainAtlanta2018$TMIN + RainAtlanta2018$WSF5, data=RainAtlanta2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rainpredict<-glm(rain~RainAtlanta2018$Season + RainAtlanta2018$AWND + RainAtlanta2018$TAVG + RainAtlanta2018$TMAX + RainAtlanta2018$TMIN + RainAtlanta2018$WSF5+ RainAtlanta2018$NewWindCat , data=RainAtlanta2018, family=binomial)
summary(rainpredict)
# use exponentials to get the log odds, if CI numbers are both > 1, then the odds are significant.
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))

rain.res<-residuals.glm(rainpredict)
ggplot(data=RainAtlanta2018, aes(x=RainAtlanta2018$Season, y=rain.res))+geom_point()
ggplot(data=RainAtlanta2018, aes(x=RainAtlanta2018$AWND, y=rain.res))+geom_point()
ggplot(data=RainAtlanta2018, aes(x=RainAtlanta2018$TAVG, y=rain.res))+geom_point()
ggplot(data=RainAtlanta2018, aes(x=RainAtlanta2018$TMAX, y=rain.res))+geom_point()
ggplot(data=RainAtlanta2018, aes(x=RainAtlanta2018$TMIN, y=rain.res))+geom_point()
ggplot(data=RainAtlanta2018, aes(x=RainAtlanta2018$WSF5, y=rain.res))+geom_point()
ggplot(data=RainAtlanta2018, aes(x=RainAtlanta2018$NewWindCat, y=rain.res))+geom_point()


rainpredict<-glm(rain~RainAtlanta2018$AWND + RainAtlanta2018$TAVG + RainAtlanta2018$TMAX + RainAtlanta2018$TMIN + RainAtlanta2018$NewWindCat + RainAtlanta2018$WSF5 + RainAtlanta2018$Season, data=RainAtlanta2018, family=binomial)
summary(rainpredict)
exp(cbind(Odds_Ratio_RainVNoRain=coef(rainpredict), confint(rainpredict)))
newdata = data.frame(AWND=6, TAVG=48.5, TMAX=66, TMIN=31, Season="fall")
predict(rainpredict, newdata, type="response")

