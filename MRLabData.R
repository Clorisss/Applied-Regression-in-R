library(readxl)
library(tidyverse)
library(mi)

setwd("C:/Users/mazhi/Documents/R")

MRLAB1 <- read_excel("C:/Users/mazhi/Documents/R/redfin.xlsx")
View(MRLAB1)

ggplot(data=MRLAB1, aes(x=MRLAB1$BEDS, y=MRLAB1$PRICE)) + geom_point(size=2)
ggplot(data=MRLAB1, aes(x=MRLAB1$BATHS, y=MRLAB1$PRICE)) + geom_point(size=2)
ggplot(data=MRLAB1, aes(x=MRLAB1$`SQUARE FEET`, y=MRLAB1$PRICE)) + geom_point(size=2)
ggplot(data=MRLAB1, aes(x=MRLAB1$`LOT SIZE`, y=MRLAB1$PRICE)) + geom_point(size=2)
ggplot(data=MRLAB1,aes(x=MRLAB1$`YEAR BUILT`, y=MRLAB1$PRICE)) + geom_point(size=2)

ggplot(data=MRLabData, aes(x=MRLabData$`YEAR BUILT`, y=MRLabData$PRICE)) + geom_point(size=2)+geom_smooth()

hist(MRLAB1$BEDS, xlab="BEDS")
hist(MRLAB1$BATHS, xlab="BATHS")
hist(MRLAB1$`SQUARE FEET`, xlab="Square Feet")
hist(MRLAB1$`YEAR BUILT`, xlab="Year Built")
hist(MRLAB1$`LOT SIZE`, xlab = "Lot Size")
hist(MRLAB1$PRICE, xlab = "PRICE")


#data reframing
MRLAB_REF <- MRLAB1 %>% select(PRICE,BEDS,BATHS,'SQUARE FEET','LOT SIZE','YEAR BUILT')
summary(MRLAB_REF)

#to delete missing data#
MRLAB4 <- MRLAB_REF[complete.cases(MRLAB_REF), ]
View(MRLAB4)

#multiple linear regression for before-log data (MRLAB4)
HouseFit2 <- lm(PRICE~BATHS+BEDS+`SQUARE FEET` + `LOT SIZE` + `YEAR BUILT`, data=MRLAB4)
summary(HouseFit2)

HouseFit1<-lm(PRICE~BATHS+BEDS+`SQUARE FEET`+`LOT SIZE`+ YearScale, data=MRLAB4)
HouseFit3<-lm(PRICE~`SQUARE FEET`, data=MRLAB4)
summary(HouseFit1)
summary(HouseFit3)


#logs#
MRLAB4$LOGPRICE <- log(MRLAB4$PRICE)
MRLAB4$LOGBed <- log(MRLAB4$BEDS)
MRLAB4$LOGBath <- log(MRLAB4$BATHS)
MRLAB4$LOGLotSize <- log(MRLAB4$`LOT SIZE`)
MRLAB4$LOGYearBuilt <- log(MRLAB4$`YEAR BUILT`)


summary(MRLAB4)
view(MRLAB4)

#multiple linear regression for after-log data (MRLAB4)
HouseFitlog<-lm(LOGPRICE~LOGBed+LOGBath+LOGSqFoot+LOGLotSize+LOGYearBuilt, data=MRLAB4)
summary(HouseFitlog)
HouseFitLog_NOSQFT<-lm(LOGPRICE~LOGBed+LOGBath+LOGLotSize+LOGYearBuilt+`SQUARE FEET`, data = MRLAB4)
summary(HouseFitLog_NOSQFT)


#residuals#
plot(HouseFit2)
Price.stdres <-residuals(HouseFit2)
ggplot(data=MRLAB4, aes(x=BEDS, y=Price.stdres))+geom_point()
ggplot(data=MRLAB4, aes(x=BATHS, y=Price.stdres))+geom_point()
ggplot(data=MRLAB4, aes(x=`SQUARE FEET`, y=Price.stdres))+geom_point()
ggplot(data=MRLAB4, aes(x=`LOT SIZE`, y=Price.stdres))+geom_point()
ggplot(data=MRLAB4, aes(x=`YEAR BUILT`, y=Price.stdres))+geom_point()

plot(HouseFitlog)
PriceLOG.stdres <-residuals(HouseFitlog)
ggplot(data=MRLAB4, aes(x=LOGBed, y=PriceLOG.stdres))+geom_point()
ggplot(data=MRLAB4, aes(x=LOGBath, y=PriceLOG.stdres))+geom_point()
ggplot(data=MRLAB4, aes(x=LOGSqFoot, y=PriceLOG.stdres))+geom_point()
ggplot(data=MRLAB4, aes(x=LOGLotSize, y=PriceLOG.stdres))+geom_point()
ggplot(data=MRLAB4, aes(x=LOGYearBuilt, y=PriceLOG.stdres))+geom_point()

plot(HouseFitLog_NOSQFT)
PriceLOG_NOSQFT_strdres <- residuals(HouseFitLog_NOSQFT)
ggplot(data=MRLAB4, aes(x=LOGBed, y=PriceLOG_NOSQFT_strdres))+geom_point()
ggplot(data=MRLAB4, aes(x=LOGBath, y=PriceLOG_NOSQFT_strdres))+geom_point()
ggplot(data=MRLAB4, aes(x=`SQUARE FEET`, y=PriceLOG_NOSQFT_strdres))+geom_point()
ggplot(data=MRLAB4, aes(x=LOGLotSize, y=PriceLOG_NOSQFT_strdres))+geom_point()
ggplot(data=MRLAB4, aes(x=LOGYearBuilt, y=PriceLOG_NOSQFT_strdres))+geom_point()


Price.predict=predict(lm(PRICE~BATHS+BEDS+`SQUARE FEET`+`LOT SIZE`+`YEAR BUILT`, data=MRLAB4))
Price_predict=predict(HouseFitLog_NOSQFT)

View(MRLAB4)
ggplot(MRLAB4, aes(x = LOGBed, y = PRICE)) +
  geom_point()+ # Points of actual values
  geom_point(aes(y = Price_predict), shape = 1)  # Points of predicted values

ggplot(MRLAB4, aes(x = LOGBath, y = PRICE)) +
  geom_point()+ # Points of actual values
  geom_point(aes(y = Price_predict), shape = 1) # Points of predicted values

ggplot(MRLAB4, aes(x = `SQUARE FEET`, y = PRICE)) +
  geom_point()+ # Points of actual values
  geom_point(aes(y = Price_predict), shape = 1) # Points of predicted values

ggplot(MRLAB4, aes(x = `LOT SIZE`, y = PRICE)) +
  geom_point()+ # Points of actual values
  geom_point(aes(y = Price_predict), shape = 1) # Points of predicted values

ggplot(MRLAB4, aes(x = `YEAR BUILT`, y = PRICE)) +
  geom_point()+ # Points of actual values
  geom_point(aes(y = Price_predict), shape = 1) # Points of predicted values

HouseFitSqFt<-lm(PRICE~`SQUARE FEET`, data=MRLAB4)
summary(HouseFitSqFt)

#to impute missing data#
mMRLab<-missing_data.frame(data.frame(MRLAB3))

show(mMRLab)

summary (mMRLab)

imputations <- mi(mMRLab, n.iter = 30, n.chains = 4, max.minutes = 20) 
show(imputations)
round(mipply(imputations, mean, to.matrix = TRUE), 3)
imputations <- mi(imputations, n.iter = 5)
plot(imputations)

Houseanalysis <- pool(PRICE ~ BEDS + BATHS + SQUARE.FEET + YEAR.BUILT + LOT.SIZE, data = imputations, m = 5)

display(Houseanalysis)


