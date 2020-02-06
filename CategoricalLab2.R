library(readxl)
library(plyr)
library(tidyverse)
library(car)

setwd("~/UWTacoma/TBANLT540/MultipleRegression")

CatLotData <- read_excel("~/UWTacoma/TBANLT540/MultipleRegression/MRLabData3.xlsx")
View(CatLotData)

CatLotData2 <- CatLotData %>% select(PRICE,BEDS, BATHS,`SQUARE FEET`,`LOT SIZE`,`YEAR BUILT`,`LOCATION`)
View(CatLotData2)
show(CatLotData2)

CatLotData2 <-  CatLotData2[complete.cases(CatLotData2), ]

show(CatLotData2)


#seperate Lot Size into categories#
hist(CatLotData2$`LOT SIZE`)

CatLotData2$LotCat<-cut(CatLotData2$`LOT SIZE`, c(0,2500,5000,7500,10000,12500,15000,Inf))


View(CatLotData2)



#create factor variable for Neighborhoods#
CatLotData2$NeighCat<-factor(CatLotData2$LOCATION)
LocCount <- as.data.frame(table(CatLotData2$LOCATION))

summary(CatLotData2$NeighCat)
summary(CatLotData2$PRICE)

aggregate(PRICE ~ NeighCat, CatLotData2, mean)

CatLotData2$NeighCat<-factor(CatLotData2$LOCATION,levels=c('Alaska Junction','North Admiral','Belvidere','Admiral','Gatewood','Fairmount','Delridge'
  ,'West Seattle','Alki','Genesee','Fauntleroy','Westwood Village','High Point','Westwood','Arbor Heights'))     # this is to enforce a order to the reference for the factor variable

#count the values in the each category#

LocCount <- as.data.frame(table(CatLotData2$LOCATION))
View(LocCount)

#recode the factor to collapse the categories#



CatLotData2$NeighCat2 <- recode(CatLotData2$NeighCat, 'c("Admiral", "North Admiral", "Alki", "Genesee") = "Admiral"; c("White Center", "West Seattle") = "West Seattle"')

CatLotData2$NeighCat2<-factor(CatLotData2$LOCATION,levels=c('Alaska Junction','Belvidere','Admiral','Gatewood','Fairmount','Delridge','West Seattle','Fauntleroy','Westwood Village','High Point','Westwood','Arbor Heights'))     # this is to enforce a order to the reference for the factor variable



summary(CatLotData2$NeighCat2) 


CatFit1<-lm(PRICE~BATHS+BEDS+`SQUARE FEET`+LotCat+`YEAR BUILT`+ NeighCat2, data=CatLotData2)


summary(CatFit1)


#VIF regression#

vif(CatFit1)

#partial F-tests#

CatFit2<-lm(PRICE~BATHS+`SQUARE FEET`+LotCat+`YEAR BUILT`+ NeighCat2, data=CatLotData2)

summary(CatFit2)
anova(CatFit1, CatFit2)

CatFit3<-lm(PRICE~BATHS+BEDS+`SQUARE FEET`+`YEAR BUILT`+ NeighCat2, data=CatLotData2)
summary(CatFit3)
anova(CatFit1,CatFit3)

CatFit4<-lm(PRICE~BATHS+BEDS+`SQUARE FEET`+`YEAR BUILT`+ LotCat, data=CatLotData2)
summary(CatFit4)
anova(CatFit1,CatFit4)

CatFit5<-lm(PRICE~BATHS+`SQUARE FEET`+`YEAR BUILT`+ LotCat+ NeighCat2, data=CatLotData2)
summary(CatFit5)

anova(CatFit1, CatFit5)

summary(CatFit1)
summary(CatFit5)

CatFit6<-lm(PRICE~BATHS+`SQUARE FEET`+ CatLotData2$BATHS*CatLotData2$LotCat, data=CatLotData2)

summary(CatFit6)


