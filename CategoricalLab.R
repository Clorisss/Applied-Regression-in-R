library(readxl)
library(plyr)
library(tidyverse)
library(car)

setwd("C:/Users/mazhi/Documents/R")

CatLotData <- read_excel("C:/Users/mazhi/Documents/R/redfin_98075.xlsx")
View(CatLotData)

#1 FIRST, pull the variables we want
CatLotData2 <- CatLotData %>% select(PRICE,BEDS,BATHS,`SQUARE FEET`,`LOT SIZE`, `YEAR BUILT`,`LOCATION`, `PROPERTY TYPE`)
view(CatLotData2)
show(CatLotData2)

#2 THEN, take out missing data 
CatLotData2 <-  CatLotData2[complete.cases(CatLotData2), ]
show(CatLotData2)

#seperate Lot Size into categories#
CatLotData2$LotCat<-cut(CatLotData2$`LOT SIZE`, c(0,2500,5000,7500,10000,12500,15000,Inf))
View(CatLotData2)

#create factor variable for Neighborhoods#factor function, factor variable is NOT string variable. 
CatLotData2$NeighCat<-factor(CatLotData2$LOCATION)
LocCount <- as.data.frame(table(CatLotData2$LOCATION))
view(LocCount)
summary(CatLotData2$NeighCat)
summary(CatLotData2$PRICE)

aggregate(PRICE ~ NeighCat, CatLotData2, mean)

#enforce a order to the reference for the factor variable, the first one is reference.
CatLotData2$NeighCat<-factor(CatLotData2$LOCATION,levels=c('Lake Sammamish','East Lake Sammamish','Issaquah','Aldarra','Beaver Lake','Pine Lake','Sammamish','Trossachs','Plateau','Issaquah/Sammamish'))     

#count the values in the each category
LocCount <- as.data.frame(table(CatLotData2$NeighCat))
View(LocCount)

NeiborhoodInfo <- c("Lake Sammamish"=6,"East Lake Sammamish"=4,"Issaquah"=3,"Alderra"=9,"Beaver Lake"=31,"Pine Lake"=28,"Sammamish"=106,"Trossachs"=38,"Plateau"=13,"Issaquah/Sammamish"=7)
barplot(NeiborhoodInfo)


#recode the factor freq<5 to collapse the categories
CatLotData2$NeighCat2 <- recode(CatLotData2$NeighCat, 'c("East Lake Sammamish", "Lake Sammamish") = "Lake Sammamish"; c("Issaquah", "Issaquah/Sammamish") = "Issaquah"')
summary(CatLotData2$NeighCat2) 
#count the new values after recode
LocCount1 <- as.data.frame(table(CatLotData2$NeighCat2))
view(LocCount1)
#enforce the order to the reference again
CatLotData2$NeighCat2 <- factor(CatLotData2$NeighCat2,levels=c('Lake Sammamish','Issaquah','Aldarra','Beaver Lake','Pine Lake','Sammamish','Plateau','Trossachs'))  
summary(CatLotData2$NeighCat2)

aggregate(PRICE ~ NeighCat2, CatLotData2, mean)

#recode the property type
CatLotData2$TypeCat <- factor(CatLotData2$`PROPERTY TYPE`)
aggregate(PRICE ~ TypeCat, CatLotData2, mean)
TypeCount <- as.data.frame(table(CatLotData2$`PROPERTY TYPE`))
view(TypeCount)
CatLotData2$TypeCat <- factor(CatLotData2$`PROPERTY TYPE`, levels = c('Single Family Residential','Townhouse','Condo/Co-op'))
CatLotData2$TypeCat2 <- recode(CatLotData2$TypeCat, 'c("Townhouse","Condo/Co-op")="Non-single Family Residential"')
summary(CatLotData2$TypeCat2)
TypeCount <- as.data.frame(table(CatLotData2$TypeCat2))
view(TypeCount)
CatLotData2$TypeCat2 <- factor(CatLotData2$TypeCat2, levels = c('Non-single Family Residential','Single Family Residential'))
summary(CatLotData2$TypeCat2)

#regression
CatFit1<-lm(PRICE~BATHS+BEDS+`SQUARE FEET`+`YEAR BUILT`+ NeighCat2 + TypeCat2, data=CatLotData2)
summary(CatFit1)

#VIF regression# if GVIF number < 5, do not have collinearity. 
#If there's collinearity, we need to leave out a variable to run the lm again.
vif(CatFit1)

#partial F-tests, compare with the original regression
#Look for the P-value on ANOVA test, if P-value is significant leave the variable in, if not then out.
CatFit2<-lm(PRICE~BATHS+`SQUARE FEET`+`YEAR BUILT`+ NeighCat2 + TypeCat2, data=CatLotData2)
summary(CatFit2)
anova(CatFit1, CatFit2)

CatFit3<-lm(PRICE~BATHS+BEDS+`SQUARE FEET`+`YEAR BUILT`+ NeighCat2, data=CatLotData2)
summary(CatFit3)
anova(CatFit1,CatFit3)

#Final parsimonious model
CatFitFinal <- lm(PRICE~BATHS+`SQUARE FEET`+`YEAR BUILT`+ NeighCat2, data=CatLotData2)
summary(CatFitFinal)

CatFit4<-lm(PRICE~BATHS+BEDS+`SQUARE FEET`+`YEAR BUILT`+ LotCat, data=CatLotData2)
summary(CatFit4)
anova(CatFit1,CatFit4)

CatFit5<-lm(PRICE~BATHS+`SQUARE FEET`+`YEAR BUILT`+ LotCat+ NeighCat, data=CatLotData2)
summary(CatFit5)

anova(CatFit1, CatFit5)

summary(CatFit1)
summary(CatFit5)

CatFit6<-lm(PRICE~BATHS+`SQUARE FEET`+ CatLotData2$BATHS*CatLotData2$LotCat, data=CatLotData2)

summary(CatFit6)


