library(readxl)
library(tidyverse)
library(mi)
library(car)

setwd("C:/Users/mazhi/Documents/R")

FinalLabData2 <- read_excel("C:/Users/mazhi/Documents/R/Airbnb_Singapore_clean.xlsx")
View(FinalLabData2)

FinalLAB3 <- FinalLabData2 %>% select(host_id, neighbourhood_group, neighbourhood, latitude, longitude, room_type, price, minimum_nights, number_of_reviews, last_review, reviews_per_month, calculated_host_listings_count,  availability_365
)
View(FinalLAB3)

#to delete missing data#
summary(FinalLAB3)
FinalLAB4 <-  FinalLAB3[complete.cases(FinalLAB3), ]
view(FinalLAB4)

#corrplot
install.packages("corrplot")
library(corrplot)
data("FinalLAB4")
Airbnb <- FinalLAB4[, c("host_id","price","number_of_reviews","calculated_host_listings_count","latitude","longitude","minimum_nights","reviews_per_month","availability_365","ReviewAge")]
M <- cor(Airbnb)
corrplot(M, method = "circle")

# Scatterplots of each explanatory variables versus price (y). 
ggplot(data=FinalLAB4, aes(x=FinalLAB4$latitude, y=FinalLAB4$price)) + geom_point(size=2) + geom_smooth(method = "lm")
ggplot(data=FinalLAB4, aes(x=FinalLAB4$longitude, y=FinalLAB4$price)) + geom_point(size=2) + geom_smooth(method = "lm")
ggplot(data=FinalLAB4, aes(x=FinalLAB4$minimum_nights, y=FinalLAB4$price)) + geom_point(size=2)+ geom_smooth(method = "lm")
ggplot(data=FinalLAB4, aes(x=FinalLAB4$availability_365, y=FinalLAB4$price)) + geom_point(size=2)+ geom_smooth(method = "lm")
ggplot(data=FinalLAB4, aes(x=FinalLAB4$host_id, y=FinalLAB4$price)) + geom_point(size=2)+ geom_smooth(method = "lm")
ggplot(data=FinalLAB4, aes(x=FinalLAB4$number_of_reviews, y=FinalLAB4$price)) + geom_point(size=2)+ geom_smooth(method = "lm")
ggplot(data=FinalLAB4, aes(x=FinalLAB4$reviews_per_month, y=FinalLAB4$price)) + geom_point(size=2)+ geom_smooth(method = "lm")
ggplot(data=FinalLAB4, aes(x=FinalLAB4$calculated_host_listings_count, y=FinalLAB4$price)) + geom_point(size=2)+ geom_smooth(method = "lm")

#histograms of all variables.
hist(FinalLAB4$latitude)#I thikn it is ok
hist(FinalLAB4$longitude)# I think this is ok too
hist(FinalLAB4$minimum_nights)#skwed right, need log 
hist(FinalLAB4$availability_365)#Factor it, I don't know why factor it? 
hist(FinalLAB4$number_of_reviews)#skwed right, need log
hist(FinalLAB4$reviews_per_month)#skwed right, need log
hist(FinalLAB4$calculated_host_listings_count)#needs log conversion
hist(FinalLAB4$price)


#logs#
FinalLAB4$LOGMinimum_nights <- log(FinalLAB4$minimum_nights)
hist(FinalLAB4$LOGMinimum_nights) #still looks not very normal, like right skewed
FinalLAB4$LOGavailability <- log(FinalLAB4$availability_365+0.01)
hist(FinalLAB4$LOGavailability)
FinalLAB4$LOGNumberOfReviews <- log(FinalLAB4$number_of_reviews+0.01)
hist(FinalLAB4$LOGNumberOfReviews) # looks better, but still a little right skewed
FinalLAB4$LOGReviewsPerMonth <- log(FinalLAB4$reviews_per_month)
hist(FinalLAB4$LOGReviewsPerMonth) # looks perfect!
FinalLAB4$LOGCalculatedHostListingCount <- log(FinalLAB4$calculated_host_listings_count)
hist(FinalLAB4$LOGCalculatedHostListingCount) # looks better, but a little right skewed
FinalLAB4$LOGprice <- log(FinalLAB4$price)
hist(FinalLAB4$LOGprice)

summary(FinalLAB4$LOGprice)
sum(is.na(FinalLAB4$LOGprice))

#Factor neighbourhood_group into categories
FinalLAB4$CatNeighGroup<-factor(FinalLAB4$neighbourhood_group,levels=c('Central Region','North-East Region','East Region','West Region','North Region'))     
#Factor room_type into categories
FinalLAB4$CatRoomType<-factor(FinalLAB4$room_type,levels=c('Shared room','Private room','Entire home/apt'))     
#Create factor variable for Neighborhoods
FinalLAB4$NeighCat<-factor(FinalLAB4$neighbourhood)
summary(FinalLAB4$NeighCat)
aggregate(FinalLAB4$LOGprice ~ FinalLAB4$NeighCat, FinalLAB4, mean)
FinalLAB4$NeighCat<-factor(FinalLAB4$neighbourhood,levels=c('Tuas','Southern Islands','Bukit Panjang','Marina South','Orchard','Museum','Downtown Core','Newton','Tanglin'
                                                           ,'Clementi','Novena','Singapore River','Rochor','Marine Parade','River Valley','Outram','Bukit Merah','Geylang','Kallang','Bedok','Queenstown','Central Water Catchment' ,'Bukit Timah','Jurong East'  ,'Toa Payoh','Bishan', 'Tampines', 
                                                           'Yishun','Bukit Batok','Woodlands','Sembawang','Hougang','Serangoon','Jurong West','Choa Chu Kang','Pasir Ris','Ang Mo Kio','Punggol','Sengkang','Mandai','Sungei Kadut','Western Water Catchment'))     
View(FinalLAB4$NeighCat)
summary(FinalLAB4$NeighCat)
#count the values in the each category#
LocCount <- as.data.frame(table(FinalLAB4$NeighCat))
View(LocCount)

#recode the factor to collapse the categories#
FinalLAB4$NeighCat2 <- recode(FinalLAB4$NeighCat, 'c("Downtown Core","Tuas","Southern Islands","Bukit Panjang","Marina South","Orchard","Museum","Newton","Tanglin","Clementi") = "Downtown Core"; c("Novena","Singapore River","Rochor","Marine Parade","River Valley","Outram","Bukit Merah","Geylang","Kallang","Bedok") = "Novena"; c("Queenstown","Central Water Catchment","Bukit Timah","Jurong East","Toa Payoh","Bishan","Tampines","Yishun","Bukit Batok","Woodlands","Sembawang","Hougang","Serangoon") = "Queenstown";c("Jurong West","Choa Chu Kang","Pasir Ris","Ang Mo Kio","Punggol","Sengkang","Mandai","Sungei Kadut","Western Water Catchment")="Jurong West"')
####FinalLAB4$NeighCat3<-factor(FinalLAB4$NeighCat2,levels=c('Tuas','Southern Islands','Bukit Panjang','Marina South','Orchard','Museum','Downtown Core','Newton','Tanglin'
####                                                         ,'Clementi','Novena','Singapore River','Rochor','Marine Parade','River Valley','Outram','Bukit Merah','Geylang','Kallang','Bedok','Queenstown','Central Water Catchment' ,'Bukit Timah','Jurong East'  ,'Toa Payoh','Bishan', 'Tampines', 
####                                                         'Yishun','Bukit Batok','Woodlands','Sembawang','Hougang','Serangoon','Jurong West','Choa Chu Kang','Pasir Ris','Ang Mo Kio','Punggol','Sengkang','Mandai','Sungei Kadut','Western Water Catchment'))     
summary(FinalLAB4$NeighCat2) 
view(FinalLAB4)

#Availability into categories
FinalLAB4$CatAvailability<-cut(FinalLAB4$availability_365, c(0,60,120,180,240,365))

#Factor last_review, Age of last review into categories
FinalLAB4$ReviewAge<-as.integer(Sys.Date()-as.Date(FinalLAB4$last_review))
View(FinalLAB4$ReviewAge)
FinalLAB4$CutReviewAge<-cut(FinalLAB4$ReviewAge, c(0,30,90,180,365,750,Inf))
FinalLAB4$CatReviewAge<-revalue(FinalLAB4$CutReviewAge, c("(0,30]"="1 month old","(30,90]"="quater",
                                                       
                                                       "(90,180]"="half","(180,365]"="year","(365,750]"="Last year","(750,Inf]"="Old" ))

#Regression
FinalFitLog<-lm(FinalLAB4$LOGprice ~ FinalLAB4$CatNeighGroup+FinalLAB4$CatRoomType+FinalLAB4$LOGMinimum_nights+FinalLAB4$LOGNumberOfReviews+FinalLAB4$LOGCalculatedHostListingCount+FinalLAB4$LOGReviewsPerMonth+FinalLAB4$LOGavailability+FinalLAB4$NeighCat2+FinalLAB4$CatReviewAge+FinalLAB4$latitude+FinalLAB4$longitude , data=FinalLAB4)
summary(FinalFitLog)
FinalFitLog2<-lm(FinalLAB4$LOGprice ~ FinalLAB4$CatNeighGroup+FinalLAB4$CatRoomType+FinalLAB4$LOGMinimum_nights+FinalLAB4$LOGNumberOfReviews+FinalLAB4$LOGCalculatedHostListingCount+FinalLAB4$reviews_per_month+FinalLAB4$LOGavailability+FinalLAB4$NeighCat2+FinalLAB4$CatReviewAge+FinalLAB4$latitude+FinalLAB4$longitude , data=FinalLAB4)
summary(FinalFitLog2)

#Residuals
PriceLOG.stdres <-residuals(FinalFitLog)

ggplot(data=FinalLAB4, aes(x=availability_365, y=PriceLOG.stdres))+geom_point()
ggplot(data=FinalLAB4, aes(x=LOGMinimum_nights, y=PriceLOG.stdres))+geom_point()
ggplot(data=FinalLAB4, aes(x=LOGReviewsPerMonth, y=PriceLOG.stdres))+geom_point()
ggplot(data=FinalLAB4, aes(x=LOGNumberOfReviews, y=PriceLOG.stdres))+geom_point()
ggplot(data=FinalLAB4, aes(x=LOGCalculatedHostListingCount, y=PriceLOG.stdres))+geom_point()
ggplot(data=FinalLAB4, aes(x=latitude, y=PriceLOG.stdres))+geom_point()
ggplot(data=FinalLAB4, aes(x=longitude, y=PriceLOG.stdres))+geom_point()


#Regression
CatFit1<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+CatAvailability+LOGMinimum_nights+LOGNumberOfReviews+LOGReviewsPerMonth+LOGCalculatedHostListingCount+latitude+longitude+NeighCat2+FinalLAB4$host_id+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit1)

#VIF regression#
vif(CatFit1)

#partial F-tests#

#Without Category Neighbourhood group
CatFit2<-lm(FinalLAB4$price~CatRoomType+CatAvailability+LOGMinimum_nights+LOGNumberOfReviews+LOGReviewsPerMonth+LOGCalculatedHostListingCount+latitude+longitude+NeighCat2+FinalLAB4$host_id+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit2)
anova(CatFit1, CatFit2)

#Without Category room type
CatFit3<-lm(FinalLAB4$price~CatNeighGroup+CatAvailability+LOGMinimum_nights+LOGNumberOfReviews+LOGReviewsPerMonth+LOGCalculatedHostListingCount+latitude+longitude+NeighCat2+FinalLAB4$host_id+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit3)
anova(CatFit1, CatFit3)

#Without Category Availability
CatFit4<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+LOGMinimum_nights+LOGNumberOfReviews+LOGReviewsPerMonth+LOGCalculatedHostListingCount+latitude+longitude+NeighCat2+FinalLAB4$host_id+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit4)
anova(CatFit1, CatFit4)

#Without log minimum nights
CatFit5<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+CatAvailability+LOGNumberOfReviews+LOGReviewsPerMonth+LOGCalculatedHostListingCount+latitude+longitude+NeighCat2+FinalLAB4$host_id+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit5)
anova(CatFit1, CatFit5)

#Without log number of reviews
CatFit6<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+CatAvailability+LOGMinimum_nights+LOGReviewsPerMonth+LOGCalculatedHostListingCount+latitude+longitude+NeighCat2+FinalLAB4$host_id+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit6)
anova(CatFit1, CatFit6)

#Without log reviews per month
CatFit7<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+CatAvailability+LOGMinimum_nights+LOGNumberOfReviews+LOGCalculatedHostListingCount+latitude+longitude+NeighCat2+FinalLAB4$host_id+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit7)
anova(CatFit1, CatFit7)

#Without calculated host listing count
CatFit8<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+CatAvailability+LOGMinimum_nights+LOGNumberOfReviews+LOGReviewsPerMonth+latitude+longitude+NeighCat2+FinalLAB4$host_id+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit8)
anova(CatFit1, CatFit8)

#without latitude
CatFit9<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+CatAvailability+LOGMinimum_nights+LOGNumberOfReviews+LOGReviewsPerMonth+LOGCalculatedHostListingCount+longitude+NeighCat2+FinalLAB4$host_id+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit9)
anova(CatFit1, CatFit9)


#Without longitude
CatFit10<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+CatAvailability+LOGMinimum_nights+LOGNumberOfReviews+LOGReviewsPerMonth+LOGCalculatedHostListingCount+latitude+NeighCat2+FinalLAB4$host_id+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit10)
anova(CatFit1, CatFit10)

#Without Categorized neibourhood
CatFit11<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+CatAvailability+LOGMinimum_nights+LOGNumberOfReviews+LOGReviewsPerMonth+LOGCalculatedHostListingCount+latitude+longitude+FinalLAB4$host_id+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit11)
anova(CatFit1, CatFit11)

#without host id 
CatFit12<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+CatAvailability+LOGMinimum_nights+LOGNumberOfReviews+LOGReviewsPerMonth+LOGCalculatedHostListingCount+latitude+longitude+NeighCat2+FinalLAB4$CatReviewAge, data=FinalLAB4)
summary(CatFit12)
anova(CatFit1, CatFit12)

#Without ReviewAge
CatFit13<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+CatAvailability+LOGMinimum_nights+LOGNumberOfReviews+LOGReviewsPerMonth+LOGCalculatedHostListingCount+latitude+longitude+NeighCat2+FinalLAB4$host_id, data=FinalLAB4)
summary(CatFit13)
anova(CatFit1, CatFit13)

#parsimonous model #Removed host id,latitude,LOGREVIEWSPERMONTH,CatreviewAge  #Regression
CatFit14<-lm(FinalLAB4$price~CatNeighGroup+CatRoomType+CatAvailability+LOGMinimum_nights+LOGNumberOfReviews+LOGCalculatedHostListingCount+longitude+NeighCat2, data=FinalLAB4)
summary(CatFit14)
 


