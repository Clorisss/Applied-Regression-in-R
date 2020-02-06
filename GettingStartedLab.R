install.packages("XLConnect") #installs package to read Excel spreadsheets
install.packages("tidyverse") #installs very powerful visualization package

library(readxl) 
library(tidyverse)
age_labor_data <- read_excel("C:/Users/mazhi/Documents/R/cpsaat03.xlsx",
                       sheet = "ByAge") #puts data in Global Environment#
View(age_labor_data) 

#summary statistics#
summary(age_labor_data)     

#rename variables#
NLF <-(age_labor_data$`Civilian labor force Not in Labor force`)
NIP <-(age_labor_data$`Civilian NI pop`) 
CLF<-(age_labor_data$`Civilian labor force Total`)

#create percentages#
NLFPer<-NLF/NIP  
NLFPer<-NLFPer*100  
NLFPer<-NLFPer

CLFPer <-CLF/NIP
CLFPer<-CLFPer*100 

#Bar Chart of Civilian Labor Force Total with labels by Age#
barplot(age_labor_data$`Civilian labor force Total`, names.arg = age_labor_data$Age, xlab="Age")

#Scatter Plot of Age and Civilian Labor Force by Percent of Population#
ggplot(data=age_labor_data)+geom_point(mapping = aes(x=Age, y=CLFPer), size=3)
ggplot(data=age_labor_data)+geom_point(mapping = aes(x=Age, y=CLFPer, size=3), color="green")
ggplot(data=age_labor_data)+geom_point(mapping = aes(x=Age, y=`Civilian labor force, percent of population`))

#Scatter Plot of Civilian Labor Force (Percent), Age, and Civilian Labor Force (Employed)#
CLFPercent <- age_labor_data$`Civilian labor force, percent of population`
CLFEmployed <- age_labor_data$`Civilian labor force employed total`
ggplot(data=age_labor_data)+geom_point(mapping = aes(x=Age, y=CLFPercent, size=CLFEmployed))

#Scatter Plot of Percent of Population Not in Labor Force by Age 
#with different color dots for Ages with labor force non-participation over 50%
age_labor_data_color <- cut(NLFPer, breaks = c(-Inf,49.99,Inf), labels = c("black","red"))
ggplot(data = age_labor_data)+geom_point(mapping = aes(x=Age, y=NLFPer), color=age_labor_data_color)


attach(age_labor_data)
names(age_labor_data)
class(Age)
cor(`Civilian labor force employed total`, `Civilian labor force Total`)
plot(`Civilian labor force employed total`,`Civilian labor force Total')
linear_model<-lm('Civilian labor force employed total'~'Civilian labor force Total')
summary(linear_model)