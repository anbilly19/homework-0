library(dplyr)
library(tidyr)

hb_read<-read.csv('../Housing_builds/housing_builds.csv')
#select income and ownership columns
hb_mod<-hb_read%>%select(Counted.Homeownership.Units,Counted.Rental.Units,Extremely.Low.Income.Units,Low.Income.Units,Very.Low.Income.Units,Moderate.Income.Units,Middle.Income.Units,Other.Income.Units)%>%head(100)
#setting low and high income counts
hb_mod$low<-hb_mod$Extremely.Low.Income.Units+hb_mod$Low.Income.Units+hb_mod$Very.Low.Income.Units
hb_mod$high<-hb_mod$Moderate.Income.Units+hb_mod$Middle.Income.Units+hb_mod$Other.Income.Units

hb_mod<-hb_mod%>%select(high,low,Counted.Homeownership.Units,Counted.Rental.Units)

#setting new variable All.Units
hb_mod$All.Units<-hb_mod$Counted.Homeownership.Units+hb_mod$Counted.Rental.Units
#Percent low 
hb_mod$Percent.of.low.income.Units<-(hb_mod$low/hb_mod$All.Units)*100
#Percent high
hb_mod$Percent.of.high.income.Units<-(hb_mod$high/hb_mod$All.Units)*100

hb_mod

