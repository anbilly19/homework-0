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
#percentage low 
hb_mod$Percent.of.low.income.Units<-(hb_mod$low/hb_mod$All.Units)*100
#percentage high
hb_mod$Percent.of.high.income.Units<-(hb_mod$high/hb_mod$All.Units)*100
#percentage of rental
hb_mod$Percent.of.rental.Units<-(hb_mod$Counted.Rental.Units/hb_mod$All.Units)*100
#percentage of homeownership
hb_mod$Percent.of.homeownership.Units<-(hb_mod$Counted.Homeownership.Units/hb_mod$All.Units)*100

hb_mod

#Next thing is regression. We want to predict our Dependent Variable 'Counted Homeownership Units' per House.
#We have 'low', 'high', 'TotalNumberOfHousingUnits' and our percentage of ____ per House as Independent Variables.
#I would say we take 100 cases and delete the Values of 'Counted Homeownership Units'.
#Then we make a Regression Analysis with the rest and predict the 100 cases. We want to reach 95 right cases.

#Take 100
the100 <- tail(hb_mod, 100)

#make a new column names Predicted.Homeownership.Units, empty
mutate(the100, 'Predicted.Homeownership.Units' = NA)

#Linear Regression
linearMod <- lm(hb_mod$Counted.Homeownership.Units ~ hb_mod$high + hb_mod$low + hb_mod$All.Units, data=hb_mod)  # build linear regression model on full data
print(linearMod)
