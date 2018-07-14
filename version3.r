library(dplyr)
library(tidyr)
library(caTools)

setwd("~/Documents/Statistik/kaggle")
hb_read <- read.csv(file="housing_builds.csv", stringsAsFactors = TRUE, header = TRUE)

#select income and ownership columns
hb_mod<-hb_read%>%select(Counted.Homeownership.Units,Counted.Rental.Units,Extremely.Low.Income.Units,Low.Income.Units,Very.Low.Income.Units,Moderate.Income.Units,Middle.Income.Units,Other.Income.Units)
#setting low and high income counts
hb_mod$low<-hb_mod$Extremely.Low.Income.Units+hb_mod$Low.Income.Units+hb_mod$Very.Low.Income.Units
hb_mod$high<-hb_mod$Moderate.Income.Units+hb_mod$Middle.Income.Units+hb_mod$Other.Income.Units

hb_mod<-hb_mod%>%select(high,low,Counted.Homeownership.Units,Counted.Rental.Units)

#setting new variable All.Units
hb_mod$All.Units <- hb_mod$Counted.Homeownership.Units+hb_mod$Counted.Rental.Units
#Percent low 
hb_mod$Percent.of.low.income.Units <- (hb_mod$low/hb_mod$All.Units)*100
#Percent high
hb_mod$Percent.of.high.income.Units <- (hb_mod$high/hb_mod$All.Units)*100
#Percent of rental
hb_mod$Percent.of.rental.Units <- (hb_mod$Counted.Rental.Units/hb_mod$All.Units)*100
#Percent of homeownership
hb_mod$Percent.of.homeownership.Units <- (hb_mod$Counted.Homeownership.Units/hb_mod$All.Units)*100

hb_mod

#Next thing is regression. We want to predict our Dependent Variable 'Counted Homeownership Units' per House.
#We have 'low', 'high', 'TotalNumberOfHousingUnits' and our percentage of ____ per House as Independent Variables.
#I would say we take 100 cases and delete the Values of 'Counted Homeownership Units'.
#Then we make a Regression Analysis with the rest and predict the 100 cases. We want to reach 95 right cases.

require(caTools)
set.seed(101) 
sample = sample.split(hb_mod$All.Units, SplitRatio = .75)
train = subset(hb_mod, sample == TRUE)
test = subset(hb_mod, sample == FALSE)

#Linear Regression
#linearMod <- lm(train$Counted.Homeownership.Units ~ train$high + train$low + train$All.Units, data=hb_mod)  # build linear regression model on full data
#print(linearMod)

#Linear Regression
linearMod <- lm(Counted.Homeownership.Units ~ high +low + +Counted.Rental.Units, +All.Units, data=train)  # build linear regression model on train data
#All.Units was not producing a good fit the predict function said it was a rank-deficient model, dunno why
print(linearMod)
### NOT SIGNIFICANT: Percent.of.high.income.Units +Percent.of.homeownership.Units +Percent.of.low.income.Units +Percent.of.rental.Units
#simple prediciton
preds <- predict(linearMod, test)
test$predicted.units<-preds
as.data.frame(test$predicted.units,test$Counted.Homeownership.Units)
summary(linearMod)
#Here we test if our predicted values are relatively equal to the the actual values

test$TRUFALS <- near(test$Counted.Homeownership.Units, test$predicted.units, tol = test$Counted.Homeownership.Units * 0.05)
gezaehlt <- count(test, vars = TRUFALS)
gezaehlt
