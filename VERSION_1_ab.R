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

summary(hb_mod)
head(hb_mod)
#Next thing is regression. We want to predict our Dependent Variable 'Counted Homeownership Units' per House.
#We have 'low', 'high', 'TotalNumberOfHousingUnits' and our percentage of ____ per House as Independent Variables.
#I would say we take 100 cases and delete the Values of 'Counted Homeownership Units'.
#Then we make a Regression Analysis with the rest and predict the 100 cases. We want to reach 95 right cases.

#got this from here: https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
require(caTools)
set.seed(101) 
sample = sample.split(hb_mod$All.Units, SplitRatio = .75)
train = subset(hb_mod, sample == TRUE)
test  = subset(hb_mod, sample == FALSE)

#make a new column names Predicted.Homeownership.Units, empty
##mutate(the100, 'Predicted.Homeownership.Units' = NA) #do we really need this column?

#Linear Regression
linearMod <- lm(Counted.Homeownership.Units ~ high +low, data=train)  # build linear regression model on train data
#All.Units was not producing a good fit the predict function said it was a rank-deficient model, dunno why
print(linearMod)
#simple prediciton
preds <- predict(linearMod, test)
test$predicted.units<-preds
as.data.frame(test$predicted.units,test$Counted.Homeownership.Units)
#I was trying to get only two columns so we could compare but clearly something seems wrong.
