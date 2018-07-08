
library('tidyr')
setwd("~/Documents/Statistik/kaggle")

sample <- read.csv(file="sample.csv", stringsAsFactors = FALSE, header = TRUE)
set <- read.csv(file="housing_builds.csv", stringsAsFactors = TRUE, header = TRUE)

str(set)
summary(sample)

head(set)

sapply(set, class)  
  
set2 <-  c(set$Extremly.Low.Income.Units + set$Very.Low.Income.Units)
set2
### combine the first 3 income columns into a single column low
set <- gather(Extremly.Low.Income.Units + Very.Low.Income.Units + Low.Units)

  
  
set$Extremely.Low.Income.Units[set$Extremely.Low.Income.Units == 0]        <- 'low' 


set$income <- c(set$Extremly.Low.Income.Units + set$Very.Low.Income.Units + set$Low.Units)





set$Low.Income.Units[set$Low.Income.Units == 'low']        <- 'low'
set$Moderate.Income.Units[set$Moderate.Income.Units == 'low']        <- 'income' 
set$Middle.Income.Units[set$Middle.Income.Units == 'low']        <- 'income' 
#set$Other.Units[set$Other.Units == 'low']        <- 'income'
set$Extremely.Low.Income.Units
set$income

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'


model$income <- set$income