#set working directory
setwd("C:/Users/GUSTAV/Desktop/Data Science Training")

#import data files
library(readr)
train <- read_csv("C:/Users/GUSTAV/Desktop/Data Science Training/train.csv")
library(readr)
test <- read_csv("C:/Users/GUSTAV/Desktop/Data Science Training/test.csv")

#look at structure of data frame
str(train)

#generate frequency table for column survived
table(train$Survived)

#freq table in %
prop.table(table(train$Survived))

#add column survived in test data. <- is used to store value to specified variable. rep is repeats a value by number of times specified
test$Survived<-rep(0,418)

#create data frame and submit
submit<-data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = False)

#recode from chr to fac
train$Sex <- as.factor(train$Sex)

#get summary for gender
summary(train$Sex)

#make cross tab for survival and sex
prop.table(table(train$Sex, train$Survived))

#cross tab table by first dimension
prop.table(table(train$Sex, train$Survived), 1)

#all female passengers survive
test$survived[test$Sex == 'female'] <- 1

#tag all minors
train$Child <- 0
train$Child[train$Age < 18] <- 1

#get number of survivors using aggregate function
#aggregate(target variable ~ var1 to subset + var2 to subset, specify data table, specift function)
#get number survivor count via sum function
aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
#get total number of passengers via length function
aggregate(Survived ~ Child + Sex, data = train, FUN = length)
#get proportion by using a function to divide no of survivors with total passengers
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x)/length(x)})

#recode fare to 3 classes
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

#get proportion based on new grouping variable Fare2
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) {sum(x)/length(x)})

#new predictions based on gender and class
test$Survived <- 0
test$Survived[test$Sex == 'female'] <-1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >=20] <-0

#import recursive partitioning and regression trees
library(rpart)

#rpart(dep var ~ ind vars, data, method)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = 'class')

#examine tree, install other packages to get more readable results
plot(fit)
text(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#make prediction using decision trees
Prediction <- predict(fit, test, type = "class")
submit <-data.frame(PassengerID = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#get 1st record from train data in name column
train$Name[1]

#add columns to test data to match columns in train data and then combine
test$Survived <-NA
test$Child <- NA
test$Fare2 <- NA
combi <- rbind(train,test)

#split name [[1]] - splits only once per characted, [2] - extracts second container
strsplit(combi$Name[1], split='[,.]')[[1]][2]
#running this script over the whole name column would return "Mr" for all cells

#use a function to apply the script to all records
combi$Title <- sapply(combi$Name, FUN = function (x){strsplit(x, split = '[,.]')[[1]][2]})

#combine titles
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#generate family Id by getting number of family members and combining with surname
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN = function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

#find and recode families with only 1 or 2 members
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <=2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

#split combi table to train and test
train <- combi[1:891,]
test <- combi[892:1309,]

#make model based on engineered variables, plot, fit and submit
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, method = "class")
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <-data.frame(PassengerID = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "featureeng.csv", row.names = FALSE)

#data cleansing: finding and replacing NAs
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = combi[!is.na(combi$Age),],method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
which(combi$Embarked == '')
combi$Embarked[c(62,830)]= "S"
combi$Embarked <- factor(combi$Embarked)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#reduce classes in familyID by including families with 3 members in 'small' category
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <=3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

#install randomForest
install.packages('randomForest')
library(randomForest)

#create random forest (set seed)
set.seet(415)
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,data = train, importance = TRUE, ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit,test)
Prediction <- predict(fit,test)
submit <-data.frame(PassengerID = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)
