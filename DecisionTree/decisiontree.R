## Example: Identifying Risky Bank Loans ----
## Step 2: Exploring and preparing the data ----
credit <- read.csv("C:\\Users\\ABINAYA-T\\Desktop\\Rexercise\\credit.csv")
str(credit)
names(credit)
# look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)
edit(credit)
# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)
# look at the class variable
table(credit$default)
# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(12345)
#sample(10,5)
train_sample <- sample(1000, 900)
str(train_sample)
# split the data frames
credit_train <- credit[train_sample, ]  #selecting columns
dim(credit_train)
credit_test  <- credit[-train_sample, ]
dim(credit_test)
# check the proportion of class variable
table(credit_train$default)
table(credit_test$default)
#prop.table(credit_train$default)
#prop.table(credit_test$default)
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
## Step 3: Training a model on the data ----
# build the simplest decision tree50 - entropy split
install.packages("C50")
library(C50)
names(credit_train)
credit_model <- C5.0(credit_train[-17], credit_train$default)
names(credit_train[-17])
# display simple facts about the tree
credit_model
summary(credit_model)
credit_pred <- predict(credit_model, credit_test)

install.packages('party')
library(party)
credit_model_tree <- ctree(credit_train$default~.,data=credit_train)
plot(credit_model_tree)
# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(credit_test$default, credit_pred,prop.t = FALSE,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
## Step 5: Improving model performance ----
#Boosting  -  create strong/better model from weak learners
## Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,prop.t = FALSE,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#Bagging / Bootstrap aggaregation : create random no samples
#and create models..model predictions
# are combined using voting(classification) /  avaeraging (for numeric)
install.packages("ipred")
library(ipred)
#dependency package lava needs to be installed..#
names(credit_train)
credit_bagg <- bagging(default~., data=credit_train,
                       nbagg=25)
#The nbagg parameter is used to control the number of decision trees voting
#in the ensemble (with a default value of 25).
credit_bagg
credit_bagg_pred <- predict(credit_bagg, credit_test)
CrossTable(credit_test$default, credit_bagg_pred,prop.t = FALSE,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#randomForest::
library(randomForest)
credit_rforest <- randomForest(default~., data=credit_train)

credit_rforest
#plot(credit_rforest)
credit_rforest_pred <- predict(credit_rforest, credit_test,type =  "response")
CrossTable(credit_test$default, credit_rforest_pred, prop.t = FALSE,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
