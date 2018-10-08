sms_raw <- read.csv("https://raw.githubusercontent.com/brenden17/sklearnlab/master/spam/sms_spam.csv", stringsAsFactors = FALSE)
#sms_raw1 <- read.csv("C:/Users/arthirathi/Desktop/RExercise/Rpractise/ML/naivebayes/sms_spam.csv", stringsAsFactors = FALSE)
#sms_rawn <- read.csv("C:\\Users\\arthirathi\\Desktop\\Peopleclick\\worksheet-Algorithms\\3-naivebayes_Wordcount\\sms_spam.txt",stringsAsFactors = FALSE)
#str(sms_rawn)
str(sms_raw)
str(sms_rawbk)
head(sms_raw)
sms_rawbk<-sms_raw
#sms_raw<-SMSSpamCollection
names(sms_raw)
#names(sms_raw)<-edit(names(sms_raw))
#sms_raw$type <- factor(sms_raw$type)
str(sms_raw)
table(sms_raw$type)
#install.packages("tm")
#library(tm)
remove.packages("tm")
install.packages("http://cran.r-project.org/bin/windows/contrib/3.0/slam_0.1-32.zip",repos=NULL)
install.packages("http://cran.r-project.org/bin/windows/contrib/3.0/tm_0.5-10.zip",repos=NULL)
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])
corpus_clean <- tm_map(sms_corpus, tolower)  # To change all characters to lower letters
#corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))
#Error: inherits(doc, "TextDocument") is not TRUE i --  for resolving this 
#error below plainTextDocument needs to run
#corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
inspect(corpus_clean[1:3])
sms_dtm <- DocumentTermMatrix(corpus_clean)
findFreqTerms(sms_dtm,100) 
findAssocs(sms_dtm, terms = "hope", corlimit = 0.1)
#Each of these words occurred more that 100 times.#
inspect(sms_dtm[1:10, 3100:3110])
inspect(sms_dtm[1:10, 20:30])        
#5559*7934        
#44062448/44105106 

sms_dtm
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5559, ]
head(sms_raw_train)
head(sms_raw_test)
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
inspect(sms_dtm_train[1:10, 3100:3110])
inspect(sms_dtm_test[1:10, 3100:3110])
findFreqTerms(sms_dtm_train, 5)
findAssocs(sms_dtm_train, terms = "call", corlimit = 0.2)

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]
inspect(sms_corpus_train[1:3])
inspect(sms_corpus_test[1:3])
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
#install.packages("wordcloud")
library(wordcloud)
wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")
names(spam)
head(spam)
head(ham)
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))


sms_dict <- Dictionary(findFreqTerms(sms_dtm_train, 5))
##########creating Dictionary function - because default Dictionary 
### function not working.. so creaing as a function
Dictionary <- function(x) {
  if( is.character(x) ) {
    return (x)
  }
  stop('x is not a character vector')
}
sms_dict
#class(sms_dict)
#A dictionary is a data structure allowing us to specify which words should appear in
#a document term matrix. To limit our training and test matrixes to only the words in
#the preceding dictionary, use the following commands:
sms_train <- DocumentTermMatrix(sms_corpus_train,
                                list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test,
                               list(dictionary = sms_dict))
sms_train
#class(sms_train)
#class(sms_test)
#dim(sms_train)
#dim(sms_test)
inspect(sms_train[1:10, 100:110])
inspect(sms_test[1:10, 100:110])
#The naive Bayes classifier is typically trained on data with categorical features. This
#poses a problem since the cells in the sparse matrix indicate a count of the times a
#word appears in a message. We should change this to a factor variable that simply
#indicates yes or no depending on whether the word appears at all.
convert_counts <- function(x) 
{
  x <- ifelse(x > 0,1,0)
  x <- factor(x, levels = c(0,1), labels = c("No", "Yes"))
  return(x)
}
#The apply() function allows a function to be used on each of the rows or columns
#in a matrix. It uses a MARGIN parameter to specify either rows or columns. Here, we'll
#use MARGIN = 2 since we're interested in the columns (MARGIN = 1 is used for rows).
#The full commands to convert the training and test matrixes are as follows:
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)
class(sms_train)
head(sms_train)
head(sms_test) 
#names(sms_train)

install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_test_pred <- predict(sms_classifier, sms_test) 

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
ctbnb<-table(sms_test_pred2, sms_raw_test$type)
library(caret)
confusionMatrix(ctbnb,positive = "spam")
sensitivity(sms_test_pred2, sms_raw_test$type,positive = "spam")
specificity(sms_test_pred2, sms_raw_test$type,negative = "ham")
posPredValue(sms_test_pred2, sms_raw_test$type,positive = "spam")
negPredValue(sms_test_pred2, sms_raw_test$type,negative = "ham")
#-------------------------------------
#accuracy:
(1204+152)/(1204+152+31+3)  = 0.9755   
#The Kappa statistic (or value)
#is a metric that compares an Observed Accuracy
#with an Expected Accuracy (random chance). 
#Total: (1390)
#Ground truth: ham (1204+3), spam (31+152)
#Machine Learning Classifier: ham (1204+31), spam (3+152)
#Observed Accuracy: ((1204+ 152) /1390) = 0.9755
#Expected Accuracy: ((1207 * 1235 /1390) + (183 * 155 / 1390)) / 1390 = 0.7861964
#Kappa = (observed accuracy - expected accuracy)/(1 - expected accuracy)
1204+3+31+152 = 1390
((1207 * 1235 /1390) + (183 * 155 / 1390)) / 1390
(0.9755 - 0.7861964 ) /  (1 - 0.7861964)  =  0.8856 
#---------------Change X and Y positions------------------
#Ground truth: ham (1204+31), spam (3+152)
#Machine Learning Classifier: ham (1204+3), spam (31+152)
#Expected Accuracy: ((1235 * 1207 /1390) + (155 * 183 / 1390)) / 1390 = 0.7861964
#Kappa = (observed accuracy - expected accuracy)/(1 - expected accuracy)
#(0.9755 - 0.7861964 ) /  (1 - 0.7861964)
-------------------------------
  #calculations:
  ((1235 * 1207 /1390) + (155 * 183 / 1390)) / 1390
###########################################################################
library(ROCR)
#pred <- prediction(c(0.1,.5,.3,.8,.9,.4,.9,.5), c(0,0,0,1,1,1,1,1))
#perf <- performance(pred, "tpr", "fpr")
#plot(perf)
str(sms_test_pred2)
#edit(sms_test_pred2)
str(sms_raw_test$type)
#str(sms_test)
sms_test_pred2_c<-as.character(sms_test_pred2)
#sms_raw_test$type_c<-as.character(sms_raw_test$type)
sms_raw_test$type_c<-as.character(sms_raw_test$type)
head(sms_raw_test)
#sms_test$type_c_df<-as.mat(sms_test$type_c)
#str(sms_test_pred2_c)
#mode(sms_test_pred2_c)
#class(sms_test_pred2_c)
#str(sms_raw_test$type_c)
#edit(sms_test_pred2_c)
#str(sms_raw_test$type_c)
# cbind of two factors will give numeric output only.. ie it takes integer value 
# factor and do cbind
newvar<-cbind(sms_test_pred2_c,sms_raw_test$type_c)
edit(newvar)
class(newvar)
#newvar_df<-as.data.frame(as.character(newvar))
newvar_df<-as.data.frame(newvar,stringsAsFactors = FALSE)
str(newvar_df)
head(newvar_df)
#colnames(newvar_df) <- c("pred","actual")
#head(newvar_df)
#str(newvar_df)
#edit(newvar_df)
#head(newvar_df$pred)
#newvar_df_pred<- newvar_df$pred
#head(newvar_df_pred)
#newvar_df_actual<-newvar_df$actual
#pred = prediction(predictions = newvar_df_pred, newvar_df_actual)
convert_binary <- function(x) {
  x <- ifelse(x == "ham", 1,0)
  return(x)
}
newvar_b <- apply(newvar_df, MARGIN = 2, convert_binary)
edit(newvar_b)
class(newvar_b)
#names(newvar_b)
str(newvar_b)
newvar_b_df<-as.data.frame(newvar_b,stringsAsFactors = F)
str(newvar_b_df)
class(newvar_b_df)
edit(newvar_b_df)
library(dplyr)
newvar_b_df1<-rename(newvar_b_df,pred=sms_test_pred2_c,actual=V2)
names(newvar_b_df1)
predt = prediction(predictions = newvar_b_df1$pred, newvar_b_df1$actual)
rocpre<- predt
perf <- performance(rocpre,"tpr","fpr")
plot(perf,colorize=T)
abline(a=0, b=1,lwd=2,lty=2)