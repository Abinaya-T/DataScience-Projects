install.packages("arules")
library(arules)
groceries <- read.csv("C:\\Users\\ABINAYA-T\\Desktop\\Rexercise\\groceries.csv", header=FALSE)
View(groceries)
summary(groceries)
str(groceries)
groceries <- read.transactions("C:\\Users\\ABINAYA-T\\Desktop\\Rexercise\\groceries.csv", sep = ",")
head(groceries)
summary(groceries)
class(groceries)   
inspect(groceries[1:5])
itemFrequency(groceries[,1:10])
itemFrequencyPlot(groceries,support=.1)
itemFrequencyPlot(groceries,topN=20)
image(groceries[1:5])
image(sample(groceries,100))
apriori(groceries)
groceriesrules <- apriori(groceries,parameter = list(support=0.006,confidence = 0.25,minlen=2))  # since there is 0 rules created with support 0.1, change it to 0.006
groceriesrules
summary(groceriesrules)

str(groceriesrules)
class(groceriesrules)

inspect(groceriesrules[1:10])
inspect(sort(groceriesrules,by="lift"))
inspect(sort(groceriesrules,by="lift")[1:5])
berryrules <- subset(groceriesrules,items %in% "berries")
inspect(berryrules)
berryrules1 <- subset(groceriesrules,items %ain% c("berries","yogurt"))
inspect(berryrules1)

#berryrules <- subset(groceriesrules,items %in% "whole bread")
#inspect(berryrules)

#write(groceriesrules,file = "F:/RProgramming/MachineLearning/groceriesrules.csv",sep=",", row.names=FALSE, quote=TRUE)
groceriesrulesdf <- as(groceriesrules,"data.frame")
View(groceriesrulesdf )
str(groceriesrulesdf)
image(groceries[1:5])
image(sample(groceries, 100))

#groceriesrulesdfsort <- sort(as(groceriesrules,"data.frame"),by="rules")
q()



#items %in% c("berries", "yogurt").
#Additional operators are available for partial matching (%pin%) and complete matching (%ain%). Partial matching allows you to find both citrus fruitand tropical fruit using one search: items %pin% "fruit". Complete matching requires that all listed items are present. For instance, items %ain% c("berries", "yogurt") finds only rules with both berries and yogurt.
