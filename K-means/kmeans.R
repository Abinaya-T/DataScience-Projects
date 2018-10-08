dat <- cbind(iris$Petal.Length,iris$Petal.Width)
library(ggplot2)
ggplot(iris, aes(Sepal.Length,Sepal.Width,color = Species))+geom_point()
ggplot(iris, aes(Sepal.Length,Sepal.Width))+geom_point()
ggplot(iris, aes(Sepal.Length,Sepal.Width,color = Species))+geom_point()
ggplot(iris, aes(Sepal.Width,Sepal.Length,color = Species))+geom_point()
ggplot(iris, aes(Petal.Width,Petal.Length,color = Species))+geom_point()
ggplot(iris, aes(Petal.Width,Petal.Length))+geom_point()
set.seed(20)
names(iris)
dim(iris)
iristst <- iris[,3:4]
#irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster <- kmeans(iris[, 3:4], 3)
iris[, 3:4]
irisCluster 
names(irisCluster)
irisCluster$centers
irisCluster$totss
irisCluster$withinss
irisCluster$betweenss
irisCluster$iter
#dist(irisCluster[1],irisCluster[5]) 
iristest<-iris
iristest<-edit(iristest)
test<-iristest[151,3:4]
test

install.packages("rJava")
install.packages("DeducerExtras")
library(DeducerExtras)
predict.kmeans(irisCluster,test)
table(irisCluster$cluster, iris$Species) [ Validating clusters externally]

library(dplyr)
iristest11<-mutate(iristst,cluster_val=irisCluster$cluster)
head(iristest11) 
ggplot(iristest11,aes(Petal.Length,Petal.Width,
                      color = cluster_val))+geom_point()
----------------------------------------------------------------------------------------------------------
# IDENTIFY THE NUMBER OF CLUSTERS - SOURCE



# CALCULATE TOTAL SUM OF SQUARES (WSS) WHEN CLUSTER = 1, var= VARIANCE
  
WSS <-((nrow(iris)-1)*sum(apply(iris[,3:4],2,var)))
WSS

# CALCULATE WITIN SUM OF SQUARES FOR CLUSTER = 2 TO 10
for (i in 2:10) WSS[i] <- sum(kmeans(iris[,3:4],centers=i)$withinss)


# PLOT "ELBOW DIAGRAM" AND SEE WHERE THE CHANGE HAPPENING
plot(1:10, WSS, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
