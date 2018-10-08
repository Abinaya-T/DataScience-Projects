swiss<-read.csv("C:\\Users\\ABINAYA-T\\Desktop\\Rexercise\\swiss.csv")
names(swiss)
summary(swiss)
summary(lm(Fertility~.,data=swiss))
str(swiss)
dim(swiss)
head(swiss)
sw<-swiss
class(sw)
sum(is.na(sw))  # How many Na's present
sw0<-na.omit(sw)  #To remove na values
library(psych)
sws<-sw[,c(-1,-2)]  #remove 1st 2 colm
names(sw[,c(-1,-2)])
pairs.panels(sws[1:5])
cor(sws)
names(sws)
apply(sws,2,mean)  #to check mean of individual col, If means varies, then do normalization
library(nFactors)
ev.sws<- eigen(cor(sws))   #Eigen values determines how many factors (must be greater than 1) here factor=2
ev.sws
swiss.pca = prcomp(sws,center = TRUE,scale = TRUE)
swiss.pca
summary(swiss.pca)
screeplot(swiss.pca, type="barplot")
screeplot(swiss.pca, type="line")
abline(h=1, col="red", lty= 3)
names(swiss.pca)
swiss.pca$sdev
swiss.pca$sdev ^ 2 
which(swiss.pca$sdev ^ 2> 1)
PCAmodel.rot.no<- principal(sws,nfactors = 2,rotate = "none" ,scores = T,method = "regression")
PCAmodel.rot.yes<- principal(sws,nfactors = 2,rotate = "varimax",scores = T,method = "regression")
PCAmodel.rot.yes
fitpca<-PCAmodel.rot.yes
fitpca$loadings
fitpca$scores
summary(fitpca)
fitpca$communality
fitpca$loadings
pcascore <- fitpca$scores
class(pcascore)
head(pcascore)
dim(pcascore)
pcascore <- as.data.frame(pcascore)
class(pcascore)
pcascore<-cbind(sw$Fertility,pcascore)
names(pcascore)
pcascore.new<-fix(pcascore)
class(pcascore.new)
#------------------------------------------------
dim(pcascore.new)
train.pca<-pcascore.new[1:30,]
test.pca<-pcascore.new[31:47,]
dim(train.pca)
dim(test.pca)
pcamodel<-lm(Fertility ~.,data=train.pca)
summary(pcamodel)
#names(pcamodel)
swspredval<- predict(pcamodel,test.pca)
cor(swspredval,test.pca$Fertility)
