library(e1071)
ir<-iris
plot(ir)
attach(ir)
names(ir)
plot(Sepal.Length,Sepal.Width,col=Species)
plot(Petal.Length,Petal.Width,col=Species)
set.seed(123)
s<-sample(150,100)
col<-c("Petal.Length","Petal.Width","Species")
ir_train<-ir[s,col]
names(ir_train)
dim(ir_train)
ir_test<-ir[-s,col]
names(ir_test)
dim(ir_test)
#svm.model<-svm (Species~.,data=ir_train,cost=0.1,type='C',kernal="linear")
#svm.model
#plot(svm.model,ir_train)

mysvm = svm (Species~.,data=ir_train,cost=10,kernel='linear')
mysvm
plot(mysvm,ir_train)

tuned<-tune(svm,Species~.,data=ir_train,kernal="Linear",
            ranges=list(cost=c(0.001,.01,0.1,1,10,100)))  #Tuning fn is used to choose best cost value. Execute this first.

summary(tuned)
p<-predict(mysvm,ir_test,type="class")
plot(p)
class(p)
table(p,ir_test[,3])
mean(p==ir_test[,3])
b.20u
 