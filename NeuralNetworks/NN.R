concrete<-read.csv("C:\\Users\\ABINAYA-T\\Desktop\\Rexercise\\concrete.csv")
str(concrete)
install.packages("neuralnet")
library(neuralnet)
con<-concrete
str(con)
summary(concrete)  #Mean value of each variable difers lot from each othr, So normalisation is required

normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))  #min max method
}

# apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)
summary(concrete_norm)

scaled.con<-scale(con)
summary(scaled.con)
head(scaled.con)
scaled.cd<-apply(scaled.con,2,sd) 


# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]
## Step 3: Training a model on the data ----
# train the neuralnet model
library(neuralnet)
# simple ANN with only a single hidden neuron
set.seed(12345) # to guarantee repeatable results
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = concrete_train)

# visualize the network topology
plot(concrete_model)
summary(concrete_model)
## Step 4: Evaluating model performance ----
# obtain model results
model_results <- compute(concrete_model, concrete_test[1:8])
names(concrete_test)
names(model_results)
# obtain predicted strength values
predicted_strength <- model_results$net.result
str(predicted_strength)
head(predicted_strength)
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

## Step 5: Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
set.seed(12345) # to guarantee repeatable results
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)  #try with hidden=c(5,2)
summary(concrete_model2)
# plot the network
plot(concrete_model2)

# evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
