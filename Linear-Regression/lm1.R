library(ggplot2)
library(dplyr)
#install.packages("Hmisc")
library(Hmisc)
#install.packages("cowplot")
library(cowplot)
#install.packages("WVPlots")
library(WVPlots)
library(psych)
set.seed(123)
Data <- read.csv("C:\\Users\\ABINAYA-T\\Desktop\\Rexercise\\insurance.csv")
sample_n(Data, 5)   # display random 5 rows from dataset
describe(Data)
names(Data)

################ Exploratory Data Analysis ##########################

# 1. By using correlation func

cor(Data[,c("age","bmi","children","expenses")])

pairs.panels(Data[,c("age","bmi","children","expenses")])

# 2. Using ggplot 

x <- ggplot(Data, aes(age, expenses)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y <- ggplot(Data, aes(bmi, expenses)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("1. Correlation between expenses and Age / BMI", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

a <- ggplot(Data, aes(sex, expenses)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

b <- ggplot(Data, aes(children, expenses)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p1 <- plot_grid(a,b) 
title1 <- ggdraw() + draw_label("1. Correlation between expenses and sex / children", fontface='bold')
plot_grid(title1, p1, ncol=1, rel_heights=c(0.1, 1))


c <- ggplot(Data, aes(smoker, expenses)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

d <- ggplot(Data, aes(region, expenses)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p2 <- plot_grid(c,d) 
title2 <- ggdraw() + draw_label("1. Correlation between expenses and smoker / region", fontface='bold')
plot_grid(title2, p2, ncol=1, rel_heights=c(0.1, 1))

##################  Preparing & Splitting data #########################################

n_train <- round(0.8 * nrow(Data))
train_indices <- sample(1:nrow(Data), n_train)
Data_train <- Data[train_indices, ]
Data_test <- Data[-train_indices, ]

formula_0 <- as.formula("expenses ~ age + sex + bmi + children + smoker + region")

###################  Train & Test model ##################################################


model_0 <- lm(formula_0, data = Data_train)
summary(model_0)

#Saving R-squared
r_sq_0 <- summary(model_0)$r.squared

#predict data on test set
prediction_0 <- predict(model_0, newdata = Data_test)

#calculating the residuals
residuals_0 <- Data_test$charges - prediction_0

#calculating Root Mean Squared Error
rmse_0 <- sqrt(mean(residuals_0^2))

# Removing Sex and building another model, since Sex is insignificant variable

formula_1 <- as.formula("charges ~ age + bmi + children + smoker + region")

model_1 <- lm(formula_1, data = Data_train)
summary(model_1)

r_sq_1 <- summary(model_1)$r.squared

prediction_1 <- predict(model_1, newdata = Data_test)

residuals_1 <- Data_test$charges - prediction_1

rmse_1 <- sqrt(mean(residuals_1^2))


##############  Compare the Models #########################################

AIC(model_0,model_1)

print(paste0("R-squared for first model:", round(r_sq_0, 4)))
print(paste0("R-squared for second model:", round(r_sq_1, 4)))

print(paste0("RMSE for first model: ", round(rmse_0, 2)))
print(paste0("RMSE for second model: ", round(rmse_1, 2)))