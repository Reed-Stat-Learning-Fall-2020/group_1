library(tidyverse)

#Load data and select variables (same variables Josh picked)
Mega_Summary <- read_csv("Models/Mega Summary.csv")


data_selected <- Mega_Summary %>%
  select(Pitches, BA, Hits,`Spin Rate`, Velocity,
         W, L, ERA, HR, BB, SO, xwOBA, `K %`,
         `Barrel %`, Salary, `K %`:`Barrel %`, `ERA (t+1)`,
         `Salary (t+1)`, Pitcher, Year) %>% select(-Pitcher)

Mega_Summary<-Mega_Summary%>% select(-Pitcher,-Salary,-ΔSalary,-Year,-ΔERA)

#Split into test and train with 80/20 splits
set.seed(0)

pitch_train<-Mega_Summary %>% sample_frac(0.8)
pitch_test<-anti_join(Mega_Summary, pitch_train)

#Creating model matrices
x_full_model<-model.matrix(`Salary (t+1)`~.,data=data_selected)[,-1]
y_full_model<-data_selected$`Salary (t+1)`

x_train<-model.matrix(`Salary (t+1)`~.,data=pitch_train)[,-1]
y_train<-pitch_train$`Salary (t+1)`

x_test<-model.matrix(`Salary (t+1)`~.,data=pitch_test)[,-1]
y_test<-pitch_test$`Salary (t+1)`

#Selecting the best lambda
my_cv_ridge<-cv.glmnet(x_train, y_train, alpha =0)
best_L_ridge<-my_cv_ridge$lambda.min
best_L_ridge

ridge_mod<- glmnet(x_train, y_train, alpha = 0, lambda = 795626.1)


#Recycling Josh's function for R-squared
r_sq<- function (true_y, fitted,n_obs,k_obs) {
  rss = sum((fitted - true_y)^2)
  tss=sum(((true_y-mean(true_y))^2))
  return (1 - ((rss/(n_obs - k_obs - 1)) / (tss/(n_obs - 1))))
}

#Ridge prediction on test
ridge_predict<-predict(ridge_mod,newx=x_test,s=795626.1)

#Calculating R-squared
set.seed(0)
ridge_r2<-r_sq(pitch_test$`Salary (t+1)`,ridge_predict,113,19)
print(ridge_r2)


#Lasso Lambda
set.seed(0)
my_cv_lasso<-cv.glmnet(x_train, y_train, alpha =1)
best_L_lasso<-my_cv_lasso$lambda.min
best_L_lasso

lasso_mod<- glmnet(x_train, y_train, alpha = 0, lambda = 81471.26)

#Lasso prediction on test
lasso_predict<-predict(lasso_mod,newx=x_test,s=81471.26)

#Calculating R-squared
set.seed(0)
lasso_r2<-r_sq(pitch_test$`Salary (t+1)`,lasso_predict,113,19)
print(lasso_r2)


#ERA Predictions

#New Matrices
#Creating model matrices
x_full_model_ERA<-model.matrix(`ERA (t+1)`~.,data=data_selected)[,-1]
y_full_model_ERA<-data_selected$`ERA (t+1)`

x_train_ERA<-model.matrix(`ERA (t+1)`~.,data=pitch_train)[,-1]
y_train_ERA<-pitch_train$`ERA (t+1)`

x_test_ERA<-model.matrix(`ERA (t+1)`~.,data=pitch_test)[,-1]
y_test_ERA<-pitch_test$`ERA (t+1)`

#Selecting the best lambda for ridge
set.seed(0)
my_cv_ridge_ERA<-cv.glmnet(x_train_ERA, y_train_ERA, alpha =0)
best_L_ridge_ERA<-my_cv_ridge_ERA$lambda.min
best_L_ridge_ERA

ridge_mod_ERA<- glmnet(x_train_ERA, y_train_ERA, alpha = 0, lambda = 0.08326701)

#Ridge pred and R-squared for ERA

#Ridge prediction on test ERA
ridge_predict_ERA<-predict(ridge_mod_ERA,newx=x_test_ERA,s=0.546488)

#Calculating R-squared
set.seed(0)

ridge_r2_ERA<-r_sq(y_test_ERA,ridge_predict_ERA,113,19)
print(ridge_r2_ERA)

full_mod<-lm(`ERA (t+1)`~.,data=pitch_train)
summary(full_mod)
