library(tidyverse)

#Load data and select variables (same variables Josh picked)
Mega_Summary <- read_csv("Models/Mega Summary.csv")


data_selected <- Mega_Summary %>%
  select(Pitches, BA, Hits,`Spin Rate`, Velocity,
         W, L, ERA, HR, BB, SO, xwOBA, `K %`,
         `Barrel %`, Salary, `K %`:`Barrel %`, `ERA (t+1)`,
         `Salary (t+1)`, Pitcher, Year) %>% select(-Pitcher)

#Split into test and train with 80/20 splits
set.seed(0)

pitch_train<-data_selected %>% sample_frac(0.8)
pitch_test<-anti_join(data_selected, pitch_train)

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

ridge_mod<- glmnet(x_train, y_train, alpha = 0, lambda = 777682.7)


#Recycling Josh's function for R-squared
r_sq<- function (true_y, fitted,n_obs,k_obs) {
  rss = sum((fitted - true_y)^2)
  tss=sum(((true_y-mean(true_y))^2))
  return (1 - ((rss/(n_obs - k_obs - 1)) / (tss/(n_obs - 1))))
}

#Ridge prediction on test
ridge_predict<-predict(ridge_mod,newx=x_test,s=777682.7)

#Calculating R-squared
set.seed(0)
ridge_r2<-r_sq(pitch_test$`Salary (t+1)`,ridge_predict,113,21)
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
lasso_r2<-r_sq(pitch_test$`Salary (t+1)`,lasso_predict,113,21)
print(lasso_r2)

