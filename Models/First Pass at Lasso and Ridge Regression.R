library(tidyverse)
library(glmnet)

#Load data and select variables (same variables Josh picked)
Mega_Summary <- read_csv("data/Final Mega Summary.csv")


data_selected <- Mega_Summary %>%
  select(Pitches, BA, Hits,`Spin Rate`, Velocity,
         W, L, ERA, HR, BB, SO, xwOBA, `K %`,
         `Barrel %`, Salary, `K %`:`Barrel %`, ERA_t1,
         salary_t1, Pitcher, Year) %>% select(-Pitcher)

Mega_Summary_ERA<-Mega_Summary%>% select(-Pitcher,-ΔSalary,-Year,-ΔERA,-salary_t1)
Mega_Summary_Salary<-Mega_Summary%>% select(-Pitcher,-ΔSalary,-Year,-ΔERA,-ERA_t1)

#Filtering out Pre-arb in salary
Mega_Summary_Salary<-Mega_Summary_Salary %>% filter(salary_t1>600000)

#Split into test and train with 80/20 splits
set.seed(0)

pitch_train_salary<-Mega_Summary_Salary %>% sample_frac(0.8)
pitch_test_salary<-anti_join(Mega_Summary_Salary, pitch_train)

pitch_train_ERA<-Mega_Summary_ERA %>% sample_frac(0.8)
pitch_test_ERA<-anti_join(Mega_Summary_ERA, pitch_train)

#Creating model matrices for salary
x_full_model_salary<-model.matrix(log(salary_t1)~.,data=Mega_Summary_Salary)[,-1]
y_full_model_salary<-log(Mega_Summary_Salary$salary_t1)

x_train_salary<-model.matrix(log(salary_t1)~.,data=pitch_train_salary)[,-1]
y_train_salary<-log(pitch_train_salary$salary_t1)

x_test_salary<-model.matrix(log(salary_t1)~.,data=pitch_train_salary)[,-1]
y_test_salary<-log(pitch_test_salary$salary_t1)

#Creating model matrices for ERA
x_full_model_ERA<-model.matrix(ERA_t1~.,data=Mega_Summary_ERA)[,-1]
y_full_model_ERA<-Mega_Summary_ERA$ERA_t1

x_train_ERA<-model.matrix(ERA_t1~.,data=pitch_train_ERA)[,-1]
y_train_ERA<-pitch_train_ERA$ERA_t1

x_test_ERA<-model.matrix(ERA_t1~.,data=pitch_train_ERA)[,-1]
y_test_ERA<-pitch_test_ERA$ERA_t1

#Selecting the best lambda
my_cv_ridge<-cv.glmnet(x_train_salary, y_train_salary, alpha =0)
best_L_ridge<-my_cv_ridge$lambda.min
best_L_ridge

ridge_mod<- glmnet(x_full_model_salary, y_full_model_salary, alpha = 0, lambda = best_L_ridge)


#Recycling Josh's function for R-squared
r_sq<- function (true_y, fitted,n_obs,k_obs) {
  rss = sum((fitted - true_y)^2)
  tss=sum(((true_y-mean(true_y))^2))
  return (1 - ((rss/(n_obs - k_obs - 1)) / (tss/(n_obs - 1))))
}

#Ridge prediction on test
ridge_predict<-predict(ridge_mod,newx=x_full_model_salary,s=best_L_ridge)

#Calculating R-squared
set.seed(0)
ridge_r2<-r_sq(y_full_model_salary,ridge_predict,457,30)
print(ridge_r2)


#5 Fold CV for ridge salary 
ridge_mod_salary_CV<- cv.glmnet(x_full_model_salary, y_full_model_salary, alpha = 0,nfolds=5)
ridge_MSE_salary<-data.frame(ridge_mod_salary_CV$cvm)
ridge_MSE_salary %>%
  summarise(MSE = min(ridge_mod_salary_CV$cvm))


#Lasso Lambda
set.seed(0)
my_cv_lasso<-cv.glmnet(x_train_salary, y_train_salary, alpha =1)
best_L_lasso<-my_cv_lasso$lambda.min
best_L_lasso

lasso_mod<- glmnet(x_full_model_salary, y_full_model_salary, alpha = 0, lambda = best_L_lasso)

#Lasso prediction on test
lasso_predict<-predict(lasso_mod,newx=x_full_model_salary,s=best_L_lasso)

#Calculating R-squared
set.seed(0)
lasso_r2<-r_sq(y_full_model_salary,lasso_predict,457,30)
print(lasso_r2)

#5 Fold CV for lasso salary 
lasso_mod_salary_CV<- cv.glmnet(x_full_model_salary, y_full_model_salary, alpha = 1,nfolds=5)
lasso_MSE_salary<-data.frame(lasso_mod_salary_CV$cvm)
lasso_MSE_salary %>%
  summarise(MSE = min(lasso_mod_salary_CV$cvm))



#ERA Predictions

#New Matrices
#Creating model matrices
x_full_model_ERA<-model.matrix(ERA_t1~.,data=Mega_Summary_ERA)[,-1]
y_full_model_ERA<-Mega_Summary_ERA$ERA_t1

x_train_ERA<-model.matrix(ERA_t1~.,data=pitch_train_ERA)[,-1]
y_train_ERA<-pitch_train_ERA$ERA_t1

x_test_ERA<-model.matrix(ERA_t1~.,data=pitch_test_ERA)[,-1]
y_test_ERA<-pitch_test_ERA$ERA_t1

#Selecting the best lambda for ridge
set.seed(0)
my_cv_ridge_ERA<-cv.glmnet(x_train_ERA, y_train_ERA, alpha =0)
best_L_ridge_ERA<-my_cv_ridge_ERA$lambda.min
best_L_ridge_ERA

ridge_mod_ERA<- glmnet(x_full_model_ERA, y_full_model_ERA, alpha = 0, lambda = best_L_ridge_ERA)

#Ridge pred and R-squared for ERA

#Ridge prediction on test ERA
ridge_predict_ERA<-predict(ridge_mod_ERA,newx=x_full_model_ERA,s=best_L_ridge_ERA)

#Calculating R-squared_ridge
set.seed(0)

ridge_r2_ERA<-r_sq(y_full_model_ERA,ridge_predict_ERA,457,29)
print(ridge_r2_ERA)


#5 fold CV for ridge on ERA
ridge_mod_ERA_CV<- cv.glmnet(x_full_model_ERA, y_full_model_ERA, alpha = 0,nfolds=5)
ridge_MSE_ERA<-data.frame(ridge_mod_ERA_CV$cvm)
ridge_MSE_ERA %>%
  summarise(MSE = min(ridge_mod_ERA_CV$cvm))



#Selecting the best lambda for Lasso
set.seed(0)
my_cv_lasso_ERA<-cv.glmnet(x_train_ERA, y_train_ERA, alpha =1)
best_L_lasso_ERA<-my_cv_lasso_ERA$lambda.min
best_L_lasso_ERA

lasso_mod_ERA<- glmnet(x_full_model_ERA, y_full_model_ERA, alpha = 1, lambda = best_L_lasso_ERA)

#Lasso pred and R-squared for ERA

#Lasso prediction on test ERA
lasso_predict_ERA<-predict(lasso_mod_ERA,newx=x_full_model_ERA,s=best_L_lasso_ERA)

#Calculating R-squared
set.seed(0)

lasso_r2_ERA<-r_sq(y_full_model_ERA,lasso_predict_ERA,565,29)
print(lasso_r2_ERA)

#5 Fold CV for lasso ERA 
lasso_mod_ERA_CV<- cv.glmnet(x_full_model_ERA, y_full_model_ERA, alpha = 1,nfolds=5)
lasso_MSE_ERA<-data.frame(lasso_mod_ERA_CV$cvm)
lasso_MSE_ERA %>%
  summarise(MSE = min(lasso_mod_ERA_CV$cvm))

#Below is code for the various MSE and R-squared models, you don't need to pay attention to it

ridge_statistics<-c(ridge_r2,min(ridge_MSE_salary),ridge_r2_ERA,min(ridge_MSE_ERA))
lasso_statistics<-c(lasso_r2,min(lasso_MSE_salary),lasso_r2_ERA,min(lasso_MSE_ERA))
forward_statistics<-c(0.6169164,0.7657645,0.1642511,0.9994591)
domain_knowledge_statistics<-c("NA","NA",0.1573634,0.972449)
full_statistics<-c(0.6092759,0.8313169,0.1397712,1.04342)
PCR_statistics<-c(0.65489,0.3313,0.13856,0.9972)
labels<-c("R-Squared on Salary", "5-fold MSE on Salary","R-Squared on ERA","5-Fold MSE on ERA")

all_statistics<-data.frame(labels,ridge_statistics,lasso_statistics,forward_statistics,domain_knowledge_statistics,full_statistics,PCR_statistics)
view(all_statistics)
