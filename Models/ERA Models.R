library(tidyverse)
library(leaps)
library(glmnet)
library(ISLR)
library(boot)


# Load Data

pitchers <- read_csv("data/Final Mega Summary.csv")

salary_data<-pitchers %>%
  select(-Pitcher,-ΔERA,-ERA_t1,-ΔSalary)

  

#Is the below still necessary?
pitchers <- pitchers %>%
  mutate(`Standardized Barrel Luck` = -scale(`ERA/Barrel %`)) %>%
  mutate(`Standardized Hard Hit Luck` = -scale(`ERA/Hard Hit %`)) %>%
  mutate(`Standardized Luck` = (`Standardized Barrel Luck` + `Standardized Hard Hit Luck`)/2) %>%
  mutate(`Luck Adjusted ERA` = ERA + (1/3)*`Standardized Luck`)

pitchers <- pitchers %>%
  select(-`BABIP - Mean BABIP`, -`xBA - BA`, -`ERA/Barrel %`, -`ERA/Hard Hit %`,-`Standardized Barrel Luck`, 
         -`Standardized Hard Hit Luck`, -`Standardized Luck`)

write_csv(pitchers, "Luck Mega Summary.csv")

pitchers <- pitchers %>%
  select(-Pitcher,-ΔERA,-salary_t1,-ΔSalary)

pitchers <- pitchers %>%
  filter(ERA_t1 < 7.00) %>%
  filter(luck_adj_ERA < 7.00)


write_csv(pitchers, "Luck Mega Summary.csv")

## Single Variable Model

k_mod <- lm(data = pitchers,
               ERA_t1 ~ K_percent)


summary(k_mod)$adj.r.squared



## Full Model for ERA

full_mod <- lm(data = pitchers,
                 ERA_t1 ~.)

#Full model adjusted R-squared

summary(full_mod)$adj.r.squared

#Full model 5 fold MSE

full_mod_glm <- glm(data = pitchers,
               ERA_t1 ~.)

full_MSE_ERA_CV<- cv.glm(pitchers, full_mod_glm,K=5)

full_MSE_ERA<-full_MSE_ERA_CV$delta[1]


#Full model for salary

full_mod_salary<-lm(data=salary_data,
                    log(salary_t1)~.)

#Full model salary adjusted R-squared

summary(full_mod_salary)$adj.r.squared

#Full model 5 fold MSE

full_mod_salary_glm <- glm(data = salary_data,
                          log(salary_t1)~.)

full_MSE_salary_CV<- cv.glm(salary_data, full_mod_salary_glm,K=5)

full_MSE_salary<-full_MSE_salary_CV$delta[1]

## Custom Model

custom_mod <- lm(data = pitchers,
                  ERA_t1 ~ xwOBA + SO + `K %` + `Barrel %` + ERA:`xwOBA - wOBA` + 
                   ERA:`xBA - BA` + ERA:`BABIP - Mean BABIP` + `ERA/Hard Hit %`)


summary(custom_mod)$adj.r.squared



## Custom Model 2

custom_mod2 <- lm(data = pitchers,
                 ERA_t1 ~ xwOBA + SO + `K %` + ERA:`ERA/Hard Hit %`)


summary(custom_mod2)$adj.r.squared



## Custom Model 3

custom_mod3 <- lm(data = pitchers,
                  ERA_t1 ~ xwOBA + SO + W + `K %` + 
                    ERA:`ERA/Hard Hit %` + ERA:`xwOBA - wOBA`)


summary(custom_mod3)$adj.r.squared



## Custom Model 4

custom_mod4 <- lm(data = pitchers, 
                   ERA_t1 ~ xwOBA + `Spin Rate` + W + G + SO + `K %` + `BB %` +
                    `Hard Hit %` + `Barrel %` + ERA:`ERA/Barrel %`)



summary(custom_mod4)$adj.r.squared



## Custom Model 5

custom_mod5 <- lm(data = pitchers, 
                  ERA_t1 ~ spin_rate + G + SO + K_percent + 
                    ERA:hard_hit_percent + ERA:barrel_percent + luck_adj_ERA)

summary(custom_mod5)$adj.r.squared


#Custom model 5 5-fold CV MSE

custom_mod_salary_glm <- glm(data = pitchers, 
                             ERA_t1 ~ spin_rate + G + SO + K_percent + 
                               ERA:hard_hit_percent + ERA:barrel_percent + luck_adj_ERA)

custom_mod_MSE_ERA_CV<- cv.glm(pitchers, custom_mod_salary_glm,K=5)

custom_mod_MSE_ERA<-custom_mod_MSE_ERA_CV$delta[1]

#Custom Model Salary

custom_mod_salary <- lm(data = salary_data,
                         log(salary_t1)~Year+SO+
                           Salary+W*luck_adj_ERA)

summary(custom_mod_salary)

#Custom salary model 5 5-fold CV MSE

custom_mod_salary_glm <- glm(data = salary_data,
                             log(salary_t1)~Year+SO+
                               Salary+W*luck_adj_ERA)

custom_mod_MSE_salary_CV<- cv.glm(salary_data, custom_mod_salary_glm,K=5)

custom_mod_MSE_salary<-custom_mod_MSE_salary_CV$delta[1]

## Subset Selection Models

# Forward Selection

forward_select <- regsubsets(data = pitchers,
                             ERA_t1 ~., 
                             nvmax = 11, 
                             method = "forward")
summary(forward_select)

#How many predictors should we include

adj_r_sq_forward<-summary(forward_select)$adjr2

rss_forward<-summary(forward_select)$rss

cp_forward<-summary(forward_select)$cp

forward_select_all<-data.frame(model = 1:12, adj_r_sq_forward, rss_forward, cp_forward )

view(forward_select_all) #In terms of adjusted R-squared, model 9 did the best

forward_mod <- lm(data = pitchers, 
                   ERA_t1 ~ wOBA+L+BFP+SO+K_percent+hard_hit_percent+
                    barrel_percent+`ERA/Barrel %`+luck_adj_ERA)

#Adjusted R-squared for forward
summary(forward_mod)$adj.r.squared

#Forward model 5 5-fold CV MSE

forward_ERA_glm <- glm(data = pitchers, 
                          ERA_t1 ~ wOBA+L+BFP+SO+K_percent+hard_hit_percent+
                            barrel_percent+`ERA/Barrel %`+luck_adj_ERA)

forward_MSE_ERA_CV<- cv.glm(pitchers, forward_ERA_glm,K=5)

forward_MSE_ERA<-forward_MSE_ERA_CV$delta[1]


# Best Subset

best_subset <- regsubsets(data = pitchers,
                             ERA_t1 ~.,
                             nvmax = 8)
summary(best_subset)

best_mod <- lm(data = pitchers, 
                  ERA_t1 ~ `Spin Rate` + ABs + H + BFP + `BB %` + `K %` + `Hard Hit %` + `Barrel %` + `ERA/Barrel %` + `ERA/Hard Hit %`)

summary(best_mod)$adj.r.squared



#Forward Select but for salary

forward_select_salary <- regsubsets(data = salary_data,
                             log(salary_t1) ~., 
                             nvmax = 11, 
                             method = "forward")

#How many predictors should we include for salary

adj_r_sq_forward_salary<-summary(forward_select_salary)$adjr2

rss_forward_salary<-summary(forward_select_salary)$rss

cp_forward_salary<-summary(forward_select_salary)$cp

forward_select_all_salary<-data.frame(model = 1:12, adj_r_sq_forward_salary, rss_forward_salary, cp_forward_salary )

view(forward_select_all_salary) #It seems like model 8 is the best

forward_mod_salary <- lm(data = salary_data,
                         log(salary_t1)~Pitches+Year+xBA+spin_rate+
                           Velocity+K_percent+Salary+luck_adj_ERA)

summary(forward_mod_salary)$adj.r.squared

#Forward model 5 5-fold CV MSE for salary

forward_salary_glm <- glm(data = salary_data,
                          log(salary_t1)~Pitches+Year+xBA+spin_rate+
                            Velocity+K_percent+Salary+luck_adj_ERA)

forward_MSE_salary_CV<- cv.glm(salary_data, forward_salary_glm,K=5)

forward_MSE_salary<-forward_MSE_salary_CV$delta[1]


#Backward Selection

backward_select <- regsubsets(data = pitchers,
                             ERA_t1 ~., 
                             nvmax = 10, 
                             method = "backward")
summary(backward_select)

backward_select <- lm(data = pitchers, ERA_t1 ~ xwOBA + xBA+ `Spin Rate` +W+G+ERA+H+SO+`BB %`+`Hard Hit %`+`ERA/Barrel %`)

summary(backward_select)$adj.r.squared



#Backward Select but for salary

backward_select_salary <- regsubsets(data = Mega_Summary_Salary,
                                    `Salary (t+1)` ~., 
                                    nvmax = 10, 
                                    method = "backward")

summary(backward_select_salary)

backward_mod_salary <- lm(data = Mega_Summary_Salary, 
                         `Salary (t+1)` ~ Hits + ABs + Velocity + BFP + BB + SO + `BB %` + `Hard Hit %` + `Barrel %` + Salary +  `ERA/Barrel %`)
                           
summary(backward_mod_salary)$adj.r.squared

