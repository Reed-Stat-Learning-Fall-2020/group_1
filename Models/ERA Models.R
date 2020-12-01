library(tidyverse)
library(leaps)
library(glmnet)
library(ISLR)

# Load Data

pitchers <- read_csv("Mega Summary.csv") %>%
  select(-Salary, -`Salary (t+1)`, -`ΔSalary`, -`ΔERA`)


## Custom Model

custom_mod <- lm(data = pitchers,
                  `ERA (t+1)` ~ xwOBA + SO + `K %` + `Barrel %` + ERA:`xwOBA - wOBA` + ERA:`xBA - BA` + ERA:`BABIP - Mean BABIP` + `ERA/Hard Hit %`)


summary(custom_mod)$adj.r.squared



## Subset Selection Models

# Forward Selection

forward_select <- regsubsets(data = pitchers,
                             `ERA (t+1)` ~.-Pitcher, 
                             nvmax = 10, 
                             method = "forward")
summary(forward_select)

forward_mod <- lm(data = pitchers, 
                   `ERA (t+1)` ~ xwOBA + `Spin Rate` + W + G + BFP + SO + `K %` + `Hard Hit %` + `Barrel %` + `ERA/Barrel %` + ERA)

summary(forward_mod)$adj.r.squared

#Forward Select but for salary

forward_select_salary <- regsubsets(data = Mega_Summary_Salary,
                             `Salary (t+1)` ~., 
                             nvmax = 10, 
                             method = "forward")

summary(forward_select_salary)

forward_mod_salary <- lm(data = Mega_Summary_Salary, 
                  `Salary (t+1)` ~ wOBA + Hits + ABs+ `Spin Rate` +Velocity+H+ SO+`BB %`+`Hard Hit %`+Salary+`xwOBA - wOBA`)

summary(forward_mod_salary)$adj.r.squared

#Backward Selection

backward_select <- regsubsets(data = pitchers,
                             `ERA (t+1)` ~.-Pitcher, 
                             nvmax = 10, 
                             method = "backward")
summary(backward_select)

backward_select <- lm(data = pitchers, `ERA (t+1)` ~ xwOBA + xBA+ `Spin Rate` +W+G+ERA+H+SO+`BB %`+`Hard Hit %`+`ERA/Barrel %`)

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

