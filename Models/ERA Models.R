library(tidyverse)
library(leaps)
library(glmnet)
library(ISLR)



# Load Data

pitchers <- read_csv("MegaMegaERA.csv")

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
  select(-Pitcher, -`ΔERA`)

pitchers <- pitchers %>%
  filter(`ERA (t+1)` < 7.00) %>%
  filter(`Luck Adjusted ERA` < 7.00)

write_csv(pitchers, "Luck Mega Summary.csv")

## Single Variable Model

k_mod <- lm(data = pitchers,
               `ERA (t+1)` ~ `K %`)


summary(k_mod)$adj.r.squared



## Full Model

full_mod <- lm(data = pitchers,
                 `ERA (t+1)` ~.)


summary(full_mod)$adj.r.squared



## Custom Model

custom_mod <- lm(data = pitchers,
                  `ERA (t+1)` ~ xwOBA + SO + `K %` + `Barrel %` + ERA:`xwOBA - wOBA` + 
                   ERA:`xBA - BA` + ERA:`BABIP - Mean BABIP` + `ERA/Hard Hit %`)


summary(custom_mod)$adj.r.squared



## Custom Model 2

custom_mod2 <- lm(data = pitchers,
                 `ERA (t+1)` ~ xwOBA + SO + `K %` + ERA:`ERA/Hard Hit %`)


summary(custom_mod2)$adj.r.squared



## Custom Model 3

custom_mod3 <- lm(data = pitchers,
                  `ERA (t+1)` ~ xwOBA + SO + W + `K %` + 
                    ERA:`ERA/Hard Hit %` + ERA:`xwOBA - wOBA`)


summary(custom_mod3)$adj.r.squared



## Custom Model 4

custom_mod4 <- lm(data = pitchers, 
                   `ERA (t+1)` ~ xwOBA + `Spin Rate` + W + G + SO + `K %` + `BB %` +
                    `Hard Hit %` + `Barrel %` + ERA:`ERA/Barrel %`)



summary(custom_mod4)$adj.r.squared



## Custom Model 5

custom_mod5 <- lm(data = pitchers, 
                  `ERA (t+1)` ~ `Spin Rate` + G + SO + `K %` + 
                    ERA:`Hard Hit %` + ERA:`Barrel %` + `Luck Adjusted ERA`)

summary(custom_mod5)$adj.r.squared

plot(custom_mod5)



## Subset Selection Models

# Forward Selection

forward_select <- regsubsets(data = pitchers,
                             `ERA (t+1)` ~., 
                             nvmax = 9, 
                             method = "forward")
summary(forward_select)

forward_mod <- lm(data = pitchers, 
                   `ERA (t+1)` ~ `Spin Rate` + BFP +  SO + `K %` + 
                    `Hard Hit %` + `Barrel %` + `BFP/G` + `Luck Adjusted ERA`)

summary(forward_mod)$adj.r.squared


# Best Subset

best_subset <- regsubsets(data = pitchers,
                             `ERA (t+1)` ~.,
                             nvmax = 8)
summary(best_subset)

best_mod <- lm(data = pitchers, 
                  `ERA (t+1)` ~ `Spin Rate` + ABs + H + BFP + `BB %` + `K %` + `Hard Hit %` + `Barrel %` + `ERA/Barrel %` + `ERA/Hard Hit %`)

summary(best_mod)$adj.r.squared



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
                             `ERA (t+1)` ~., 
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

