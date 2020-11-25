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

