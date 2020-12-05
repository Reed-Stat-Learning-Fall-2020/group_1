library(tidyverse)
library(leaps)
library(glmnet)
library(ISLR)
library(randomForest)


# Load Data

ERA_pitchers <- read_csv("Mega Summary.csv")

ERA_pitchers <- ERA_pitchers %>%
  mutate(`Standardized Barrel Luck` = -scale(`ERA/Barrel %`)) %>%
  mutate(`Standardized Hard Hit Luck` = -scale(`ERA/Hard Hit %`)) %>%
  mutate(`Standardized Luck` = (`Standardized Barrel Luck` + `Standardized Hard Hit Luck`)/2) %>%
  mutate(`Luck Adjusted ERA` = ERA + (1/3)*`Standardized Luck`)

ERA_pitchers <- ERA_pitchers %>%
  select(-`BABIP - Mean BABIP`, -`xBA - BA`, -`ERA/Barrel %`, -`ERA/Hard Hit %`,-`Standardized Barrel Luck`, 
         -`Standardized Hard Hit Luck`, -`Standardized Luck`)

ERA_pitchers <- ERA_pitchers %>%
  filter(`ERA (t+1)` < 7.00) %>%
  filter(`Luck Adjusted ERA` < 7.00)

Salary_pitchers <- ERA_pitchers %>%
  select(-Pitcher, -`ΔERA`, -`ERA (t+1)`, -`ΔSalary`, -Salary)

ERA_pitchers <- ERA_pitchers %>%
  select(-Pitcher, -`ΔERA`, -Salary, -`Salary (t+1)`, -`ΔSalary`)

Salary_pitchers <- Salary_pitchers %>%
  rename(spin_rate = `Spin Rate`,
         K_percent = `K %`,
         BB_percent = `BB %`,
         hard_hit_percent = `Hard Hit %`,
         barrel_percent = `Barrel %`,
         salary_t1 = `Salary (t+1)`,
         BFP_per_G = `BFP/G`,
         xwOBA_minus_wOBA = `xwOBA - wOBA`,
         luck_adj_ERA = `Luck Adjusted ERA`)

Salary_pitchers <- Salary_pitchers %>%
  select(-Salary)

write_csv(Salary_pitchers, "Legally Named Salary.csv")

ERA_pitchers <- ERA_pitchers %>%
  rename(spin_rate = `Spin Rate`,
         K_percent = `K %`,
         BB_percent = `BB %`,
         hard_hit_percent = `Hard Hit %`,
         barrel_percent = `Barrel %`,
         ERA_t1 = `ERA (t+1)`,
         BFP_per_G = `BFP/G`,
         xwOBA_minus_wOBA = `xwOBA - wOBA`,
         luck_adj_ERA = `Luck Adjusted ERA`)


write_csv(ERA_pitchers, "Legally Named ERA.csv")



# Salary Forest Model

set.seed(11)
salary_forest <- randomForest(data = Salary_pitchers, salary_t1 ~., 
                              ntrees = 11)

salary_forest

importance(salary_forest)

varImpPlot(salary_forest)



# ERA Forest Model

set.seed(11)
ERA_forest <- randomForest(data = ERA_pitchers, `ERA_t1`~., 
                              ntrees = 11)

ERA_forest

importance(ERA_forest)

varImpPlot(ERA_forest)


