library(tidyverse)
library(rvest)
library(baseballr)
library(Lahman)


# 2015 Data

pitchers_2015 <- read_csv("Pitchers 2015.csv")
pitchers_2015 <- pitchers_2015 %>% 
  select(pitches, player_name, year, ba, babip, woba, xwoba, xba, hits, abs, spin_rate, velocity)


# 2016 Data

pitchers_2016 <- read_csv("Pitchers 2016.csv")
pitchers_2016 <- pitchers_2016 %>% 
  select(pitches, player_name, year, ba, babip, woba, xwoba, xba, hits, abs, spin_rate, velocity)


# 2017 Data

pitchers_2017 <- read_csv("Pitchers 2017.csv")
pitchers_2017 <- pitchers_2017 %>% 
  select(pitches, player_name, year, ba, babip, woba, xwoba, xba, hits, abs, spin_rate, velocity)


# 2018 Data

pitchers_2018 <- read_csv("Pitchers 2018.csv")
pitchers_2018 <- pitchers_2018 %>% 
  select(pitches, player_name, year, ba, babip, woba, xwoba, xba, hits, abs, spin_rate, velocity)


# 2019 Data

pitchers_2019 <- read_csv("Pitchers 2019.csv")
pitchers_2019 <- pitchers_2019 %>% 
  select(pitches, player_name, year, ba, babip, woba, xwoba, xba, hits, abs, spin_rate, velocity)


# Names

names <- People %>%
  select(playerID, nameFirst, nameLast) %>%
  unite("full_name", 2:3, sep = " ") %>%
  rename(player_name = full_name)


# 2015 Lahman

lahman_2015 <- Pitching %>%
  filter(yearID == 2015) %>%
  rename(year = yearID) %>%
  select(playerID, W, L, G, BFP, ERA, H, HR, BB, SO) %>%
  mutate(`K %` = SO/BFP,
         `BB %` = BB/BFP) %>%
  mutate(`K %` = format(round(`K %`, 3), nsmall = 3)) %>%
  mutate(`BB %` = format(round(`BB %`, 3), nsmall = 3))

lahman_2015 <- left_join(lahman_2015, names, by = "playerID") %>%
  select(-playerID) %>%
  distinct(player_name, .keep_all = TRUE)


# 2016 Lahman

lahman_2016 <- Pitching %>%
  filter(yearID == 2016) %>%
  rename(year = yearID) %>%
  select(playerID, W, L, G, BFP, ERA, H, HR, BB, SO) %>%
  mutate(`K %` = SO/BFP,
         `BB %` = BB/BFP) %>%
  mutate(`K %` = format(round(`K %`, 3), nsmall = 3)) %>%
  mutate(`BB %` = format(round(`BB %`, 3), nsmall = 3))

lahman_2016 <- left_join(lahman_2016, names, by = "playerID") %>%
  select(-playerID) %>%
  distinct(player_name, .keep_all = TRUE)


# 2017 Lahman

lahman_2017 <- Pitching %>%
  filter(yearID == 2017) %>%
  rename(year = yearID) %>%
  select(playerID, W, L, G, BFP, ERA, H, HR, BB, SO) %>%
  mutate(`K %` = SO/BFP,
         `BB %` = BB/BFP) %>%
  mutate(`K %` = format(round(`K %`, 3), nsmall = 3)) %>%
  mutate(`BB %` = format(round(`BB %`, 3), nsmall = 3))

lahman_2017 <- left_join(lahman_2017, names, by = "playerID") %>%
  select(-playerID) %>%
  distinct(player_name, .keep_all = TRUE)


# 2018 Lahman

lahman_2018 <- Pitching %>%
  filter(yearID == 2018) %>%
  rename(year = yearID) %>%
  select(playerID, W, L, G, BFP, ERA, H, HR, BB, SO) %>%
  mutate(`K %` = SO/BFP,
         `BB %` = BB/BFP) %>%
  mutate(`K %` = format(round(`K %`, 3), nsmall = 3)) %>%
  mutate(`BB %` = format(round(`BB %`, 3), nsmall = 3))

lahman_2018 <- left_join(lahman_2018, names, by = "playerID") %>%
  select(-playerID) %>%
  distinct(player_name, .keep_all = TRUE)


# 2019 Lahman

lahman_2019 <- Pitching %>%
  filter(yearID == 2019) %>%
  rename(year = yearID) %>%
  select(playerID, W, L, G, BFP, ERA, H, HR, BB, SO) %>%
  mutate(`K %` = SO/BFP,
         `BB %` = BB/BFP) %>%
  mutate(`K %` = format(round(`K %`, 3), nsmall = 3)) %>%
  mutate(`BB %` = format(round(`BB %`, 3), nsmall = 3))

lahman_2019 <- left_join(lahman_2019, names, by = "playerID") %>%
  select(-playerID) %>%
  distinct(player_name, .keep_all = TRUE)


# 2016 ERA

ERA_2016 <- lahman_2016 %>%
  filter(BFP >= 150) %>% 
  select(player_name, ERA) %>%
  rename(`ERA (t+1)` = ERA)


# 2017 ERA

ERA_2017 <- lahman_2017 %>%
  filter(BFP >= 150) %>% 
  select(player_name, ERA) %>%
  rename(`ERA (t+1)` = ERA)


# 2018 ERA

ERA_2018 <- lahman_2018 %>%
  filter(BFP >= 150) %>% 
  select(player_name, ERA) %>%
  rename(`ERA (t+1)` = ERA)


# 2019 ERA

ERA_2019 <- lahman_2019 %>%
  filter(BFP >= 150) %>% 
  select(player_name, ERA) %>%
  rename(`ERA (t+1)` = ERA)


# 2020 ERA

ERA_2020 <- read_csv("ERA 2020.csv")
ERA_2020 <- ERA_2020 %>%
  filter(pa >= 150) %>% 
  select(last_name, first_name, era) %>%
  unite("player_name", 2:1, sep = " ") %>%
  rename(`ERA (t+1)` = era)


# 2015 Salary

salary_2015 <- read_csv("2015salary.csv") %>%
  rename(player_name = mlb_name) %>%
  distinct(player_name, .keep_all = TRUE)
salary_2015two <- read_csv("2016salary.csv") %>%
  distinct(player_name, .keep_all = TRUE)
salary_2015 <- salary_2015 %>%
  select(player_name, salary) %>%
  rename(Salary = salary)
salary_2015two <- salary_2015two %>%
  select(player_name, salary) %>%
  rename(`Salary (t+1)` = salary)


# 2016 Salary

salary_2016 <- read_csv("2016salary.csv") %>%
  distinct(player_name, .keep_all = TRUE)
salary_2016two <- read_csv("2017salary.csv") %>%
  rename(player_name = name) %>%
  distinct(player_name, .keep_all = TRUE)
salary_2016 <- salary_2016 %>%
  select(player_name, salary) %>%
  rename(Salary = salary)
salary_2016two <- salary_2016two %>%
  select(player_name, salary) %>%
  rename(`Salary (t+1)` = salary)


# 2017 Salary

salary_2017 <- read_csv("2017salary.csv") %>%
  rename(player_name = name) %>%
  distinct(player_name, .keep_all = TRUE)
salary_2017two <- read_csv("2018salary.csv") %>%
  rename(player_name = name) %>%
  distinct(player_name, .keep_all = TRUE)
salary_2017 <- salary_2017 %>%
  select(player_name, salary) %>%
  rename(Salary = salary)
salary_2017two <- salary_2017two %>%
  select(player_name, salary) %>%
  rename(`Salary (t+1)` = salary)


# 2018 Salary

salary_2018 <- read_csv("2018salary.csv") %>%
  rename(player_name = name) %>%
  distinct(player_name, .keep_all = TRUE)
salary_2018two <- read_csv("2019salary.csv") %>%
  rename(player_name = name) %>%
  distinct(player_name, .keep_all = TRUE)
salary_2018 <- salary_2018 %>%
  select(player_name, salary) %>%
  rename(Salary = salary)
salary_2018two <- salary_2018two %>%
  select(player_name, salary) %>%
  rename(`Salary (t+1)` = salary)


# 2019 Salary

salary_2019 <- read_csv("2019salary.csv") %>%
  rename(player_name = name) %>%
  distinct(player_name, .keep_all = TRUE)
salary_2019two <- read_csv("2020salary.csv") %>%
  rename(player_name = name) %>%
  distinct(player_name, .keep_all = TRUE)
salary_2019 <- salary_2019 %>%
  select(player_name, salary) %>%
  rename(Salary = salary)
salary_2019two <- salary_2019two %>%
  select(player_name, salary) %>%
  rename(`Salary (t+1)` = salary)


# 2015 Statcast

statcast_2015 <- read_csv("2015statcast.csv") %>%
  unite("player_name", 2:1, sep = " ") %>%
  select(player_name, ev95percent, brl_percent) %>%
  rename(`Hard Hit %` = ev95percent,
         `Barrel %` = brl_percent)


# 2016 Statcast

statcast_2016 <- read_csv("2016statcast.csv") %>%
  unite("player_name", 2:1, sep = " ") %>%
  select(player_name, ev95percent, brl_percent) %>%
  rename(`Hard Hit %` = ev95percent,
         `Barrel %` = brl_percent)


# 2017 Statcast

statcast_2017 <- read_csv("2017statcast.csv") %>%
  unite("player_name", 2:1, sep = " ") %>%
  select(player_name, ev95percent, brl_percent) %>%
  rename(`Hard Hit %` = ev95percent,
         `Barrel %` = brl_percent)


# 2018 Statcast

statcast_2018 <- read_csv("2018statcast.csv") %>%
  unite("player_name", 2:1, sep = " ") %>%
  select(player_name, ev95percent, brl_percent) %>%
  rename(`Hard Hit %` = ev95percent,
         `Barrel %` = brl_percent)


# 2019 Statcast

statcast_2019 <- read_csv("2019statcast.csv") %>%
  unite("player_name", 2:1, sep = " ") %>%
  select(player_name, ev95percent, brl_percent) %>%
  rename(`Hard Hit %` = ev95percent,
         `Barrel %` = brl_percent)


# 2015 Summary

summary_2015 <- left_join(pitchers_2015, lahman_2015, by = "player_name")
summary_2015 <- left_join(summary_2015, statcast_2015, by = "player_name")
summary_2015 <- left_join(summary_2015, ERA_2016, by = "player_name")
summary_2015 <- left_join(summary_2015, salary_2015, by = "player_name")
summary_2015 <- left_join(summary_2015, salary_2015two, by = "player_name")


# 2016 Summary

summary_2016 <- left_join(pitchers_2016, lahman_2016, by = "player_name")
summary_2016 <- left_join(summary_2016, statcast_2016, by = "player_name")
summary_2016 <- left_join(summary_2016, ERA_2017, by = "player_name")
summary_2016 <- left_join(summary_2016, salary_2016, by = "player_name")
summary_2016 <- left_join(summary_2016, salary_2016two, by = "player_name")


# 2017 Summary

summary_2017 <- left_join(pitchers_2017, lahman_2017, by = "player_name")
summary_2017 <- left_join(summary_2017, statcast_2017, by = "player_name")
summary_2017 <- left_join(summary_2017, ERA_2018, by = "player_name")
summary_2017 <- left_join(summary_2017, salary_2017, by = "player_name")
summary_2017 <- left_join(summary_2017, salary_2017two, by = "player_name")


# 2018 Summary

summary_2018 <- left_join(pitchers_2018, lahman_2018, by = "player_name")
summary_2018 <- left_join(summary_2018, statcast_2018, by = "player_name")
summary_2018 <- left_join(summary_2018, ERA_2019, by = "player_name")
summary_2018 <- left_join(summary_2018, salary_2018, by = "player_name")
summary_2018 <- left_join(summary_2018, salary_2018two, by = "player_name")


# 2019 Summary

summary_2019 <- left_join(pitchers_2019, lahman_2019, by = "player_name")
summary_2019 <- left_join(summary_2019, statcast_2019, by = "player_name")
summary_2019 <- left_join(summary_2019, ERA_2020, by = "player_name")
summary_2019 <- left_join(summary_2019, salary_2019, by = "player_name")
summary_2019 <- left_join(summary_2019, salary_2019two, by = "player_name")


# Mega Summary

mega_summary <- rbind(summary_2015,
                      summary_2016,
                      summary_2017,
                      summary_2018,
                      summary_2019)

mega_summary_uncut <- rbind(summary_2015,
                      summary_2016,
                      summary_2017,
                      summary_2018,
                      summary_2019)


# Filter

mega_summary <- mega_summary %>%
  filter(BFP >= 150) 

mega_summary <- mega_summary %>%
  drop_na()

mega_summary <- mega_summary %>%
  filter(G <= 35)

mega_summary <- mega_summary %>%
  mutate(`BFP/G` = BFP/G)

mega_summary %>%
  ggplot(aes(x = BFP/G)) +
  geom_histogram(bins = 50) +
  theme_minimal()

mega_summary <- mega_summary %>%
  filter(`BFP/G` > 16)


# Variable Mutations

mega_summary <- mega_summary %>%
  mutate(ba = as.numeric(ba),
         babip = as.numeric(babip),
         woba = as.numeric(woba),
         xwoba = as.numeric(xwoba),
         xba = as.numeric(xba),
         spin_rate = as.numeric(spin_rate),
         velocity = as.numeric(velocity)) %>%
  mutate(`BABIP - Mean BABIP` = babip - mean(babip, na.rm = TRUE),
         `xBA - BA` = xba - ba,
         `xwOBA - wOBA` = xwoba - woba,
         `ERA/Barrel %` = ERA/`Barrel %`,
         `ERA/Hard Hit %` = ERA/`Hard Hit %`,
         `ΔERA` = `ERA (t+1)` - ERA,
         `ΔSalary` = `Salary (t+1)` - Salary) %>%
  mutate(`ERA/Barrel %` = format(round(`ERA/Barrel %`, 3), nsmall = 3)) %>%
  mutate(`ERA/Hard Hit %` = format(round(`ERA/Hard Hit %`, 3), nsmall = 3)) %>%
  mutate(`BABIP - Mean BABIP` = format(round(`BABIP - Mean BABIP`, 4), nsmall = 4)) %>%
  rename(Pitches = pitches,
         Pitcher = player_name,
         Year = year,
         BA = ba,
         BABIP = babip,
         wOBA = woba,
         xwOBA = xwoba,
         xBA = xba,
         Hits = hits,
         ABs = abs,
         `Spin Rate` = spin_rate,
         `Velocity` = velocity)
  

mega_summary_uncut <- mega_summary_uncut %>%
  mutate(ba = as.numeric(ba),
         babip = as.numeric(babip),
         woba = as.numeric(woba),
         xwoba = as.numeric(xwoba),
         xba = as.numeric(xba),
         spin_rate = as.numeric(spin_rate),
         velocity = as.numeric(velocity)) %>%
  mutate(`BABIP - Mean BABIP` = babip - mean(babip, na.rm = TRUE),
         `xBA - BA` = xba - ba,
         `xwOBA - wOBA` = xwoba - woba,
         `ERA/Barrel %` = ERA/`Barrel %`,
         `ERA/Hard Hit %` = ERA/`Hard Hit %`,
         `ΔERA` = `ERA (t+1)` - ERA,
         `ΔSalary` = `Salary (t+1)` - Salary) %>%
  mutate(`ERA/Barrel %` = format(round(`ERA/Barrel %`, 3), nsmall = 3)) %>%
  mutate(`ERA/Hard Hit %` = format(round(`ERA/Hard Hit %`, 3), nsmall = 3)) %>%
  mutate(`BABIP - Mean BABIP` = format(round(`BABIP - Mean BABIP`, 4), nsmall = 4)) %>%
  rename(Pitches = pitches,
         Pitcher = player_name,
         Year = year,
         BA = ba,
         BABIP = babip,
         wOBA = woba,
         xwOBA = xwoba,
         xBA = xba,
         Hits = hits,
         ABs = abs,
         `Spin Rate` = spin_rate,
         `Velocity` = velocity) %>%
  select(-Salary, -`Salary (t+1)`, -`ΔSalary`) %>%
  mutate(`BFP/G` = BFP/G)


# Write CSV

write_csv(mega_summary, "Mega Summary.csv")

write_csv(mega_summary_uncut, "Uncut Mega Summary.csv")

write_csv(salary_2015, "2015 Salaries.csv")
write_csv(salary_2016, "2016 Salaries.csv")
write_csv(salary_2017, "2017 Salaries.csv")
write_csv(salary_2018, "2018 Salaries.csv")
write_csv(salary_2019, "2019 Salaries.csv")
write_csv(salary_2019two, "2020 Salaries.csv")


# Load MegaMegaSummary

MegaMegaSummary <- read_csv("MegeMegaSummary.csv")

MegaMegaSummary <- MegaMegaSummary %>%
  filter(BFP > 150) %>%
  mutate(`BFP/G` = BFP/G)  %>%
  filter(`BFP/G` > 16) %>%
  filter(G <= 35) %>%
  rename(Pitcher = pitcher) %>%
  select(-Salary) %<%
 

# Write CSV

write_csv(MegaMegaSummary, "MegaMegaERA.csv")


# Final Mega Summary

FinalMegaSummary <- read_csv("Mega Summary.csv")

FinalMegaSummary <- FinalMegaSummary %>%
  mutate(`Standardized Barrel Luck` = -scale(`ERA/Barrel %`)) %>%
  mutate(`Standardized Hard Hit Luck` = -scale(`ERA/Hard Hit %`)) %>%
  mutate(`Standardized Luck` = (`Standardized Barrel Luck` + `Standardized Hard Hit Luck`)/2) %>%
  mutate(`Luck Adjusted ERA` = ERA + (1/3)*`Standardized Luck`) %>%
  mutate(`Luck Adjusted ERA` = as.numeric(`Luck Adjusted ERA`)) 

FinalMegaSummary <- FinalMegaSummary %>%
  select(-`xBA - BA`, -`Standardized Barrel Luck`, 
         -`Standardized Hard Hit Luck`, -`Standardized Luck`)

FinalMegaSummary <- FinalMegaSummary %>%
  mutate(`Luck Adjusted ERA` = format(round(`Luck Adjusted ERA`, 2), nsmall = 2)) %>%
  filter(`ERA (t+1)` < 7.00) %>%
  filter(`ERA` < 7.00) %>%
  filter(`Luck Adjusted ERA` < 7.00)

FinalMegaSummary <- FinalMegaSummary %>%
  rename(spin_rate = `Spin Rate`,
         K_percent = `K %`,
         BB_percent = `BB %`,
         hard_hit_percent = `Hard Hit %`,
         barrel_percent = `Barrel %`,
         salary_t1 = `Salary (t+1)`,
         ERA_t1 = `ERA (t+1)`,
         BFP_per_G = `BFP/G`,
         xwOBA_minus_wOBA = `xwOBA - wOBA`,
         luck_adj_ERA = `Luck Adjusted ERA`)

write_csv(FinalMegaSummary, "Final Mega Summary.csv")


