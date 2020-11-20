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
  filter(BFP >= 100) %>% 
  select(player_name, ERA) %>%
  rename(`ERA (t+1)` = ERA)


# 2017 ERA

ERA_2017 <- lahman_2017 %>%
  filter(BFP >= 100) %>% 
  select(player_name, ERA) %>%
  rename(`ERA (t+1)` = ERA)


# 2018 ERA

ERA_2018 <- lahman_2018 %>%
  filter(BFP >= 100) %>% 
  select(player_name, ERA) %>%
  rename(`ERA (t+1)` = ERA)


# 2019 ERA

ERA_2019 <- lahman_2019 %>%
  filter(BFP >= 100) %>% 
  select(player_name, ERA) %>%
  rename(`ERA (t+1)` = ERA)


# 2015 Summary

summary_2015 <- left_join(pitchers_2015, lahman_2015, by = "player_name")
summary_2015 <- left_join(summary_2015, ERA_2016, by = "player_name")


# 2016 Summary

summary_2016 <- left_join(pitchers_2016, lahman_2016, by = "player_name")
summary_2016 <- left_join(summary_2016, ERA_2017, by = "player_name")


# 2017 Summary

summary_2017 <- left_join(pitchers_2017, lahman_2017, by = "player_name")
summary_2017 <- left_join(summary_2017, ERA_2018, by = "player_name")


# 2018 Summary

summary_2018 <- left_join(pitchers_2018, lahman_2018, by = "player_name")
summary_2018 <- left_join(summary_2018, ERA_2019, by = "player_name")


# Mega Summary

mega_summary <- rbind(summary_2015,
                      summary_2016,
                      summary_2017,
                      summary_2018)


# Variable Mutations

mega_summary <- mega_summary %>%
  mutate(ba = as.numeric(ba),
         babip = as.numeric(babip),
         woba = as.numeric(woba),
         xwoba = as.numeric(xwoba),
         xba = as.numeric(xba),
         spin_rate = as.numeric(spin_rate),
         velocity = as.numeric(velocity)) %>%
  mutate(`xBA - BA` = xba - ba,
         `xwOBA - wOBA` = xwoba - woba,
         `ΔERA` = `ERA (t+1)` - ERA)


# Filter

mega_summary <- mega_summary %>%
  filter(BFP >= 100) 

mega_summary <- mega_summary %>%
  drop_na()


# Write CSV

write_csv(mega_summary, "Mega Summary.csv")






