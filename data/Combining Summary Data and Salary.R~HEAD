library(tidyverse)
library(rvest)
library(baseballr)
library(plyr)



# Scrape Data 2015
pitches1_2015 <- scrape_statcast_savant(start_date = "2015-4-3", 
                                        end_date = "2015-4-10", 
                                        player_type='pitcher')

pitches2_2015 <- scrape_statcast_savant(start_date = "2015-4-11", 
                                        end_date = "2015-4-18", 
                                        player_type='pitcher')

pitches3_2015 <- scrape_statcast_savant(start_date = "2015-4-19", 
                                        end_date = "2015-4-26", 
                                        player_type='pitcher')

pitches4_2015 <- scrape_statcast_savant(start_date = "2015-4-27", 
                                        end_date = "2015-5-4", 
                                        player_type='pitcher')

pitches5_2015 <- scrape_statcast_savant(start_date = "2015-5-5", 
                                        end_date = "2015-5-12", 
                                        player_type='pitcher')

pitches6_2015 <- scrape_statcast_savant(start_date = "2015-5-13", 
                                        end_date = "2015-5-20", 
                                        player_type='pitcher')


pitches7_2015 <- scrape_statcast_savant(start_date = "2015-5-21", 
                                        end_date = "2015-5-28", 
                                        player_type='pitcher')

pitches8_2015 <- scrape_statcast_savant(start_date = "2015-5-29", 
                                        end_date = "2015-6-5", 
                                        player_type='pitcher')

pitches9_2015 <- scrape_statcast_savant(start_date = "2015-6-6", 
                                        end_date = "2015-6-13", 
                                        player_type='pitcher')

pitches10_2015 <- scrape_statcast_savant(start_date = "2015-6-14", 
                                        end_date = "2015-6-21", 
                                        player_type='pitcher')

pitches11_2015 <- scrape_statcast_savant(start_date = "2015-6-21", 
                                        end_date = "2015-6-28", 
                                        player_type='pitcher')

pitches12_2015 <- scrape_statcast_savant(start_date = "2015-6-29", 
                                         end_date = "2015-7-6", 
                                         player_type='pitcher')

pitches13_2015 <- scrape_statcast_savant(start_date = "2015-7-7", 
                                         end_date = "2015-7-14", 
                                         player_type='pitcher')

pitches14_2015 <- scrape_statcast_savant(start_date = "2015-7-15", 
                                         end_date = "2015-7-22", 
                                         player_type='pitcher')

pitches15_2015 <- scrape_statcast_savant(start_date = "2015-7-23", 
                                         end_date = "2015-7-30", 
                                         player_type='pitcher')

pitches16_2015 <- scrape_statcast_savant(start_date = "2015-7-31", 
                                         end_date = "2015-8-7", 
                                         player_type='pitcher')

pitches17_2015 <- scrape_statcast_savant(start_date = "2015-8-8", 
                                         end_date = "2015-8-15", 
                                         player_type='pitcher')

pitches18_2015 <- scrape_statcast_savant(start_date = "2015-8-22", 
                                         end_date = "2015-8-29", 
                                         player_type='pitcher')

pitches17_2015 <- scrape_statcast_savant(start_date = "2015-8-30", 
                                         end_date = "2015-9-6", 
                                         player_type='pitcher')

pitches18_2015 <- scrape_statcast_savant(start_date = "2015-9-7", 
                                         end_date = "2015-9-14", 
                                         player_type='pitcher')

pitches19_2015 <- scrape_statcast_savant(start_date = "2015-9-15", 
                                         end_date = "2015-9-22", 
                                         player_type='pitcher')

pitches20_2015 <- scrape_statcast_savant(start_date = "2015-9-23", 
                                         end_date = "2015-10-2", 
                                         player_type='pitcher')

# Create Data Frame
summary_2015 <- rbind(pitches1_2015,
                      pitches2_2015,
                      pitches3_2015,
                      pitches4_2015,
                      pitches5_2015,
                      pitches6_2015,
                      pitches7_2015,
                      pitches8_2015,
                      pitches9_2015,
                      pitches10_2015,
                      pitches11_2015,
                      pitches12_2015,
                      pitches13_2015,
                      pitches14_2015,
                      pitches15_2015,
                      pitches16_2015,
                      pitches17_2015,
                      pitches18_2015,
                      pitches19_2015,
                      pitches20_2015)



# Select Variables
summary_2015 <- summary_2015 %>%
  dplyr::select(pitch_type, player_name, release_speed, effective_speed, 
         release_spin_rate, plate_x, plate_z, zone, pfx_x, pfx_z, events,balls,barrel,type)



# Create Outcome Variables
summary_2015 <- summary_2015 %>%
  mutate(`1B` = ifelse(events == "single", 1, 0),
         `2B` = ifelse(events == "double", 1, 0),
         `3B` = ifelse(events == "triple", 1, 0),
         `HR` = ifelse(events == "home_run", 1, 0),
         `SF` = ifelse(events == "sac_fly", 1, 0),
         `BB` = ifelse(events == "walk" | events == "hit_by_pitch", 1, 0),
         `HBP` = ifelse(events == "hit_by_pitch", 1, 0),
         `SO` = ifelse(events == "strikeout", 1, 0),
         `Strike` = ifelse(type == "S", 1, 0),
         `Ball` = ifelse(type == "B", 1, 0),
         `Hit` = ifelse(type == "X", 1, 0),
         `AB` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "strikeout_double_play" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play", 1, 0),
         `PA` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "strikeout_double_play" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play" | events == "walk" | events == "hit_by_pitch" | events == "sac_fly", 1, 0))



# Create Pitch Category Variable


# Create Corner Variable (Proxy for Command)
summary_2015 <- summary_2015 %>%
  mutate(`corner` = ifelse(zone == 1 | zone == 3 | zone == 7 | zone == 9, 1, 0))



# Summary Data
summary_2015 <- summary_2015 %>%
  group_by(player_name) %>%
  dplyr::summarise(`Pitches` = n(),
            `Average Pitch Speed` = mean(release_speed, na.rm = TRUE),
            `Average Effective Pitch Speed` = mean(effective_speed, na.rm = TRUE),
            `Average Spin Rate` = mean(release_spin_rate, na.rm = TRUE),
            `Average Horizontal Break` = mean(pfx_x, na.rm = TRUE),
            `Average Vertical Break` = mean(pfx_z, na.rm = TRUE),
            `Average Balls` = mean(balls, na.rm = TRUE),
            `Average Barrel` = mean(barrel, na.rm = TRUE),
            `Corner %` = mean(corner, na.rm = TRUE),
            `Total Strikes`= (sum(Strike)),
            `Total Balls`= (sum(Ball)),
            `Total Hit`= (sum(Hit)),
            `Strike Percent` =(sum(Strike)) / (sum(Strike)+sum(Ball)+sum(Hit)),
            `Ball Percent` =(sum(Ball)) / (sum(Strike)+sum(Ball)+sum(Hit)),
            `Hit Percent` =(sum(Hit)) / (sum(Strike)+sum(Ball)+sum(Hit)),
            `BAA` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1) + sum(`HR` == 1))/(sum(`AB` == 1)),
            `SLGA` = (sum(`1B` == 1) + 2*sum(`2B` == 1) + 3*sum(`3B` == 1) + 4*sum(`HR` == 1))/(sum(`AB` == 1)),
            `BABIPA` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1))/(sum(`AB` == 1) - sum(`HR` == 1) - sum(`SO` == 1) + sum(`SF` == 1)),
            `wOBAA` = (0.69*sum(`BB` == 1) + 0.72*sum(`HBP` == 1) + 0.89*sum(`1B` == 1) + 1.27*sum(`2B` == 1) + 1.62*sum(`3B` == 1) + 2.1*sum(`HR` == 1))/(sum(`PA` == 1)))



# Filter
summary_2015 <- summary_2015 %>%
  filter(Pitches > 99)



# Round Numbers
summary_2015_rounded <- summary_2015 %>%
  mutate(`Average Pitch Speed` = format(round(`Average Pitch Speed`, 1), nsmall = 1)) %>%
  mutate(`Average Effective Pitch Speed` = format(round(`Average Effective Pitch Speed`, 1), nsmall = 1)) %>%
  mutate(`Average Spin Rate` = format(round(`Average Spin Rate`, 1), nsmall = 1)) %>%
  mutate(`Average Horizontal Break` = format(round(`Average Horizontal Break`, 3), nsmall = 3)) %>%
  mutate(`Average Vertical Break` = format(round(`Average Vertical Break`, 3), nsmall = 3)) %>%
  mutate(`Corner %` = format(round(`Corner %`, 3), nsmall = 3)) %>%
  mutate(`BAA` = format(round(`BAA`, 3), nsmall = 3)) %>%
  mutate(`SLGA` = format(round(`SLGA`, 3), nsmall = 3)) %>%
  mutate(`BABIPA` = format(round(`BABIPA`, 3), nsmall = 3)) %>%
  mutate(`wOBAA` = format(round(`wOBAA`, 3), nsmall = 3))

view(summary_2015_rounded)

library(readr)

pitcher_salaries_wins <- read_csv("data/pitcher_salaries_wins.csv")
View(pitcher_salaries_wins)

pitcher_wins <- pitcher_salaries_wins %>% dplyr::select(mlb_id,W,L,ER,player_name)

pitcher_salary_ERA<-pitcher_salaries_wins %>% dplyr::select(mlb_id,player_name,salary,yearID,ERA)

pitcher_wins<-ddply(pitcher_wins,"player_name",numcolwise(sum))

pitcher_salary_ERA<-ddply(pitcher_salary_ERA,"player_name",numcolwise(mean))

pitcher_salaries_wins_joined <- pitcher_salary_ERA %>% inner_join(pitcher_wins)

total_2015<-summary_2015 %>% inner_join(pitcher_salaries_wins_joined)

total_2015_WL <- total_2015 %>%
  group_by(player_name) %>%
  summarise(`Total Wins` = sum(W),`Total Loss`=sum(L),`Total ER`=sum(ER),`ERA`=ERA)

total_2015_final<- total_2015 %>% inner_join(total_2015_WL)


total_2015_filtered<-total_2015_final %>% filter(salary>550000)

write_csv(total_2015_filtered, "total_2015_filtered.csv")



#total_2015_filtered<- total_2015_filtered %>% dplyr::mutate(Win_Percent=`Total Wins`/(`Total Wins`+`Total Loss`)) %>% drop_na()

ggplot(total_2015_filtered, aes( x=`Total Wins`,y=log(salary))) +
  geom_point(alpha=0.5,color="dodgerblue",fill="dodgerblue")+
  theme_classic()

cor(total_2015_filtered$`Total Wins`,log(total_2015_filtered$salary))

cor(total_2015_filtered$`Total Strikes`,log(total_2015_filtered$salary))

