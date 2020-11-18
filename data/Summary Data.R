# Libraries
library(tidyverse)
library(rvest)
library(baseballr)


# Scrape Data
pitches1_2020 <- scrape_statcast_savant(start_date = "2020-07-23", 
                                        end_date = "2020-07-30", 
                                        player_type='pitcher')

pitches2_2020 <- scrape_statcast_savant(start_date = "2020-07-31", 
                                        end_date = "2020-08-07", 
                                        player_type='pitcher')

pitches3_2020 <- scrape_statcast_savant(start_date = "2020-08-08", 
                                        end_date = "2020-08-15", 
                                        player_type='pitcher')

pitches4_2020 <- scrape_statcast_savant(start_date = "2020-08-16", 
                                        end_date = "2020-08-23", 
                                        player_type='pitcher')

pitches5_2020 <- scrape_statcast_savant(start_date = "2020-08-24", 
                                        end_date = "2020-08-31", 
                                        player_type='pitcher')

pitches6_2020 <- scrape_statcast_savant(start_date = "2020-09-01", 
                                        end_date = "2020-09-08", 
                                        player_type='pitcher')

pitches7_2020 <- scrape_statcast_savant(start_date = "2020-09-09", 
                                        end_date = "2020-09-16", 
                                        player_type='pitcher')

pitches8_2020 <- scrape_statcast_savant(start_date = "2020-09-17", 
                                        end_date = "2020-09-24", 
                                        player_type='pitcher')

pitches9_2020 <- scrape_statcast_savant(start_date = "2020-09-25", 
                                        end_date = "2020-09-27", 
                                        player_type='pitcher')



# Create Data Frame
summary_2020 <- rbind(pitches1_2020,
                      pitches2_2020,
                      pitches3_2020,
                      pitches4_2020,
                      pitches5_2020,
                      pitches6_2020,
                      pitches7_2020,
                      pitches8_2020,
                      pitches9_2020)



# Select Variables
summary_2020 <- summary_2020 %>%
  select(pitch_type, player_name, release_speed, effective_speed, 
         release_spin_rate, plate_x, plate_z, zone, pfx_x, pfx_z, events)



# Create Outcome Variables
summary_2020 <- summary_2020 %>%
  mutate(`1B` = ifelse(events == "single", 1, 0),
         `2B` = ifelse(events == "double", 1, 0),
         `3B` = ifelse(events == "triple", 1, 0),
         `HR` = ifelse(events == "home_run", 1, 0),
         `SF` = ifelse(events == "sac_fly", 1, 0),
         `BB` = ifelse(events == "walk" | events == "hit_by_pitch", 1, 0),
         `HBP` = ifelse(events == "hit_by_pitch", 1, 0),
         `SO` = ifelse(events == "strikeout", 1, 0),
         `AB` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "strikeout_double_play" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play", 1, 0),
         `PA` = ifelse(events == "single" | events == "double" | events == "triple" | events == "home_run" | events == "strikeout" | events == "strikeout_double_play" | events == "double_play" | events == "field_error" | events == "field_out" | events == "fielders_choice" | events == "force_out" | events == "grounded_into_double_play" | events == "walk" | events == "hit_by_pitch" | events == "sac_fly", 1, 0))



# Create Pitch Category Variable
summary_2020 <- summary_2020 %>%
  mutate(`pitch_category` = ifelse(pitch_type == "FF" | events == "FT" | events == "FC", "Fastball", "Off-speed"))



# Create Corner Variable (Proxy for Command)
summary_2020 <- summary_2020 %>%
  mutate(`corner` = ifelse(zone == 1 | events == 3 | events == 7 | events == 9, 1, 0))



# Summary Data
summary_2020 <- summary_2020 %>%
  group_by(player_name, pitch_category) %>%
  summarise(`Pitches` = n(),
            `Average Pitch Speed` = mean(release_speed, na.rm = TRUE),
            `Average Effective Pitch Speed` = mean(effective_speed, na.rm = TRUE),
            `Average Spin Rate` = mean(release_spin_rate, na.rm = TRUE),
            `Average Horizontal Break` = mean(pfx_x, na.rm = TRUE),
            `Average Vertical Break` = mean(pfx_z, na.rm = TRUE),
            `Corner %` = (sum(corner = 1))/n(),
            `BAA` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1) + sum(`HR` == 1))/(sum(`AB` == 1)),
            `SLGA` = (sum(`1B` == 1) + 2*sum(`2B` == 1) + 3*sum(`3B` == 1) + 4*sum(`HR` == 1))/(sum(`AB` == 1)),
            `BABIPA` = (sum(`1B` == 1) + sum(`2B` == 1) + sum(`3B` == 1))/(sum(`AB` == 1) - sum(`HR` == 1) - sum(`SO` == 1) + sum(`SF` == 1)),
            `wOBAA` = (0.69*sum(`BB` == 1) + 0.72*sum(`HBP` == 1) + 0.89*sum(`1B` == 1) + 1.27*sum(`2B` == 1) + 1.62*sum(`3B` == 1) + 2.1*sum(`HR` == 1))/(sum(`PA` == 1)))

            

# Filter
summary_2020 <- summary_2020 %>%
  filter(Pitches > 99)



# Round Numbers
summary_2020_rounded <- summary_2020 %>%
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


# Write CSV
write_csv(summary_2020_rounded, "2020_summary.csv")


            
