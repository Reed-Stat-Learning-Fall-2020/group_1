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
summary_2020 <- rbind()



# Select Variables
summary_2020 %>%
  select(pitch_type, player_name, release_speed, release_spin_rate, plate_x, plate_z)
