# Libraries
library(tidyverse)



# Load Data
pitcher_data <- read_csv("2020_summary.csv")



# Off-speed RPM vs. wOBAA
pitcher_data %>%
  filter(pitch_category == "Off-speed") %>%
  ggplot(aes(x = `Average Spin Rate`, y = wOBAA)) +
  geom_point(size = 2, alpha = 0.5) +
  labs(title = "Average Spin Rate vs. wOBAA (Off-speed)",
       x = "Average Spin Rate (RPM)", y = "wOBAA") +
  theme_minimal()

cor(pitcher_data$`Average Spin Rate`, pitcher_data$wOBAA)

# Result: No relatonship



# Fastball Speed vs. wOBAA
pitcher_data %>%
  filter(pitch_category == "Fastball") %>%
  ggplot(aes(x = `Average Pitch Speed`, y = wOBAA)) +
  geom_point(size = 2, alpha = 0.5) +
  labs(title = "Average Pitch Speed vs. wOBAA (Fastball)",
       x = "Average Pitch Speed (MPH)", y = "wOBAA") +
  theme_minimal()

cor(pitcher_data$`Average Pitch Speed`, pitcher_data$wOBAA)

# Result: No relatonship

