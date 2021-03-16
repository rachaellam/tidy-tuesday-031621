install.packages("tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)
library(devtools)
library(ggplot2)
library(dplyr)
library(scales)

tuesdata <- tidytuesdayR::tt_load('2021-03-16')
games <- tuesdata$games
write_csv(games, "inputs/data/raw_data.csv")
raw_data <- read.csv("inputs/data/raw_data.csv")

monthly <- raw_data %>%
  as_tibble() %>%
  group_by(month) %>%
  summarise(avg_total = sum(avg))

monthly$month <- factor(monthly$month, 
                        levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

monthly %>%
  ggplot(aes(x = month, y = avg_total, group = 1)) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(x = "Month",
       y = "Total Average Users") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))