### Preamble ###
# Purpose: Play with Tidy Tuesday content
# Author: Rachael Lam
# Date: March 16 2021
# Contact: rachael.lam@mail.utoronto.ca
# Pre-req: None

### Setting Up Workspace ###
#install.packages("tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)
library(devtools)
library(ggplot2)
library(dplyr)
library(scales)

### Grabbing Data ###
tuesdata <- tidytuesdayR::tt_load('2021-03-16')
games <- tuesdata$games
write_csv(games, "inputs/data/raw_data.csv")
raw_data <- read.csv("inputs/data/raw_data.csv")

### Looking at monthly number of players across all years ###
monthly <- raw_data %>%
  as_tibble() %>%
  group_by(month) %>%
  summarise(avg_total = sum(avg))

monthly$month <- factor(monthly$month, 
                        levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

monthly %>%
  ggplot(aes(x = month, y = avg_total, group = 1)) +
  geom_smooth(se = FALSE, color = "#fe92f8") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(x = "Month",
       y = "Total Average Users",
       title = "Average Users Plotted Monthly Between 2012 and 2021 ") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Looking at number of players across all years ###
yearly <- raw_data %>%
  as_tibble() %>%
  group_by(year) %>%
  summarise(avg_total = sum(avg))

yearly %>%
  ggplot(aes(x = year, y = avg_total, group = 1)) +
  geom_smooth(se = FALSE, color = "#0595e4") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year",
       y = "Total Average Users",
       title = "Average Users Plotted Between 2012 and 2021") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Looking at the COVID-19 year ###
twentytwenty <- raw_data %>%
  as_tibble() %>%
  filter_at(vars("year"), any_vars(. == 2020)) %>%
  group_by(month) %>%
  summarise(avg_total = sum(avg))


twentytwenty$month <- factor(twentytwenty$month, 
                        levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

twentytwenty %>%
  ggplot(aes(x = month, y = avg_total, group = 1)) +
  geom_smooth(se = FALSE, color = "#ce85f9") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  labs(x = "Year",
       y = "Total Average Users",
       title = "Average Users Plotted Monthly in 2020 (COVID-19 Year)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









