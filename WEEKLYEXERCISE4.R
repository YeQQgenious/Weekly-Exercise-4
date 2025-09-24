# Main
library(readr)
library(dplyr)
library(ggplot2)
# Read csv
olympics <- read_csv("Olympics.csv")

# Find country had the largest delegation of athletes in 1992
largest_delegation <- olympics %>%
  filter(athletes == max(athletes, na.rm = TRUE)) %>%
  select(country, athletes)

largest_delegation
#create tibble
country_athletes <- olympics %>%
  select(country, athletes)

head(country_athletes)

#Choose country
five_countries <- c("United States", "France", "Germany", "Russia", "China")

# calculate gold medals
gold_data <- olympics %>%
  filter(country %in% five_countries) %>%
  group_by(year, country) %>%
  summarise(total_gold = sum(gold, na.rm = TRUE)) %>%
  ungroup()

# plot the 
ggplot(gold_data, aes(x = year, y = total_gold, color = country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Gold Medals Over Time (Selected Countries)",
       x = "Year",
       y = "Number of Gold Medals") +
  theme_minimal() +
  theme(legend.title = element_blank())