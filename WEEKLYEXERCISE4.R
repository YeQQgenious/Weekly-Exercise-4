 DEV
#main
library(readr)
library(dplyr)
library(ggplot2)
olympics <- read.csv("Olympics.csv")
#sum of gold,silver,brozn,and add in olympic.csv.
olympics <- olympics %>%
  mutate(total.medals = gold + silver + bronze)

head(olympics)
#Each country gold medals win.
gold_by_country <- olympics%>%
  group_by(country)%>%
  summarise(total_glad = sum(gold,na.rm = TRUE))

head(gold_by_country)
#Each years total medals.
medals_by_year <- olympics %>%
  group_by(year) %>%
  summarise(total_medals = sum(gold + silver + bronze, na.rm = TRUE))

head(medals_by_year)
#visualization
ggplot(medals_by_year, aes(x = year, y = total_medals)) +
  geom_line(color = "black", size = 1) +
  geom_point(size = 2) +
  labs(title = "Total Medals by Year",
       x = "Year", 
       y = "Total Medals")

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

# plot the gold medals overtime.
ggplot(gold_data, aes(x = year, y = total_gold, color = country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Gold Medals Over Time (Selected Countries)",
       x = "Year",
       y = "Number of Gold Medals") +
  theme_minimal() +
  theme(legend.title = element_blank())
 main
