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
