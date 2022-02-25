incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)

#DATA WRANGLING
#data frame that has the total number of black people convicted in Texas 
#by year
ga_black_convicted <- incarceration_df %>% 
  group_by(year) %>% 
  filter(state == "GA") %>% 
  filter(year >= 1990) %>% 
  summarize(year_black_population = sum(black_jail_pop, na.rm = TRUE))

#data frame that has the total number of white people convicted in Texas 
#by year
ga_white_convicted <- incarceration_df %>% 
  group_by(year) %>% 
  filter(state == "GA") %>% 
  filter(year >= 1990) %>% 
  summarize(year_white_population = sum(white_jail_pop, na.rm = TRUE))

#joined data frame of the total number of black and white people convicted
#in Texas
ga_black_white <- left_join(ga_black_convicted, ga_white_convicted, 
                            by = c("year")) %>% 
  gather(key = Race, value = Population, -year)

#GRAPH
#Graph comparing the number of black and white people convicted in 
#Texas overtime

chart_1 <- ggplot(data = ga_black_white) + 
  geom_line(aes(x = year, y = Population, color = Race)) +
  labs(x = "Year", 
       y = "Population",
       color = "Legend",
       title = "Population of Black VS White people in jail in Georgia")