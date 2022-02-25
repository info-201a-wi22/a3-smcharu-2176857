incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)
View(incarceration_df)

library(dplyr)
library(tidyr)
library(stringr)

#State with highest Black population between the ages of 15 and 64 in 1970
state_highest_black_1990 <- incarceration_df %>% 
  group_by(state) %>% 
  filter(year == 1990) %>% 
  summarize(total_black_pop_1990 = sum(black_jail_pop, na.rm = TRUE)) %>% 
  filter(total_black_pop_1990 == max(total_black_pop_1990, na.rm = TRUE)) %>% 
  select(state)

#State with highest Black population in jail in 2018 
state_highest_black_2018 <- incarceration_df %>% 
  group_by(state) %>% 
  filter(year == 2018) %>% 
  summarize(total_black_pop_2018 = sum(black_jail_pop, na.rm = TRUE)) %>% 
  filter(total_black_pop_2018 == max(total_black_pop_2018, na.rm = TRUE)) %>% 
  select(state)

#State with Black lowest population convicted in 2018 
state_lowest_2018 <- incarceration_df %>% 
  group_by(state) %>% 
  filter(year == 2018) %>% 
  summarize(total_black_pop_2018= sum(black_jail_pop, na.rm = TRUE)) %>% 
  filter(total_black_pop_2018 == min(total_black_pop_2018, na.rm = TRUE)) %>% 
  select(state)

#Total convicted Black population in Georgia in 2018
highest_state_black <- incarceration_df %>% 
  filter(year == 2018) %>% 
  filter(state == "GA") %>% 
  summarize(black_pop_ga = sum(black_jail_pop, na.rm = TRUE)) 
  
#Total convicted white population in Georgia in 2018
highest_state_white <- incarceration_df %>% 
  filter(year == 2018) %>% 
  filter(state == "GA") %>% 
  summarize(white_pop_ga = sum(white_jail_pop, na.rm = TRUE))

