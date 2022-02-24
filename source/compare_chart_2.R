incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#Total Georgia jail population
ga_jail_pop <- incarceration_df %>% 
  group_by(year) %>% 
  filter(state == "GA") %>% 
  filter(year >= 2000 & year <= 2013) %>% 
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))

#Total Georgia number of females in Jail
ga_female_jail_pop <- incarceration_df %>% 
  group_by(year) %>% 
  filter(state == "GA") %>% 
  filter(year >= 2000 & year <= 2013) %>% 
  summarize(female_jail_pop = sum(female_jail_pop, na.rm = TRUE))

#Georgia female dcrp numbers
ga_female_jail_dcrp <- incarceration_df %>% 
  group_by(year) %>% 
  filter(state == "GA") %>% 
  filter(year >= 2000 & year <= 2013) %>% 
  summarize(female_jail_pop_dcrp = sum(female_jail_pop_dcrp, na.rm = TRUE))

#Combining "ga_female_jail_pop_dcrp" and "ga_female_jail_pop"
ga_female_jail_pop_dcrp <- left_join(ga_female_jail_pop, ga_female_jail_dcrp,
                                     by = c("year"))

#Total Georgia number of males in Jail
ga_male_jail_pop <- incarceration_df %>% 
  group_by(year) %>% 
  filter(state == "GA") %>% 
  filter(year >= 2000 & year <= 2013) %>% 
  summarize(male_jail_pop = sum(male_jail_pop, na.rm = TRUE))

#Georgia male dcrp numbers
ga_male_jail_dcrp <- incarceration_df %>% 
  group_by(year) %>% 
  filter(state == "GA") %>% 
  filter(year >= 2000 & year <= 2013) %>% 
  summarize(male_jail_pop_dcrp = sum(male_jail_pop_dcrp, na.rm = TRUE))
  
#Combining "ga_male_jail_pop_dcrp" and "ga_male_jail_pop"
ga_male_jail_pop_dcrp <- left_join(ga_male_jail_pop, ga_male_jail_dcrp,
                                     by = c("year"))
#combining male and female data frames
female_male_df <- left_join(ga_female_jail_pop_dcrp, ga_male_jail_pop_dcrp, 
                            by = c("year"))
#Combining total population data frame with female_male_df
combined_df <- left_join(ga_jail_pop, female_male_df, 
                         by = c("year")) %>% 
  gather(key = Sex, value = Number, -year, -total_jail_pop)

chart_2 <- ggplot(data = combined_df) +
  geom_point(mapping = aes(x = total_jail_pop, y = Number, color = Sex)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete () +
  labs(
    x = "Total Jail Population",
    y = "Values",
    color = "Legend",
    title = "Comparison of total population and DCRP between males and females, GA"
  )