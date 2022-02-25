incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(maps)

# Information Summary

# State with highest Black jail population in 1970
state_highest_black_1990 <- incarceration_df %>%
  group_by(state) %>%
  filter(year == 1990) %>%
  summarize(total_black_pop_1990 = sum(black_jail_pop, na.rm = TRUE)) %>%
  filter(total_black_pop_1990 == max(total_black_pop_1990, na.rm = TRUE)) %>%
  pull(state)

# State with highest Black jail population in jail in 2018
state_highest_black_2018 <- incarceration_df %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  summarize(total_black_pop_2018 = sum(black_jail_pop, na.rm = TRUE)) %>%
  filter(total_black_pop_2018 == max(total_black_pop_2018, na.rm = TRUE)) %>%
  pull(state)

# State with lowest Black jail population in 2018
state_lowest_2018 <- incarceration_df %>%
  group_by(state) %>%
  filter(year == 2018) %>%
  summarize(total_black_pop_2018 = sum(black_jail_pop, na.rm = TRUE)) %>%
  filter(total_black_pop_2018 == min(total_black_pop_2018, na.rm = TRUE)) %>%
  pull(state)

# Total Black population in Georgia in 2018
highest_state_black <- incarceration_df %>%
  filter(year == 2018) %>%
  filter(state == "GA") %>%
  summarize(black_pop_ga = sum(black_jail_pop, na.rm = TRUE)) %>%
  pull(black_pop_ga)

# Total convicted white population in Georgia in 2018
highest_state_white <- incarceration_df %>%
  filter(year == 2018) %>%
  filter(state == "GA") %>%
  summarize(white_pop_ga = sum(white_jail_pop, na.rm = TRUE)) %>%
  pull(white_pop_ga)

# CHART 1: TIME CHART

# DATA WRANGLING
# data frame that has the total number of black people convicted in Georgia
# by year from 1990
ga_black_convicted <- incarceration_df %>%
  group_by(year) %>%
  filter(state == "GA") %>%
  filter(year >= 1990) %>%
  summarize(convicted_black_population = sum(black_jail_pop, na.rm = TRUE))

# data frame that has the total number of white people convicted in Georgia
# by year from 1990
ga_white_convicted <- incarceration_df %>%
  group_by(year) %>%
  filter(state == "GA") %>%
  filter(year >= 1990) %>%
  summarize(convicted_white_population = sum(white_jail_pop, na.rm = TRUE))

# joined data frame of the total number of black and white people convicted
# in Georgia
ga_convicted_black_white <- left_join(ga_black_convicted, ga_white_convicted,
                                      by = c("year"))  %>%
  gather(key = Race, value = Population, -year)

# CHART 1: Graph comparing the number of black and white people convicted in
# Georgia overtime

chart_1 <- ggplot(data = ga_convicted_black_white) +
  geom_line(aes(x = year, y = Population, color = Race)) +
  labs(x = "Year",
       y = "Population",
       color = "Legend",
       title = "Population of Black VS White people in jail in Georgia")

#
#
#
#

# Chart 2: Comparing Continous Values

# DATA WRANGLING
# Total Georgia jail population
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
#Data frame combining total population data frame with female_male_df
combined_df <- left_join(ga_jail_pop, female_male_df,
                         by = c("year")) %>%
  gather(key = Sex, value = Number, -year, -total_jail_pop)

# CHART 2: Graph comparing two continous values, total jail population and
# female jail pop, female jail dcrp, male jail pop, and male jail dcrp
chart_2 <- ggplot(data = combined_df) +
  geom_point(mapping = aes(x = total_jail_pop, y = Number, color = Sex)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete() +
  labs(
    x = "Total Jail Population",
    y = "Values",
    color = "Legend",
    title = "Comparison of total population and DCRP
    between males and females, GA"
  )

#
#
#
#

# Map: Geographic variable distribution

# Blank Map of United States
county_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

ggplot(county_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    color = "white", #show state outlines
    size = .1
  ) +
  coord_map()

# Define a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# DATA WRANGLING
# Filtering out df for year 2018, fips, county, state,
black_jail_pop_2018 <- incarceration_df %>%
  filter(year == 2018) %>%
  group_by(fips) %>%
  select(fips, black_jail_pop, total_jail_pop) %>%
  summarize(percent_black_jail_pop
            = (black_jail_pop * 100) / total_jail_pop) %>%
  filter(percent_black_jail_pop <= 100)

# Combining black_jail_pop_2018 data frame and county_shape data frame
combined_df <- left_join(county_shape, black_jail_pop_2018,
                         by = "fips")

# MAP: Rendering map of
map <- ggplot(combined_df) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group,
                  fill = percent_black_jail_pop),
    size = 0.1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Percent",
       title = "2018: Percentage of Black Jail Population
       VS Total Jail Population") +
  blank_theme