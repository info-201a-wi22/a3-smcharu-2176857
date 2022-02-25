incarceration_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", header = TRUE, stringsAsFactors = FALSE)

library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(maps)

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
         = (black_jail_pop * 100 ) / total_jail_pop) %>% 
  filter(percent_black_jail_pop <= 100)

# Combining black_jail_pop_2018 data frame and county_shape data frame
combined_df <- left_join(county_shape, black_jail_pop_2018,
                         by = "fips") 
    
# Drawing the Map
map <- ggplot(combined_df) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, 
                  fill = percent_black_jail_pop),
    size = 0.1
  ) +
  coord_map() +
  scale_fill_continuous(low = "#132B43", high = "Red") +
  labs(fill = "Percent  ") +
  blank_theme
  
  
