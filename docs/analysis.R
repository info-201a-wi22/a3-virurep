incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = F)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(maps)
library(mapproj)
library(patchwork)
library(usmap)
library(lintr)

#Region with the highest number of black inmates in the most recent year?

reg_highest_black_pop <- incarceration_data %>%
  group_by(region) %>%
  filter(year == max(year)) %>%
  summarize(black_pop_total = sum(black_jail_pop, na.rm = T)) %>%
  filter(black_pop_total == max(black_pop_total)) %>%
  pull(region)

#state with highest percentage of black prisoners in jail in most recent year?

state_highest_black <- incarceration_data %>% 
  group_by(state) %>%
  filter(year ==  max(year, na.rm = T)) %>% 
  summarize(black_jailed = (sum(black_jail_pop, na.rm = T)) / 
              sum(total_pop, na.rm = T)) %>% 
  filter(black_jailed == max(black_jailed, na.rm = T)) %>% 
  pull(state)

#The proportion of all black individuals jailed in 
#Louisiana in the most recent year?

prop_black_in_la<- incarceration_data %>% 
  filter(year == max(year)) %>% 
  filter(state == "LA") %>% 
  summarize(prop_black = (sum(black_jail_pop, na.rm = T)) /
             sum(black_pop_15to64) ) %>% 
  pull(prop_black)

#The proportion of all white individuals jailed in 
#Louisiana in the most recent year?

prop_white_in_la<- incarceration_data %>% 
  filter(year == max(year)) %>% 
  filter(state == "LA") %>% 
  summarize(prop_white = (sum(white_jail_pop, na.rm = T)) /
              sum(white_pop_15to64) ) %>% 
  pull(prop_white)

#County with highest percentage of black prisoners in jail in most recent year?

county_highest_black <- incarceration_data %>% 
  filter(year == max(year, na.rm = T)) %>% 
  filter(black_jail_pop == max(black_jail_pop, na.rm = T)) %>% 
  pull(county_name) 

#Chart 1: that shows incarceration rates of different races after the year 2000
#in the state of Lousinana.

race_over_time <- incarceration_data %>% 
  select(year, state, black_jail_pop, latinx_jail_pop, white_jail_pop) %>% 
  filter(year >= 2000) %>% 
  filter(state == "LA") %>% 
  group_by(year) %>% 
  summarize(total_black_pop = sum(black_jail_pop, na.rm = T),
            total_latino_pop = sum(latinx_jail_pop, na.rm = T),
            total_white_pop = sum(white_jail_pop, na.rm = T))

chart_one <- ggplot(data = race_over_time)+
  geom_line(mapping = aes(x = year,
                          y = total_black_pop, color = "Black Population")) +
  geom_line(mapping = aes(x = year,
                          y = total_latino_pop, color = "Latino Population")) +
  geom_line(mapping = aes(x = year,
                          y = total_white_pop, color = "White Population")) +
  labs(x = "Year",
       y = "Total Jail Inmates",
       title = "Total Jail Inmates Over Time by Race since 2000 in Lousiana",
       colour = "Race")

 #Chart 2: plot comparing the population of 15-64 black inmates to population

jail_vs_total <- incarceration_data %>% 
  select(year, black_jail_pop, black_pop_15to64) %>% 
  filter(year >= 2000) %>% 
  group_by(year) %>% 
  summarize(total_black_pop_ages = sum(black_pop_15to64, na.rm = T),
            total_jail_pop = sum(black_jail_pop, na.rm = T))

chart_two <- ggplot(data = jail_vs_total ) +
  geom_point(mapping = aes(x = total_black_pop_ages,
                           y = total_jail_pop)) +
  labs(x = "Total Black Population Between 15-64 Years of Age",
       y = "Black Population In Jail",
       title = "Black Popuation vs Black Inmates In Jail")

#Chart 3: Heat Map of Black Incarceration

data_map <- incarceration_data %>%
  filter(year == "2018") %>% 
  filter(state == "LA") %>%
  group_by(county_name, fips, state) %>%
  summarize(black_in_jail = sum(black_jail_pop))

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(data_map, by = "fips") %>%
  filter(state == "LA")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

chart_3 <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_in_jail),
    color = "gray", size = 0.3) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_in_jail)),
                        na.value = "white", low = "pink",
                        high = "purple") +
  blank_theme +
  ggtitle("Total Number of African American Incarcerations In Lousiana Per County (2018)")
           





