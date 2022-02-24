# Kayla Tounalom
#INFO 201 AH
#Data Visualization: Incarceration 


# Loading Packages 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
#library(mapproj)
#library(patchwork)
library(RColorBrewer)  
display.brewer.all()

# Set working directory 

# Load data from csv file
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#Part 1: Introduction/summary information
#Questions: What is the comparison between female and male incarceration from 
#the data set.
#num of rows/observations in Incarceration data 
obs_incarceration <- nrow(incarceration_data)
print(obs_incarceration)

#153811 total rows

#num of columns in Incarceration data
num_features_incarceration <- ncol(incarceration_data)
print(num_features_incarceration)

#121 total columns

#range of years in Incarceration data
range_years <- range(incarceration_data$year)

#1970-2018 

#What is the total number of females incarcerated in 2018?
num_female_2018 <- incarceration_data%>%
  #drop_na() %>%
  filter(year == 2018) %>%
  group_by(female_jail_pop)%>%
  summarise(female_jail_pop = sum(female_jail_pop)) %>% 
  nrow()

#What is the total number of females incarcerated in 1970?
num_female_1970 <- incarceration_data%>%
  #drop_na() %>%
  filter(year == 1970) %>%
  group_by(female_jail_pop)%>%
  summarise(female_jail_pop = sum(female_jail_pop)) %>% 
  nrow()

#What is the total number of males incarcerated in 2018?
num_male_2018 <- incarceration_data %>%
  #drop_na() %>%
  filter(year == 2018) %>%
  group_by(male_jail_pop)%>%
  summarise(male_jail_pop = sum(male_jail_pop)) %>% 
  nrow()

#Testing, I am trying to find the total population of incarcerated 
# In the year 2018.
black_male_2018 <- incarceration_data %>%
 # drop_na() %>%
  group_by(year)%>%
  summarise(black_jail_pop = sum(black_jail_pop)) %>% 
  filter(year == 1990) %>%
  pull(black_jail_pop)

#State with most female jail population
 state_female_incarceration <- incarceration_data %>%
    drop_na() %>%
    filter(female_jail_pop == max(female_jail_pop)) %>%
    pull(state)
  print(state_female_incarceration)

#State with most male jail population
  state_male_incarceration <- incarceration_data %>%
    drop_na() %>%
    filter(male_jail_pop == max(male_jail_pop)) %>%
    pull(state)
  print(state_male_incarceration)
  
  
#Part 2 #############################################################
# Trends over time 
# Number of females jailed over the years
# NO COLOR!-KAYLA
  chart_1 <- ggplot(data = incarceration_data) +
    geom_col(mapping = aes(x = year , y = female_jail_pop)) +
    labs(title = "Female Jail Population Over time")  +
    scale_fill_brewer(palette="Blues")
  
  
#Part 3 ############################################################
# Compares two variables 
# Number of male incarceration over the years
# NO COLOR!-KAYLA
chart_2 <- ggplot(data = incarceration_data) +
    geom_col(mapping = aes(x = year, y = male_jail_pop)) +
    labs(title = "Male Jail Population Over Time") +
    scale_fill_brewer(palette = "Blues")
    

#Part 4 ############################################################
# Map

counties_mod <- incarceration_data %>%
    group_by(female_jail_pop)%>%
   filter(year == 2018)%>%
  summarise(female_jail_pop = sum(female_jail_pop))
    
#Map showing incarceration of females in Washington.
#Compare the regions of incarcerated females in Washington
#Unite
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by="polyname")

map_data <- county_shapes %>%
  left_join(counties_mod, by ="fips") %>%
  filter(state == "washington" & county != "unknown")

#Blank Theme
  blank_theme <- theme_bw() +
    theme(
      axis.line = element_blank(), # remove axis lines 
      axis.text = element_blank(), #remove axis labels 
      axis.ticks = element_blank(), #remove axis ticks
      axis.title = element_blank(), #remove axis tiles 
      plot.background = element_blank(), #remove gray background
      panel.grid.major = element_blank(), #remove major grid lines 
      panel.grid.minor = element_blank(), #remove minor grid lines
      panel.border = element_blank() # remove border around plot
    )
  
#Making Map
  incarceration_map <-ggplot(map_data) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group),
      color = "grey", size = 0.3
    ) +
  coord_map() +
    scale_fill_continuous(limits = c(0, map_data$total_jail_pop),na.value = "white", low ="yellow", high ="red") +
    blank_theme +
    ggtitle("Female Incarceration in the United States")
