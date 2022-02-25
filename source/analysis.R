# Kayla Tounalom
#INFO 201 AH
#Data Visualization: Incarceration 

# Loading Packages 
library(dplyr)
library(tidyverse)
library(ggplot2)
library(maps)
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

#Range of years in Incarceration data
range_years <- range(incarceration_data$year)

#1970-2018 

#Total number of black females incarcerated in 2000
black_female_2000 <- incarceration_data%>%
  group_by(year)%>%
  summarise(black_female_prison_pop = sum(black_female_prison_pop, na.rm = T))%>%
  filter(year == "2000")%>%
  arrange(-year) %>%
  pull(black_female_prison_pop)

# 17407

#Total number of black females incarcerated in 1970
black_female_1970 <- incarceration_data%>%
  group_by(year)%>%
  summarise(black_female_prison_pop = sum(black_female_prison_pop, na.rm = T))%>%
  filter(year == "1970")%>%
  arrange(-year) %>%
  pull(black_female_prison_pop)

#86

#Total number of white females incarcerated in 2000
white_female_2000 <- incarceration_data%>%
  group_by(year)%>%
  summarise(white_female_prison_pop= sum(white_female_prison_pop, na.rm = T))%>%
  filter(year == "2000")%>%
  arrange(-year) %>%
  pull(white_female_prison_pop)

# 18010
  
# State with Highest percentage of black female incarcerated in the U.S.
highest_percent_femaleB <- incarceration_data %>%
  drop_na() %>%
  filter(black_female_prison_pop == max(black_female_prison_pop))%>%
  pull(state)

# NY

# State with Highest percentage of white female incarcerated in the U.S.
highest_percent_femaleW <- incarceration_data %>%
  drop_na() %>%
  filter(white_female_prison_pop == max(white_female_prison_pop))%>%
  pull(state)

# CA

#State with most female incarcerated population
 state_female_incarceration <- incarceration_data %>%
    drop_na() %>%
    filter(female_jail_pop == max(female_jail_pop)) %>%
    pull(state)
  print(state_female_incarceration)

#NY

#Part 2 ############################################################
# Trends over time 
# Number of white females incarcerated over the years 1970-2010
  chart_1 <- ggplot(data = incarceration_data) +
    geom_col(mapping = aes(x = year , y = white_female_prison_pop, fill = year)) +
    scale_fill_distiller(palette = "PuBu")+
    labs(title = "White Female Jail Population Over Time") +
    scale_alpha_continuous(10, 20)

  
#Part 3 ############################################################
# Compares two variables 
# Number of black female incarceration over the years 1970-2010
  chart_2 <- ggplot(data = incarceration_data) +
    geom_col(mapping = aes(x = year , y = black_female_prison_pop, fill = year)) +
    scale_fill_distiller(palette = "PuRd")+  ## feel free to change the palette as you find best
    labs(title = "Black Female Jail Population Over time")  +
    scale_alpha_continuous(10, 20)    

#Part 4 ############################################################
# Map
county_mod <- incarceration_data %>%
    group_by(county_name) %>%
    select(year, fips, black_female_prison_pop, county_name, state)

    
#Map showing incarceration of females in Washington.
#Compare the regions of incarcerated females in Washington
#Unite
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by="polyname")

map_data <- left_join(county_mod, county_shapes)

ny_data <- map_data %>%
  filter(state == "NY", na.rm = TRUE) %>%
  drop_na()

#Blank Theme
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
  
#Making Map
  incarceration_map <-ggplot(ny_data) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = black_female_prison_pop),
      color = "grey", size = 0.3
    ) +
  coord_map() +
    scale_fill_continuous(limits = c(0, max(ny_data$black_female_prison_pop)),na.value = "white", low ="blue", high ="red") +
    blank_theme +
    ggtitle("Black Female Incarceration in New York State") 
    
  
    
