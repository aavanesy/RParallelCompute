
# 6.Coding Session: Scrapping Wikipedia
library(rvest)
library(tidyverse)

# Simple dplyr style code
# This is my personal preference of codestyle 
# Most of my style is coming from https://style.tidyverse.org/

# Objective is to scrap the text for all countries and perform a simple text mining..
all_countries <- 'https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population'

# First we need a list of all countries. ----
country_links <- read_html(all_countries) %>%
  html_nodes(".wikitable a") %>%
  html_attr("href") %>% 
  enframe() %>% 
  filter(grepl('wiki', value)) %>% 
  mutate(value = paste0('https://en.wikipedia.org', value)) %>% 
  slice(-c(1:3)) %>% 
  # arrange(value) %>% 
  select(Country = value) %>% 
  distinct()  #remove duplicates

write.csv(country_links, file = 'country_links.csv', row.names = F)


## Alternative data - for your personal project ----
# Not as much data, example for replication purposes
all_countries <- 'https://en.wikipedia.org/wiki/World_Heritage_Sites_by_country'

country_links <- read_html(all_countries) %>%
  html_nodes(".wikitable a") %>%
  html_attr("href") %>% 
  enframe() %>% 
  filter(grepl('wiki', value)) %>% 
  filter(grepl('Heritage', value)) %>% 
  mutate(value = paste0('https://en.wikipedia.org', value)) %>% 
  # arrange(value) %>% 
  select(Country = value) %>% 
  distinct()

write.csv(country_links, file = 'country_links.csv', row.names = F)

