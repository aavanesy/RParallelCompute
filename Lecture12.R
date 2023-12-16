# Coding Session: Data Manipulation with multidplyr

# https://multidplyr.tidyverse.org/index.html
# install.packages("multidplyr")
# https://github.com/tidyverse/multidplyr/

library(multidplyr)
library(dplyr, warn.conflicts = FALSE)
library(nycflights13)
library(tictoc)

str(flights)

flights %>% head() %>% View()


?multidplyr::new_cluster

cluster <- new_cluster(4)

print(cluster)

flights1 <- flights %>% group_by(dest) %>% partition(cluster)
flights1

tic()
flights %>% 
  group_by(dest) %>% 
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE))
toc()

tic()
flights1 %>% 
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  collect()
toc()

## WHEN TO USE ----
# For basic dplyr verbs, multidplyr is unlikely to give you significant speed ups 
# unless you have 10s or 100s of millions of data points
# (and in that scenario you should first try dtplyr, which uses data.table).
# 
# multipldyr might help, however, if you’re doing more complex things. 
# Let’s see how that plays out when fitting a moderately complex model. 

daily_flights <- flights %>%
  count(dest) %>%
  filter(n >= 365)

common_dest <- flights %>% 
  semi_join(daily_flights, by = "dest") %>% 
  mutate(yday = lubridate::yday(ISOdate(year, month, day))) %>% 
  group_by(dest)

by_dest <- common_dest %>% partition(cluster)
by_dest

## cluster requires the library to be loaded prior to run
library(mgcv)

tic()
models <- common_dest %>% 
  group_by(dest) %>% 
  do(mod = gam(dep_delay ~ s(yday) + s(dep_time), data = .))
toc()

cluster_library(cluster, "mgcv")
tic()
models <- by_dest %>% 
  do(mod = gam(dep_delay ~ s(yday) + s(dep_time), data = .))
toc()
