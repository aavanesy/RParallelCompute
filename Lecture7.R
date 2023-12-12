
# 7.Coding Session: Real Example of Slow Web Scrapping
## Advanced Scrapping Function
library(dplyr); library(tidyr); library(tibble);
library(tidytext); library(rvest);
library(tictoc)

# Pay attention to memory!

get_ngramms <- function(url){
  read_html(url) %>% 
    html_elements("p") %>% 
    html_text() %>% 
    enframe() %>% 
    drop_na() %>% 
    rename(line = 1, text = 2) %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    filter(!is.na(bigram)) %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>% 
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>% 
    count(word1, word2, sort = TRUE) %>% 
    unite(bigram, word1, word2, sep = " ") %>% 
    filter(n >= 5)
}

# First example
url <- 'https://en.wikipedia.org/wiki/United_States'

tic()
usa_words <- get_ngramms(url)
toc()

print(usa_words)

# All countries, one by one?
244 <- read.csv('country_links.csv') %>% 
  pull(1) 

## Function Breakdown -- Extra ----

read_html(url) %>% 
  html_elements("p") %>% 
  html_text() %>% 
  enframe() %>% 
  drop_na() %>% 
  rename(line = 1, text = 2) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  filter(n >= 5)

