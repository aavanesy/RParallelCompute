
# 8. Coding Session: Sequential Script Execution
library(dplyr); library(tidyr); library(tibble);
library(tidytext); library(rvest);
library(tictoc)

# Single process calculations 
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


## All countries
country_links <- read.csv('country_links.csv') %>% 
  pull(1) %>% 
  head(10)

## Using Loop ----
for(i in 1:10){
  print(i)
  res_i <- get_ngramms(country_links[i])
}

# Using Base R and Apply Scripts ----
tic()
res_apply <- lapply(country_links[1:10], get_ngramms)
toc()

# Using purrr and map ----
tic()
res_map <- purrr::map(.x = country_links[1:10], .f = get_ngramms)
toc()


# When to use apply and map? ----
# extra functions
?group_map

iris %>%
  group_by(Species) %>%
  group_map(~ broom::tidy(lm(Petal.Length ~ Sepal.Length, data = .x)))





