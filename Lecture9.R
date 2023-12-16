## Advanced Scrapping Function
# Pay attention to memory
library(dplyr); library(tidyr); library(tibble);
library(tidytext); library(rvest); library(purrr);
library(tictoc)

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

# throws error 
get_ngramms('https://en.wikipedia.org/wiki/United_States')
get_ngramms('abc')

# When running thousands of threads, we need to make sure code continues to run

# Simple Solutions

# Wrapper Option with try and silent = T ----
url = 'xyz'
res <- try(get_ngramms(url), silent = T)

get_ngramms_silent <- function(url){
  res <- try(get_ngramms(url), silent = T)
  if(class(res) == "try-error"){
    warning(paste('Incorrect URL:', url)) # Warning will show in console
    return(tibble())
  }else{
    return(res)
  }
}

result <- get_ngramms_silent('xyzz')


# Saving Intermediate Results ----

get_ngramms_silent <- function(url, save_path = 'output/'){
  res <- try(get_ngramms(url), silent = T)
  if(length(class(res)) == 1 && class(res) == "try-error"){
    warning(paste('Incorrect URL:', url)) # Warning will show in console
    return(tibble())
  }else{
    url2 <- url %>% gsub('[/]', '_', .) %>% 
      gsub('[:]', '_', .) %>% 
      gsub('https___en.wikipedia.org_wiki_', '', .)
    save(res, file = paste0(save_path, url2, '.Rdata'))
    return(res)
  }
}


dir.create('output')

result <- get_ngramms_silent('xyzz')

url3 <- 'https://en.wikipedia.org/wiki/United_States'
result <- get_ngramms_silent(url3)

all_countries <- read.csv('country_links.csv') %>% 
  pull(1)

tic()
res_map <- purrr::map(.x = all_countries, .f = get_ngramms_silent)
toc()



