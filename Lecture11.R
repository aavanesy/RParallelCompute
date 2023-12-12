
# Coding Session: future parallel processing
library(dplyr); library(tidyr); library(tibble);
library(tidytext); library(rvest);
library(tictoc)

unlink('output/*')

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

all_urls <- read.csv('country_links.csv') %>% 
  pull(1)

# https://furrr.futureverse.org/
# supported by RStudio
library(furrr) 

?plan

plan(sequential)  #default plan


# Using purrr and map
tic()
res_map <- purrr::map(.x = all_urls[1:10], .f = get_ngramms)
toc()

# Using furrr and future map -----
tic()
plan(multisession, workers = 3) # Multiple parallel processes
res_map_future <- furrr::future_map(.x = all_urls[1:10], 
                                    .f = get_ngramms, 
                                    .options = furrr_options(seed = T))
plan(sequential)
toc()

# Using furrr and future map -----
tic()
plan(multisession, workers = 8) # Multiple parallel processes
res_map_future <- furrr::future_map(.x = all_urls, 
                                    .f = get_ngramms,
                                    .options = furrr_options(seed = T))
plan(sequential)
toc()

save(res_map_future, file = 'res_map_future.Rdata')


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

# Using furrr and map -----

unlink('output/*')

tic()
plan(multisession, workers = 6) # Multiple parallel processes
res_map_future <- furrr::future_map(.x = all_urls, 
                                    .f = get_ngramms_silent,
                                    .options = furrr_options(seed = T))
plan(sequential)
toc()

## Error handling? purr codes only
?safely
?quietly
?possibly

## Progress bar -----
x <- replicate(n = 10, runif(20), simplify = FALSE)

fn <- function(x) {
  Sys.sleep(2)
  sum(x)
}

plan(multisession, workers = 2)
result <- future_map(x, fn, .progress = T)
plan(sequential)
###

# https://furrr.futureverse.org/articles/progress.html
library(furrr)
library(progressr)
x <- replicate(n = 10, runif(20), simplify = FALSE)

plan(multisession, workers = 2)


fn <- function(x, p) {
  p()
  Sys.sleep(2)
  sum(x)
}

with_progress({
  p <- progressor(steps = length(x))
  result <- future_map(x, fn, p = p)
})

plan(sequential)
