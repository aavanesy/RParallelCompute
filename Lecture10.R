library(dplyr); library(tidyr); library(tibble);
library(tidytext); library(rvest);
library(tictoc)

dir.create('output')
unlink('output/*')

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


all_urls <- read.csv('country_links.csv') %>% 
  pull(1) %>% 
  head(10)

## Simple FOR Loop ----

res_all <- tibble()

for(i in 1:length(all_urls)){
  print('---------')
  print(i)
  url_i <- all_urls[i]
  print(url_i)
  res_i <- get_ngramms(url_i)
  
  res_all <- res_all %>% 
    bind_rows(res_i)
}


## parallel computing code - using optimized function ----

library(doSNOW)
# https://cran.r-project.org/web/packages/doSNOW/index.html


all_urls <- read.csv('country_links.csv') %>% 
  pull(1)

tic()
get_ngramms(all_urls[1])
toc()

tic()
cl <- makeCluster(6) 
registerDoSNOW(cl)

result <- foreach(i = all_urls, 
                  .packages=c('dplyr', 'tidyr', 'tibble', 'rvest', 'tidytext'),
                  .errorhandling="stop") %dopar%
  {
    get_ngramms(i)
  }

stopCluster(cl)
toc()

# Combine afterwards
result <- result %>% 
  purrr::reduce(bind_rows)

## parallel computing code - using error handling function ----

all_urls2 <- c(all_urls[1:10], 'abcd')

library(doSNOW)

tic()
cl <- makeCluster(3) 
registerDoSNOW(cl)

res_stop <- foreach(i = all_urls2, 
                      .packages=c('dplyr', 'tidyr', 'tibble', 'rvest', 'tidytext'),
                      .errorhandling="stop") %dopar%
  {
    get_ngramms(i)
  }


res_remove <- foreach(i = all_urls2, 
                      .packages=c('dplyr', 'tidyr', 'tibble', 'rvest', 'tidytext'),
                      .errorhandling="remove") %dopar%
  {
    get_ngramms(i)
  }

res_pass <- foreach(i = all_urls2, 
                    .packages=c('dplyr', 'tidyr', 'tibble', 'rvest', 'tidytext'),
                    .errorhandling="pass") %dopar%
  {
    get_ngramms(i)
  }

res_pass %>% 
  purrr::reduce(bind_rows)

stopCluster(cl)
toc()


## Progress bar -----

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


unlink('output/*')

tic()
cl <- makeCluster(10) 
registerDoSNOW(cl)

iterations <- length(all_urls)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

result <- foreach(i = all_urls, 
                  .packages=c('dplyr', 'tidyr', 'tibble', 'rvest', 'tidytext'),
                  .options.snow = opts,
                  .errorhandling="stop") %dopar%
  {
    get_ngramms_silent(i)
  }

close(pb)
stopCluster(cl)
toc()