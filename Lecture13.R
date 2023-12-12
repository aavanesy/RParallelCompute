
# Coding Session: Text Visualization with wordcloud

library(wordcloud)
library(wordcloud2)
library(dplyr)

load('res_map_future.Rdata')

data <- res_map_future %>% 
  purrr::reduce(dplyr::bind_rows) %>% 
  rename(word = 1, freq = 2) %>% 
  summarise(freq = sum(freq), .by = word) %>% 
  arrange(desc(freq)) %>% 
  filter(!grepl('parser', word)) %>% 
  filter(!grepl('0px', word))

set.seed(1234) # for reproducibility 
wordcloud(words = data$word,
          freq = data$freq,
          min.freq = 1,
          max.words = 100,
          random.order=FALSE,
          # rot.per=0.35,
          # scale=c(3.5,0.25),
          colors=brewer.pal(8, "Dark2"))

