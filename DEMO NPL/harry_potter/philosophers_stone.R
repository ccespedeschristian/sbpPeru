library(tidyverse)   
library(stringr)     
library(tidytext)    
library(harrypotter) 
library(dpylr)

text_tb <- tibble(chapter = seq_along(philosophers_stone),
                  text = philosophers_stone)

bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")

book1 <- text_tb %>% 
  unnest_tokens(word, text)

Philo_sentimient <- book1 %>%
  inner_join(bing) %>% 
  count(chapter, word, sentiment)

Philo_sentimient %>% 
  group_by(chapter) %>% 
  mutate(total = sum(n),
         percent = n/ total) %>%
  arrange(-percent) %>%
  top_n(10) %>%
  ungroup() %>% 
  mutate(word = reorder(word, n))
  
  


book1 %>%
  count(word, sort = TRUE) %>%
  inner_join(get_sentiments("afinn")) %>%
  filter(score < 0)
  