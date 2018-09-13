library(googlesheets)
library(dplyr)
library(tidytext)
library(ggplot2)

#my_sheets <- gs_ls()
#searchTerms <- gs_title("Search Terms")
#adsTerms <- searchTerms %>% gs_read(ws = "Terms", range = "A1:I1000")


adsTerms <- Search.Terms...Terms <- read.csv("C:/Users/s6114349/Downloads/Search Terms - Terms.csv")

branded <- adsTerms %>%
  filter(Campaign._name == "DF_SEM_AO_DigitalAdoption_Branded_Desktop", Impressions >= mean(adsTerms$Impressions)) %>%
  select(Ad_group_name, Matched_search_term, Match_type, Impressions, Conversions) %>%
  arrange(-Impressions)

generic <- adsTerms %>%
  filter(Campaign._name == "DF_SEM_AO_DigitalAdoption_Generic_Desktop", Impressions >= mean(adsTerms$Impressions)) %>%
  select(Ad_group_name, Matched_search_term, Match_type, Impressions, Conversions) %>%
  arrange(-Impressions)


branded %>%
  filter(Ad_group_name == "Brand_EnLinea", Match_type == "broad")


branded %>%
  filter(Ad_group_name == "Brand")








#GRÁFICOS
ggplot(branded, aes(Impressions, Matched_search_term)) +
  geom_tile(aes(Ad_group_name)) 

ggplot(generic, aes(Impressions, Matched_search_term)) +
  geom_tile(aes(Ad_group_name)) 
