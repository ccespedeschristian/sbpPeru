library(readr)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(lubridate)
ga_auth()

scotiabank <- 86666711

#anuncios de facebook TRavel
segment_Savings <- "gaid::DH9DCJTZTJG9qZD0H37C2g"
seg_S_ALL <- segment_ga4("Savings", segment_id = segment_Savings)
FacebookData <- google_analytics(scotiabank,
                                 date_range = c("2018-08-01","2018-08-31"),
                                 metrics = c("sessions", "goal7Completions"),
                                 dimensions = c("month", "sourceMedium", "campaign", "adContent","keyword"),
                                 segments = seg_S_ALL,
                                 anti_sample = TRUE)

#analisis de motivos &  formato travel
anunciosFB.travel <- FacebookData %>% 
  filter(sourceMedium %in% c("facebook / cpa","instagram / cpa","facebook / reach","facebook / cpc"),
         campaign == "savings_aon_conversiones") %>%
  mutate(Travel=round(goal7Completions,0), 
         TtasaConversion=round(Travel/sessions,2)) %>% 
  select(month, adContent, Travel, TtasaConversion) %>%
  mutate(formato = str_extract(anunciosFB.travel$adContent, "canvas|ppl|multiproducto"),
         producto = str_extract(anunciosFB.travel$adContent, "power|travel|free")) %>% 
  filter(producto == "travel")
  

anunciosFB.travel$formato <- str_extract(anunciosFB.travel$adContent, "canvas|ppl|multiproducto")

mean(anunciosFB.travel$Travel)

#RESULTADO MENSUAL
anunciosFB.travel %>% 
  group_by(month) %>%
  summarise(Visitas=sum(sessions), FSolicitudes = sum(Travel)) %>%
  mutate(tasa=round(FSolicitudes/Visitas*100,1))

