library(readr)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(lubridate)
library(googleAnalyticsR)
ga_auth()

scotiabank <- 86666711

#anuncios de facebook Power
segment_Savings <- "gaid::DH9DCJTZTJG9qZD0H37C2g"
seg_S_ALL <- segment_ga4("Savings", segment_id = segment_Savings)
FacebookData <- google_analytics(scotiabank,
                                 date_range = c("2018-08-01","2018-08-31"),
                                 metrics = c("sessions", "goal8Completions"),
                                 dimensions = c("month", "sourceMedium", "campaign", "adContent","keyword"),
                                 segments = seg_S_ALL,
                                 anti_sample = TRUE)

#analisis de motivos &  formato power
anunciosFB.power <- FacebookData %>% 
  filter(sourceMedium %in% c("facebook / cpa","instagram / cpa", "facebook / reach", "facebook / cpc"),
         campaign == "savings_aon_conversiones") %>%
  mutate(Power=round(goal8Completions,0), 
         TtasaConversion=round(Power/sessions,2)) %>%
  select(month, adContent, Power, TtasaConversion) %>%
  mutate(formato = str_extract(anunciosFB.power$adContent, "canvas|ppl|multiproducto"),
         producto = str_extract(anunciosFB.power$adContent, "power|travel|free")) %>% 
  filter(producto == "power")





#RESULTADO MENSUAL
anunciosFB.power %>% 
  group_by(month) %>%
  summarise(Visitas=sum(sessions), PSolicitudes = sum(Power)) %>%
  mutate(tasa=round(PSolicitudes/Visitas*100,1))