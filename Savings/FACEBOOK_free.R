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

#anuncios de facebook FREE
segment_Savings <- "gaid::DH9DCJTZTJG9qZD0H37C2g"
seg_S_ALL <- segment_ga4("Savings", segment_id = segment_Savings)
FacebookData <- google_analytics(scotiabank,
                                 date_range = c("2018-08-01","2018-08-31"),
                                 metrics = c("sessions", "goal6Completions"),
                                 dimensions = c("month", "sourceMedium", "campaign", "adContent","keyword"),
                                 segments = seg_S_ALL,
                                 anti_sample = TRUE)

#analisis de motivos &  formato free
anunciosFB.free <- FacebookData %>% 
  filter(sourceMedium %in% c("facebook / cpa","instagram / cpa", "facebook / reach", "facebook / cpc"),
         campaign == "savings_aon_conversiones") %>%
  mutate(Free=round(goal6Completions,0), 
         FtasaConversion=round(Free/sessions,2)) %>% 
  select(month, adContent, Free, FtasaConversion) %>%
  mutate(formato = str_extract(anunciosFB.free$adContent, "canvas|ppl|multiproducto"),
         producto = str_extract(anunciosFB.free$adContent, "power|travel|free")) %>% 
  filter(producto == "free")


str_extract(anunciosFB.free$adContent, "canvas|ppl|multiproducto")

#RESULTADO MENSUAL
anunciosFB.free %>% 
  group_by(month) %>%
  summarise(Visitas=sum(sessions), FSolicitudes = sum(Free)) %>%
  mutate(tasa=round(FSolicitudes/Visitas*100,1))



#resultados de Enero a Marzo Anuncios Free
Free1.formato <- anunciosFB.free %>%
  filter(month %in% c("01", "02", "03")) %>%
  select(month,formato, sessions, Free, FtasaConversion) 

Free1.formato %>%
  group_by(formato) %>%
  summarise(Visitas=sum(sessions), FSolicitudes = sum(Free)) %>%
  mutate(tasa=round(FSolicitudes/Visitas*100,1))

Free2.formato <- anunciosFB.free %>%
  filter(month %in% c("04", "05")) %>%
  select(month,formato, sessions, Free, FtasaConversion) 

Free2.formato %>%
  group_by(formato) %>%
  summarise(Visitas=sum(sessions), FSolicitudes = sum(Free)) %>%
  mutate(tasa=round(FSolicitudes/Visitas*100,1))




Free1.formato %>%
  count(formato)



