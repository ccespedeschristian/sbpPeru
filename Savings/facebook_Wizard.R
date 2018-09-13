library(readr)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(lubridate)
#Wizard Facebook CTR
wizard.data <- read.csv("C:/Users/s6114349/Desktop/SAVINGS DATA/wizard.csv")

wizard.data$formato <- str_extract(wizard.data$Ad_name, "ppl|multiproducto")
wizard.data$creatividad <- str_extract(wizard.data$Ad_name, "video|generico")

wizard.a <- wizard.data %>%
    select(Month, formato, creatividad, Reach, Impressions, Clicks) %>%
    group_by(formato) %>%
    summarise(Impresiones = sum(Impressions), Clicks = sum(Clicks),Alcance = sum(Reach)) %>%
    mutate(Frecuencia = round(Impresiones/Alcance,2), CTR = round(Clicks/Impresiones*100,2))
  


#Wizard Facebook leads

#anuncios de facebook Wizard
segment_Savings <- "gaid::DH9DCJTZTJG9qZD0H37C2g"
seg_S_ALL <- segment_ga4("Savings", segment_id = segment_Savings)
FacebookData <- google_analytics(scotiabank,
                                 date_range = c("2018-01-01","2018-05-31"),
                                 metrics = c("sessions", "goal9Completions"),
                                 dimensions = c("month", "sourceMedium", "campaign", "adContent","keyword"),
                                 segments = seg_S_ALL,
                                 anti_sample = TRUE)

#analisis de motivos &  formato Wizard 
anunciosFB.wizard <- FacebookData %>% 
  filter(sourceMedium %in% c("facebook / cpa","instagram / cpa"),campaign == "savings_aon_conversiones") %>%
  mutate(Wizard=round(goal9Completions,0), WtasaConversion=round(Wizard/sessions,2)) %>%
  select(month, adContent,sessions, Wizard, WtasaConversion) %>%
  arrange(-Wizard) 

anunciosFB.wizard$formato <- str_extract(anunciosFB.wizard$adContent, "canvas|ppl|multiproducto")
anunciosFB.wizard$Fcreatividad <- str_extract(anunciosFB.wizard$adContent, "generico|video")


anunciosFB.wizard %>% 
  group_by(month) %>%
  summarise(Visitas=sum(sessions), WSolicitudes = sum(Wizard)) %>%
  mutate(tasa=round(WSolicitudes/Visitas*100,1))


anunciosFB.wizard %>%
  group_by(formato) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Wizard)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1))


anunciosFB.wizard %>%
  group_by(Fcreatividad) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Wizard)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1)) 






wizardFormato %>% mutate( month = as.numeric(month) ) %>% 
  left_join(
    wizard.a %>% rename(month = Month)) 

#Solicitudes TOTALES por canales de trafico POR SEMANA
f2 <- ggplot(wizardCreatividad, aes(Fcreatividad,Solicitudes, fill = month)) 
f2 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + color_savings() + my_font() + geom_text(aes(label = Solicitudes, y= Solicitudes + 0.05), position = position_dodge(0.7), vjust= -1) 







