library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(wesanderson)
library(stringr)

GSP_acumulado <- FR 

str(motivos)

GSP_acumulado$anuncio <- tolower(GSP_acumulado$Image.ad.name)
GSP_acumulado$motivo <- str_extract(GSP_acumulado$anuncio, "bebe|papa|familia|eurotrip|rusia|boda|mudanza|reencuentro|remodelación")

GSP <- GSP_acumulado %>%
  select(Month,anuncio,motivo,Impressions,Clicks,Cost,Conversions) %>%
  mutate(CTR = round(Clicks/Impressions,3),
         TC = round(Conversions/Clicks,3),
         Month = as.character.Date(Month))

motivos <- GSP %>%
  group_by(Month, motivo) %>%
  summarise(Impresiones = sum(Impressions), 
            Clicks = sum(Clicks), 
            Inversion = sum(Cost), 
            Conversiones= sum(Conversions)) %>%
  mutate(TC=round(Conversiones/Clicks,3),
         ctr = round(Clicks/Impresiones,3)) %>%
  data.frame()

mensual <- GSP %>%
  group_by(Month) %>%
  summarise(Impresiones = sum(Impressions), 
            Clicks = sum(Clicks), 
            Inversion = sum(Cost), 
            Conversiones= sum(Conversions)) %>%
  mutate(TC=round(Conversiones/Clicks,3),
         ctr = round(Clicks/Impresiones,2)) %>%
  data.frame()

motivos %>% 
 filter(Month == "7") %>%
 arrange(-Conversiones)

####Solicitudes por motivos
j <- ggplot(motivos, aes(reorder(motivo, -ctr),ctr)) 
j + geom_bar(width=0.7, stat = "identity") + theme_light()

