library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(wesanderson)
library(stringr)

GSP_Acu_ADS  <- ads

str(GSP_Acu_ADS)

GSP_Acu_ADS$anuncio <- tolower(GSP_Acu_ADS$Image_ad_name)
GSP_Acu_ADS$motivo <- str_extract(GSP_Acu_ADS$anuncio, "concierto|viaje|pareja|promo|bebe|arequipa|viaje cuzco|papa")

GSP_ads <- GSP_Acu_ADS %>%
  select(Month,anuncio,motivo,Impressions,Clicks,Cost,Conversions) %>%
  mutate(CTR = round(Clicks/Impressions,3),
         TC = round(Conversions/Clicks,3),
         Month = as.character.Date(Month)) %>%
  arrange(-Conversions)


motivos <- GSP_ads %>%
  group_by(Month, motivo) %>%
  summarise(Impresiones = sum(Impressions), 
            Clicks = sum(Clicks), 
            Inversion = sum(Cost), 
            Conversiones= sum(Conversions)) %>%
  mutate(TC=round(Conversiones/Clicks,3),
         ctr = round(Clicks/Impresiones,3)) %>%
  data.frame()

motivos %>% 
  filter(Month == "7") %>%
  arrange(-Conversiones)
