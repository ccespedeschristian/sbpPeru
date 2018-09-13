library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(stringr)
ga_auth()

scotiabank <- 86666711

#anuncios de facebook
segment_Savings <- "gaid::DH9DCJTZTJG9qZD0H37C2g"
seg_S_ALL <- segment_ga4("Savings", segment_id = segment_Savings)
FacebookData <- google_analytics(scotiabank,
                               date_range = c("2018-01-01","2018-05-31"),
                               metrics = c("sessions", "calcMetric_SavingsLeads","goal8Completions", "goal9Completions", "goal6Completions", "goal7Completions"),
                               dimensions = c("month", "sourceMedium", "campaign", "adContent","keyword"),
                               segments = seg_S_ALL,
                               anti_sample = TRUE)


my_font <- function() {
  theme(text=element_text(size=16))
}
color_savings <- function(){
  scale_fill_brewer(palette = "Purples")
}
color_savings1 <- function(){
  scale_fill_brewer(palette = "OrRd")
}


#analisis de motivos &  formato
anunciosFB <- FacebookData %>% 
  filter(sourceMedium %in% c("facebook / cpa","instagram / cpa"),campaign == "savings_aon_conversiones") %>%
  mutate(solicitudTotal=calcMetric_SavingsLeads,Travel=round(goal7Completions,0),Free=round(goal6Completions,0), Power=round(goal8Completions,0), Wizard=round(goal9Completions,0), tasaConversion=round(solicitudTotal/sessions,2)) %>%
  select(month, adContent,sessions, solicitudTotal, tasaConversion) %>%
  filter(!solicitudTotal == 0) %>%
  arrange(-solicitudTotal) 

anunciosFB$Anuncio <- str_replace_all(anunciosFB$adContent, c("Free" = "free", "Travel" = "travel"))
anunciosFB$formato <- str_extract(anunciosFB$Anuncio, "canvas|ppl|multiproducto")
anunciosFB$Fcreatividad <- str_extract(anunciosFB$Anuncio, "estatico|video")
anunciosFB$Producto <-  str_extract(anunciosFB$Anuncio, "free|generico|travel|power|sueldo")
anunciosFB$Motivo <- str_extract(anunciosFB$adContent, "1|2|3")




format.global <-  anunciosFB %>% 
  group_by(formato, month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(solicitudTotal)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1))
  
Fcreatividad.global <- anunciosFB %>% 
  group_by(Fcreatividad, month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(solicitudTotal)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1))
  
Producto.global <- anunciosFB %>% 
group_by(Producto, month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(solicitudTotal)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1))

free <- Producto.global %>%
  filter(Producto == "free") %>%
  mutate(Cambio_Solicitud =round((Solicitudes/lag(Solicitudes))-1,2))
         
generico <- Producto.global %>%
  filter(Producto == "generico") %>%
  mutate(Cambio_Solicitud =round((Solicitudes/lag(Solicitudes))-1,2))

power <- Producto.global %>%
  filter(Producto == "power") %>%
  mutate(Cambio_Solicitud =round((Solicitudes/lag(Solicitudes))-1,2))
         
travel <-  Producto.global %>%
  filter(Producto == "travel") %>%
  mutate(Cambio_Solicitud =round((Solicitudes/lag(Solicitudes))-1,2))

anunciosFB %>%
  filter(Producto == "power") %>%
  group_by(formato, month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(solicitudTotal)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1)) 

anunciosFB %>%
  filter(Producto == "travel") %>%
  count(formato)

  



#=================================================================================================================
setwd("/Users/Invitado/Desktop/R Scripts")
l <- list("anunciosFree"= anunciosFree, "anunciosTRAVEL"=anunciosTRAVEL, "anunciosGENERICO" = anunciosGENERICO, "anunciosPOWER" = anunciosPOWER)
write.xlsx(l, file = "anuncios - savings.xlsx")
getwd()
