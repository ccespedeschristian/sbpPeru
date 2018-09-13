library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)
ga_auth()

scotiabank <- 86666711


#anuncios de facebook
segment_ADS <- "gaid::Z9LFageBRcKSs4EDC-qDTg"
seg_ADS_ALL <- segment_ga4("Adelanto Sueldo", segment_id = segment_ADS)
FacebookData <- google_analytics(scotiabank,
                                   date_range = c("2018-08-01","2018-08-31"),
                                   metrics = c("sessions","goal3Completions"),
                                   dimensions = c("month", "sourceMedium", "campaign", "adContent","keyword"),
                                   segments = seg_ADS_ALL,
                                   anti_sample = TRUE)


facebook.data <- FacebookData %>%
  filter(sourceMedium == "facebook / cpa",campaign == "adelantosueldo_aon_conversiones") %>%
  mutate(Solicitudes=goal3Completions,tasaConversion=round(Solicitudes/sessions,2)) %>%
  select(month, adContent,sessions, Solicitudes, tasaConversion) %>%
  filter(!Solicitudes == 0) %>%
  arrange(-Solicitudes)

facebook.data$formato <- str_extract(facebook.data$adContent, "ppl|multiproducto|canvas|livephoto")
facebook.data$Fcreatividad <- str_extract(facebook.data$adContent, "estatico|video")
facebook.data$Creatividad <- str_extract(facebook.data$adContent, "arequipa|orejas|concierto|pareja|viajecusco|viaje|papa|promo")


formato.FB <- facebook.data %>%
  group_by(formato, month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1))

Creatividad.FB <- facebook.data %>%
  group_by(Creatividad, month) %>%
  summarise(
    Visitas=sum(sessions), 
    Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas,1)) %>%
  data.frame()

#volumen de leads por formato
x8 <- ggplot(Creatividad.FB, aes(reorder(Creatividad, -Solicitudes),Solicitudes, fill = month))
x8 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1)


#volumen de leads por formato
x81 <- ggplot(Creatividad.FB, aes(reorder(Creatividad, -tasa),tasa, fill = month))
x81 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + geom_text(aes(label = tasa), position = position_dodge(0.7), vjust= -1)



#Tasa de Conversion por formato
x7 <- ggplot(formato.FB, aes(reorder(formato, -tasa),tasa, fill = month))
x7 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + geom_text(aes(label = tasa), position = position_dodge(0.7), vjust= -1)


#Performance de PPL
PPL <-  facebook.data %>%
  filter(formato == "ppl") %>%
  group_by(Creatividad, month)%>%
  select(month, Creatividad, sessions, Solicitudes, tasaConversion) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes), tasa=round(Solicitudes/Visitas*100,1))

#Performance de PPL
PPL1 <-  facebook.data %>%
  filter(formato == "ppl") %>%
  group_by(Creatividad1, month)%>%
  select(month, Creatividad, sessions, Solicitudes, tasaConversion) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes), tasa=round(Solicitudes/Visitas*100,1))


#Tasa de Conversion por formato
x9 <- ggplot(PPL1, aes(reorder(Creatividad1, -tasa),tasa, fill = month))
x9 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + geom_text(aes(label = tasa), position = position_dodge(0.7), vjust= -1)

  

