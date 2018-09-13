library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(stringr)
library(RColorBrewer)
ga_auth()

scotiabank <- 86666711

#anuncios de facebook
segment_Plazo_Online <- "gaid::fydeuTtUT8qllRaNDiRI2Q"
seg_PO_ALL <- segment_ga4("Plazo Online Landing", segment_id = segment_Plazo_Online)
FacebookData <- google_analytics(scotiabank,
                               date_range = c("2018-08-01","2018-08-31"),
                               metrics = c("sessions","goal2Completions"),
                               dimensions = c("month", "sourceMedium", "campaign", "adContent", "keyword"),
                               segments = seg_PO_ALL,
                               anti_sample = TRUE)

anunciosFB <- FacebookData %>%
  filter(campaign == "plazoonline_aon_conversiones", 
         !goal2Completions == 0) %>%
  mutate(Solicitudes= goal2Completions, 
         tasaConversion=round(Solicitudes/sessions,2)) %>%
  select(month, adContent, keyword,sessions, Solicitudes,tasaConversion) %>% 
  arrange(-Solicitudes) 

anunciosFB$Ad <- str_extract(anunciosFB$adContent, "vacaciones|eurotrip|reencuentro|
                             hawai|boda|rusia|mudanza|roadtrip|
                             caribe|asia|remodelacion|
                             remodelacion|embarazada|emabarazada")
anunciosFB$Creatividad <- str_extract(anunciosFB$adContent, "video|estatico")
anunciosFB$Formato <- str_extract(anunciosFB$adContent, "ppl|canvas|multiproducto|stories")
anunciosFB$Ciudad <- str_extract(anunciosFB$keyword, "lima|provincia")

em <- grep("em(.*)", anunciosFB$Ad , ignore.case = TRUE)
anunciosFB$Ad [em] <- "embarazada"


my_color_PO <- function() {
  scale_fill_brewer()
}

#analisis de anuncios
f <- anunciosFB %>%
  group_by(month, Ad) %>%
  summarise(Visitas = sum(sessions), Solicitudes = sum(Solicitudes)) %>%
  mutate(TC=round(Solicitudes/Visitas,3))

anunciosFB %>%
  group_by(month, Ad) %>%
  summarise(Visitas = sum(sessions), Solicitudes = sum(Solicitudes)) %>%
  mutate(Solicitudes/Visitas)

anunciosFB %>%
  group_by(month) %>%
  summarise(Visitas = sum(sessions), 
            Solicitudes = sum(Solicitudes)) %>%
  mutate(Solicitudes/Visitas)


f %>%
  filter(month == "05") %>%
  arrange(-Solicitudes)

anunciosFB %>%
  filter(month == "02") %>%
  group_by(month, Formato) %>%
  summarise(Solicitudes = sum(Solicitudes), Visitas = sum(sessions)) %>%
  mutate(Solicitudes/Visitas) %>%
  arrange(-Solicitudes)




t1 <- ggplot(f, aes(reorder(Ad, -Solicitudes), Solicitudes, Solicitudes, fill = month))  
t1 + geom_col(width=0.7, position = "dodge") + theme_light() 

#analisis de formato
f1 <- anunciosFB %>%
  group_by(month, Formato) %>%
  summarise(Visitas = sum(sessions), 
             Solicitudes = sum(Solicitudes)) %>%
              mutate(Solicitudes/Visitas)




t2 <- ggplot(f1, aes(reorder(Formato, -Solicitudes), Solicitudes, fill = month))  
t2 + geom_col(width=0.7, position = "dodge") + theme_light() 

#analisis de ciudad
f2 <- anunciosFB %>%
  na.omit %>%
  group_by(month, Ciudad) %>%
  summarise(Solicitudes = sum(Solicitudes))

t3 <- ggplot(f2, aes(reorder(Ciudad, -Solicitudes), Solicitudes, fill = month))  
t3 + geom_col(width=0.7, position = "dodge") + theme_light() + 
  geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1) 


t2 <- ggplot(anunciosFB, aes(Formato, tasaConversion,fill = month))  
t2 + geom_col(width=0.7,position = "dodge") + theme_light()



#=================================================================================================================
setwd("C:/Users/s6114349/Desktop/Christian/Reportes/R Scripts/Plazo Online")
l <- list("FacebookAds"= FacebookAds)
write.xlsx(l, file = "Facebook Ads - Plazo Online.xlsx")
getwd()
