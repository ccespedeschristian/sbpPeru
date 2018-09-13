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
segment_DA <- "gaid::_GqIZjcjTDm0vdz7hp_9HA"
seg_DA_ALL <- segment_ga4("Adopcion Digital", segment_id = segment_DA)
FacebookData <- google_analytics(scotiabank,
                               date_range = c("2018-03-05","yesterday"),
                               metrics = c("sessions","goal12Completions"),
                               dimensions = c("month", "sourceMedium", "campaign", "adContent","keyword"),
                               segments = seg_DA_ALL,
                               anti_sample = TRUE)

#EFICIENCIA DE BBDD
BBDD <- FacebookData %>%
  filter(sourceMedium %in% c("facebook / cpa", "instagram / cpa"), campaign == "digital_adoption_aon_conversiones") %>%
  select(keyword,sessions,goal12Completions) %>%
  group_by(keyword)%>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(goal12Completions)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1)) %>%
  filter(!Solicitudes == 0, Visitas > 400) %>%
  arrange(-tasa)

#volumen de leads por BBDD
x111 <- ggplot(BBDD, aes(reorder(keyword, -Solicitudes),Solicitudes))
x111 + geom_bar(width=0.7, stat = "identity", fill = "#0FC677") + theme_light() + geom_text(aes(label = Solicitudes), vjust= -1) + my_font()

#tasa de conversion por BBDD
x222 <- ggplot(BBDD, aes(reorder(keyword, -tasa),tasa))
x222 + geom_bar(width=0.7, stat = "identity", fill = "#290736") + theme_light() + geom_text(aes(label = tasa), vjust= -1) + my_font()


#FORMATOS
formatoGlobal <- dispositivoFB %>%
  select(Formato,sessions,Solicitudes,tasaConversion) %>%
  group_by(Formato) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1)) %>%
  arrange(-tasa)

my_font <- function() {
  theme(text=element_text(size=16))
}

#volumen de leads
x11 <- ggplot(formatoGlobal, aes(reorder(Formato, -Solicitudes),Solicitudes))
x11 + geom_bar(width=0.7, stat = "identity", fill = "#179AA7") + theme_light() + geom_text(aes(label = Solicitudes), vjust= -1) + my_font()

#tasa de conversion
x22 <- ggplot(formatoGlobal, aes(reorder(Formato, -tasa),tasa))
x22 + geom_bar(width=0.7, stat = "identity", fill = "#EC7E22") + theme_light() + geom_text(aes(label = tasa), vjust= -1) + my_font()


#CREATIVIDAD DE LOS ANUNCIOS
Fcreatividad_Global <- dispositivoFB %>%
  separate(adContent, c("Formato", "Producto", "Fcreatividad", "Dispositivo"), sep = "_") %>%
  select(Formato,Fcreatividad, sessions, Solicitudes) %>%
  group_by(Fcreatividad) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1)) %>%
  arrange(-tasa)


#PERFORMANCE DE DISPOSITIVO POR FACEBOOK
dispositivoFB <- FacebookData %>%
filter(sourceMedium == "facebook / cpa", campaign == "digital_adoption_aon_conversiones") %>%
mutate(Solicitudes=goal12Completions,tasaConversion=round(Solicitudes/sessions,2)) %>%
select(adContent,sessions, Solicitudes, tasaConversion) %>%
arrange(-Solicitudes) 


dispositivoFB$Formato <- str_extract(dispositivoFB$adContent, "multiproducto|ppl|canvas|livephoto")
dispositivoFB$Dispositivo <- str_extract(dispositivoFB$adContent, "mobile|desktop")


formatoFacebook <- dispositivoFB %>%
  group_by(Formato) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1))




#RESULTADOS POR FORMATO - MOBILE
Formato_M <- dispositivoFB %>% filter(grepl("mobile",adContent), !Solicitudes == 0) %>% 
  separate(adContent, c("Formato", "Producto", "Fcreatividad", "Dispositivo"), sep = "_") %>%
  select(Formato,Fcreatividad,sessions,Solicitudes,tasaConversion) %>%
  group_by(Formato) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1)) %>%
  arrange(-tasa)



#volumen de leads
x1 <- ggplot(Formato_M, aes(reorder(Formato, -Solicitudes),Solicitudes))
x1 + geom_bar(width=0.7, stat = "identity", fill = "#5D737E") + theme_light() + geom_text(aes(label = Solicitudes), vjust= -1)

#tasa de conversion
x2 <- ggplot(Formato_M, aes(reorder(Formato, -tasa),tasa))
x2 + geom_bar(width=0.7, stat = "identity", fill = "#2AA899") + theme_light() + geom_text(aes(label = tasa), vjust= -1)


#RESULTADOS POR FORMATO DE LA CREATIVIDAD (VIDEO/ ESTATICO)
creatividad_M <- dispositivoFB %>% filter(grepl("mobile",adContent), !Solicitudes == 0) %>% 
  separate(adContent, c("Formato", "Producto", "Fcreatividad", "Dispositivo"), sep = "_") %>%
  select(Formato,Fcreatividad,sessions,Solicitudes,tasaConversion) %>%
  group_by(Fcreatividad) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1)) %>%
  arrange(-tasa)

#volumen de leads
x3 <- ggplot(creatividad_M, aes(reorder(Fcreatividad, -Solicitudes),Solicitudes))
x3 + geom_bar(width=0.7, stat = "identity", fill = "#5D737E") + theme_light() + geom_text(aes(label = Solicitudes), vjust= -1)

#tasa de conversion
x4 <- ggplot(creatividad_M, aes(reorder(Fcreatividad, -tasa),tasa))
x4 + geom_bar(width=0.7, stat = "identity", fill = "#2AA899") + theme_light() + geom_text(aes(label = tasa), vjust= -1)


#RESULTADOS POR FORMATO - DESKTOP
Formato_D <- dispositivoFB %>% filter(grepl("desktop",adContent), !Solicitudes == 0) %>% 
  separate(adContent, c("Formato", "Producto", "Fcreatividad", "Dispositivo"), sep = "_") %>%
  select(Formato,Fcreatividad,sessions,Solicitudes,tasaConversion) %>%
  group_by(Formato) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1)) %>%
  arrange(-tasa)

dispositivoFB %>% filter(grepl("desktop",adContent), !Solicitudes == 0) %>% 
  separate(adContent, c("Formato", "Producto", "Fcreatividad", "Dispositivo"), sep = "_") %>%
  select(Formato,Fcreatividad,sessions,Solicitudes,tasaConversion) %>%
  count(Formato)



#volumen de leads desktop
x5 <- ggplot(Formato_D, aes(reorder(Formato, -Solicitudes),Solicitudes))
x5 + geom_bar(width=0.7, stat = "identity", fill = "#2B616D") + theme_light() + geom_text(aes(label = Solicitudes), vjust= -1)

#tasa de conversion desktop
x6 <- ggplot(Formato_D, aes(reorder(Formato, -tasa),tasa))
x6 + geom_bar(width=0.7, stat = "identity", fill = "#2AA899") + theme_light() + geom_text(aes(label = tasa), vjust= -1)


#RESULTADOS POR FORMATO DE LA CREATIVIDAD (VIDEO/ ESTATICO)
creatividad_D <- dispositivoFB %>% filter(grepl("mobile",adContent), !Solicitudes == 0) %>% 
  separate(adContent, c("Formato", "Producto", "Fcreatividad", "Dispositivo"), sep = "_") %>%
  select(Formato,Fcreatividad,sessions,Solicitudes,tasaConversion) %>%
  group_by(Fcreatividad) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1)) %>%
  arrange(-tasa)


#INSTAGRAM
#PERFORMANCE DE DISPOSITIVO POR INSTAGRAM

instagramDATA <- FacebookData %>%
  filter(sourceMedium == "instagram / cpa",campaign == "digital_adoption_aon_conversiones") %>%
  mutate(Solicitudes=goal12Completions,tasaConversion=round(Solicitudes/sessions,2)) %>%
  select(adContent,sessions, Solicitudes, tasaConversion) %>%
  filter(!Solicitudes == 0)

instagramDATA$Formato <- str_extract(instagramDATA$adContent, "multiproducto|ppl|stories") 
instagramDATA$Fcreatividad <- str_extract(instagramDATA$adContent, "estatico|video")

formatoInstagram <- instagramDATA %>%
  group_by(Formato) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1))


my_font <- function() {
  theme(text=element_text(size=16))
}


#volumen de leads desktop
x7 <- ggplot(formatoInstagram, aes(reorder(Formato, -Solicitudes),Solicitudes))
x7 + geom_bar(width=0.7, stat = "identity", fill = "#2B616D") + theme_light() + geom_text(aes(label = Solicitudes), vjust= -1) + my_font()

#tasa de conversion desktop
x8 <- ggplot(formatoInstagram, aes(reorder(Formato, -tasa),tasa))
x8 + geom_bar(width=0.7, stat = "identity", fill = "#97EAD2") + theme_light() + geom_text(aes(label = tasa), vjust= -1) + my_font()


fcreatividad_Instagram <- instagramDATA %>%
  group_by(Fcreatividad) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1))

#volumen de leads desktop
x5 <- ggplot(Formato_D, aes(reorder(Formato, -Solicitudes),Solicitudes))
x5 + geom_bar(width=0.7, stat = "identity", fill = "#2B616D") + theme_light() + geom_text(aes(label = Solicitudes), vjust= -1)

#tasa de conversion desktop
x6 <- ggplot(Formato_D, aes(reorder(Formato, -tasa),tasa))
x6 + geom_bar(width=0.7, stat = "identity", fill = "#2B616D") + theme_light() + geom_text(aes(label = tasa), vjust= -1)





#=================================================================================================================
setwd("/Users/Invitado/Desktop/R Scripts")
l <- list(formatoGlobal = "formatoGlobal", Fcreatividad_Global = "Fcreatividad_Global")
write.xlsx(l, file = "anuncios - Digital Adoption.xlsx")
getwd()
