library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(stringr)
ga_auth()

my_segments <- ga_segment_list()
segs <- my_segments$items

scotiabank <- 86666711

segment_clave_digital <- "gaid::tph9INNtQcGGaqjALdsg2A"
seg_Clave_Digital <- segment_ga4("Clave Digital", segment_id = segment_clave_digital)
dailyAll <- google_analytics(scotiabank,
                             date_range = c("2018-05-01","2018-06-30"),
                             metrics = c("sessions","goal17Completions"),
                             dimensions = c("month", "sourceMedium", "campaign", "adContent", "keyword"),
                             segments = seg_Clave_Digital,
                             anti_sample = TRUE)

Ads_claveDigital <- dailyAll %>%
  filter(campaign == "clave_digital_aon_conversiones", !goal17Completions == 0) %>%
  mutate(Afiliacion= goal17Completions, tasaConversion=round(Afiliacion/sessions,2)) %>%
  select(adContent,sessions, Afiliacion,tasaConversion) %>% 
  arrange(-Afiliacion) 

Ads_claveDigital$Formato <- str_extract(Ads_claveDigital$adContent, "ppl|stories|multiproducto")
Ads_claveDigital$CreativeAds <- str_extract(Ads_claveDigital$adContent, "estatico|video")
Ads_claveDigital$Motivo <- str_extract(Ads_claveDigital$adContent, "1|2|3|4|5")

claveDigital_Formato <- Ads_claveDigital %>%
  group_by(Formato) %>%
  summarise(Visitas=sum(sessions), Solicitudes = sum(Afiliacion), tasaConversion=Solicitudes/Visitas)%>%
  arrange(-Solicitudes)

Ads_claveDigital %>%
  group_by(CreativeAds) %>%
  summarise(Visitas=sum(sessions), Solicitudes = sum(Afiliacion), tasaConversion=Solicitudes/Visitas)%>%
  arrange(-Solicitudes)

claveDigital_Motivo <- Ads_claveDigital %>%
  group_by(Motivo) %>%
  summarise(Visitas=sum(sessions), Solicitudes = sum(Afiliacion), tasaConversion=Solicitudes/Visitas)%>%
  arrange(-Solicitudes)

Ads_claveDigital %>%
  count(Formato)


#GRAFICOS
#funciones
my_color <- function() {
  scale_fill_brewer()
}

#resultados por formato
c11 <- ggplot(claveDigital_Formato, aes(Formato,Solicitudes)) 
c11 + geom_bar(width=0.7, stat = "identity") + theme_light()


#resultados por Motivo

