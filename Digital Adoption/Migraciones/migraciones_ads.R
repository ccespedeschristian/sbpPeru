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

segment_migraciones <- "gaid::JwNNLHDiTWm99hY-L1DUuw"
seg_migraciones <- segment_ga4("Migraciones", segment_id = segment_migraciones)
dailyAll <- google_analytics(scotiabank,
                             date_range = c("2018-07-30","2018-08-27"),
                             metrics = c("sessions","goal5Completions"),
                             dimensions = c("sourceMedium","campaign", "adContent", "keyword"),
                             segments = seg_migraciones,
                             anti_sample = TRUE)

adsMigraciones <- dailyAll %>%
  filter(campaign == "digital_adoption_aon_conversiones", 
         !goal5Completions == 0) %>%
  mutate(Afiliacion = goal5Completions, 
         tasaConversion = round(Afiliacion/sessions,2),
         Ponderado = Afiliacion*tasaConversion/100) %>%
  select(adContent,sessions, Afiliacion,tasaConversion,Ponderado) %>% 
  arrange(-Ponderado) %>%
  data.frame()


adsMigraciones$Formato <- str_extract(adsMigraciones$adContent, "ppl|stories|multiproducto")
adsMigraciones$CreativeAds <- str_extract(adsMigraciones$adContent, "estatico|video")
adsMigraciones$Motivo <- str_extract(adsMigraciones$adContent, "11|12|13|14|15|16|17|18")

adsMigraciones_Formato <- adsMigraciones %>%
  group_by(Formato) %>%
  summarise(Visitas=sum(sessions), Solicitudes = sum(Afiliacion), tasaConversion=Solicitudes/Visitas)%>%
  arrange(-Solicitudes) %>% 
  na.omit()


adsMigraciones %>%
  group_by(CreativeAds) %>%
  summarise(Visitas=sum(sessions), Solicitudes = sum(Afiliacion), tasaConversion=Solicitudes/Visitas)%>%
  arrange(-Solicitudes) %>% 
  na.omit()

adsMigraciones_Motivo <- adsMigraciones %>%
  group_by(Motivo) %>%
  summarise(Visitas=sum(sessions), Solicitudes = sum(Afiliacion), tasaConversion=Solicitudes/Visitas)%>%
  arrange(-Solicitudes) %>% 
  na.omit()

adsMigraciones %>% 
  filter(Motivo == "12") %>%
  group_by(Formato) %>%
  summarise(Visitas=sum(sessions), Solicitudes = sum(Afiliacion), tasaConversion=Solicitudes/Visitas)%>%
  arrange(-Solicitudes) %>% 
  na.omit()





#GRAFICOS
#funciones
my_color <- function() {
  scale_fill_brewer()
}

#resultados por formato
c11 <- ggplot(claveDigital_Formato, aes(Formato,Solicitudes)) 
c11 + geom_bar(width=0.7, stat = "identity") + theme_light()


#resultados por Motivo

