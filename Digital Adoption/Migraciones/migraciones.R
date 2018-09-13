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
                             date_range = c("2018-07-30","2018-08-26"),
                             metrics = c("sessions","goal5Completions"),
                             dimensions = c("sourceMedium"),
                             segments = seg_migraciones,
                             anti_sample = TRUE)

paidMedia <- dailyAll %>%
  filter(sourceMedium %in% c("facebook / cpa", 
                             "google / cpc", 
                             "instagram / cpa", 
                             "facebook / reach", 
                             "websitecard_twitter / websitecard_twitter",
                             "instagram / reach",
                             "dbm / cpm")) %>%
  mutate(Afiliacion=goal5Completions) %>%
  group_by(sourceMedium) %>%
  summarise(Visitas=sum(sessions), 
            Afiliacion=sum(Afiliacion)) %>%
  mutate(tasaConversion=round(Afiliacion/Visitas,3)) %>%
  data.frame()

dailyAll %>%
  filter(sourceMedium %in% c("facebook / cpa", 
                             "google / cpc", 
                             "instagram / cpa", 
                             "facebook / reach", 
                             "websitecard_twitter / websitecard_twitter",
                             "instagram / reach", 
                             "dbm / cpm")) %>%
  mutate(Afiliacion=goal5Completions) %>%
  summarise(Visitas=sum(sessions), 
            Afiliacion=sum(Afiliacion)) %>%
  mutate(tasaConversion=round(Afiliacion/Visitas,3))




#Solicitudes TOTALES por canales de trafico
C1 <- ggplot(paidMedia, aes(sourceMedium,Afiliacion, fill = month)) 
C1 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light()  + 
  geom_text(aes(label = Afiliacion), position = position_dodge(0.7), vjust= -1)


