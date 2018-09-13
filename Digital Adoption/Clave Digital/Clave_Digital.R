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
                             dimensions = c("month", "sourceMedium"),
                             segments = seg_Clave_Digital,
                             anti_sample = TRUE)

paidMedia <- dailyAll %>%
  filter(sourceMedium %in% c("facebook / cpa", "google / cpc", "instagram / cpa")) %>%
  mutate(Afiliacion=goal17Completions) %>%
  group_by(sourceMedium) %>%
  summarise(Visitas=sum(sessions), Afiliacion=sum(Afiliacion)) %>%
  mutate(tasaConversion=round(Afiliacion/Visitas,3))

#Solicitudes TOTALES por canales de trafico
C1 <- ggplot(paidMedia, aes(sourceMedium,Afiliacion, fill = month)) 
C1 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light()  + 
  geom_text(aes(label = Afiliacion), position = position_dodge(0.7), vjust= -1)


