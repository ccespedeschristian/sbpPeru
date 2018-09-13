library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
ga_auth()

scotiabank <- 86666711

segment_xtralinea <- "gaid::LOx95bmgRuq5Mm8OslTkig"
seg_Xtralinea <- segment_ga4("Xtralinea", segment_id = segment_xtralinea)
dailyAll <- google_analytics(scotiabank,
                             date_range = c("2018-05-01","yesterday"),
                             metrics = c("sessions","goal16Completions"),
                             dimensions = c("month", "sourceMedium"),
                             segments = seg_Xtralinea,
                             anti_sample = TRUE)

paidMedia <- dailyAll %>% filter(sourceMedium %in% c("facebook / cpa", "google / cpc", "instagram / cpa")) %>%
  mutate(Solicitudes = goal16Completions) %>%
  group_by(sourceMedium, month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasaConversion=round(Solicitudes/Visitas,3))

t <- paidMedia %>%
  filter(sourceMedium %in% c("facebook / cpa", "instagram / cpa")) %>%
  group_by(month) %>%
  summarise(Visitas=sum(Visitas), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasaConversion=round(Solicitudes/Visitas,3))

sum(paidMedia$Visitas)
#Solicitudes TOTALES por canales de trafico
x1 <- ggplot(paidMedia, aes(sourceMedium,Solicitudes, fill = month)) 
x1 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + 
  geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1)