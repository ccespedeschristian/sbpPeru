library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(stringr)
ga_auth()

scotiabank <- 86666711


segment_Plazo_Online <- "gaid::fydeuTtUT8qllRaNDiRI2Q"
seg_PO_ALL <- segment_ga4("Plazo Online Landing", segment_id = segment_Plazo_Online)
dailyAll <- google_analytics(scotiabank,
                               date_range = c("2018-01-01","yesterday"),
                               metrics = c("sessions","goal2Completions"),
                               dimensions = c("month", "sourceMedium"),
                               segments = seg_PO_ALL,
                               anti_sample = TRUE)

dailyAll$fuente <- str_extract(dailyAll$sourceMedium, "(none)|organic")

OD <- dailyAll %>% filter(!fuente ==  "NA") %>% 
  mutate(Solicitudes = goal2Completions) %>%
  select(fuente, month,sessions,Solicitudes) %>%
  group_by(fuente, month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes), tasaConversion = Solicitudes/Visitas)


k3 <- ggplot(OD, aes(month,Solicitudes, fill = fuente)) 
k3 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + 
  geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1) 

