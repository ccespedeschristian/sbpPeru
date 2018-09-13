library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
ga_auth()

scotiabank <- 86666711

my_segments <- ga_segment_list()
segs <- my_segments$items

segment_Savings <- "gaid::DH9DCJTZTJG9qZD0H37C2g"
seg_S_ALL <- segment_ga4("Savings", segment_id = segment_Savings)
dailyAll <- google_analytics_4(scotiabank,
                               date_range = c("2018-01-01","yesterday"),
                               metrics = c("sessions","goal8Completions", "goal9Completions", "goal6Completions", "goal7Completions"),
                               dimensions = c("month", "sourceMedium"),
                               segments = seg_S_ALL,
                               anti_sample = TRUE)

dailyAll$fuente <- str_extract(dailyAll$sourceMedium, "(none)|organic")

OD <- dailyAll %>% filter(!fuente ==  "NA") %>% 
  mutate(Travel=goal7Completions,Free=goal6Completions, Power=goal8Completions, Wizard=goal9Completions, solicitudTotal=Travel+Free+Power+Wizard, tasaConversion=round(solicitudTotal/sessions,2)) %>%
  select(fuente, month,sessions,solicitudTotal) %>%
  group_by(fuente, month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(solicitudTotal), tasaConversion = Solicitudes/Visitas)

k3 <- ggplot(OD, aes(month,Solicitudes, fill = fuente)) 
k3 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + 
  geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1) 
