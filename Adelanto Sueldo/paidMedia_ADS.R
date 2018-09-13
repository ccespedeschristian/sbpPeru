library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
ga_auth()

scotiabank <- 86666711

segment_Savings <- "gaid::Z9LFageBRcKSs4EDC-qDTg"
seg_ADS_ALL <- segment_ga4("Adelanto Sueldo", segment_id = segment_Savings)
dailyAll <- google_analytics(scotiabank,
                               date_range = c("2018-06-01","yesterday"),
                               metrics = c("sessions","goal3Completions"),
                               dimensions = c("month", "sourceMedium"),
                               segments = seg_ADS_ALL,
                               anti_sample = TRUE)



paidMedia_ADS <- dailyAll %>% 
  filter(sourceMedium %in% c("facebook / cpa", "google / cpc")) %>%
  mutate(Solicitudes=goal3Completions, tasaConversion=round(Solicitudes/sessions,2)) %>%
  select(month,sourceMedium,sessions, Solicitudes, tasaConversion) %>%
  group_by(sourceMedium,month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasaConversion=round(Solicitudes/Visitas,3))


 dailyAll %>% 
  filter(!month == "06") %>%
  mutate(Solicitudes=goal3Completions, tasaConversion=round(Solicitudes/sessions,2)) %>%
  select(month,sourceMedium,sessions, Solicitudes, tasaConversion) %>%
  group_by(month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasaConversion=round(Solicitudes/Visitas,3))


my_color_ADS <- function() {
  scale_fill_brewer(palette = "BuGn") 
}
my_font <- function() {
  theme(text=element_text(size=16))
}

#Solicitudes TOTALES por canales de trafico POR SEMANA
aa <- ggplot(paidMedia_ADS, aes(sourceMedium,Solicitudes, fill = month)) 
aa + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + my_font() + my_color_ADS() + geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1)


#FACEBOOK
 facebookADS <- paidMedia_ADS %>% filter(sourceMedium == "facebook / cpa") %>%
   mutate(cambioLeads =round((Solicitudes/lag(Solicitudes))-1,2),cambioTasa =round((tasaConversion/lag(tasaConversion))-1,2))
 
 #GOOGLE ADWORDS
 GoogleADS <- paidMedia_ADS %>% filter(sourceMedium == "google / cpc") %>%
   mutate(cambioLeads =round((Solicitudes/lag(Solicitudes))-1,2),cambioTasa =round((tasaConversion/lag(tasaConversion))-1,2))
 
################## 
 setwd("/Users/Invitado/Desktop/R Scripts")
 l <- list("paidMedia_S"= paidMedia_S)
 write.xlsx(l, file = "Adelanto Sueldo - Raw Data.xlsx")
 getwd()