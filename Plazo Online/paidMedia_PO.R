library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
ga_auth()

scotiabank <- 86666711


segment_Plazo_Online <- "gaid::fydeuTtUT8qllRaNDiRI2Q"
seg_PO_ALL <- segment_ga4("Plazo Online Landing", segment_id = segment_Plazo_Online)
dailyAll <- google_analytics(scotiabank,
                               date_range = c("2018-08-01","2018-06-30"),
                               metrics = c("sessions","goal2Completions"),
                               dimensions = c("month", "sourceMedium"),
                               segments = seg_PO_ALL,
                               anti_sample = TRUE)


paidMedia <- dailyAll %>% filter(sourceMedium %in% c("facebook / cpa", "google / cpc", "instagram / cpa")) %>%
  mutate(Solicitudes=goal2Completions) %>%
  group_by(sourceMedium, month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasaConversion=round(Solicitudes/Visitas,3))
  

dailyAll %>%  group_by(month) %>% 
  summarise(Visitas=sum(sessions), Solicitudes=sum(goal2Completions))

paidMedia %>% group_by(month) %>% 
summarise(Visitas=sum(Visitas), Solicitudes=sum(Solicitudes))


my_color_paidMediaPO <- function() {
  scale_fill_brewer(palette = "Oranges")
}
my_font <- function() {
  theme(text=element_text(size=16))
}

#Solicitudes TOTALES por canales de trafico
t3 <- ggplot(paidMedia, aes(sourceMedium,Solicitudes, fill = month)) 
t3 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + my_color_paidMediaPO() + 
  geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1) + my_font()


#tasa de conversión por canales de trafico
t2 <- ggplot(paidMedia, aes(sourceMedium,tasaConversion, fill = month)) 
t2 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + my_color_paidMediaPO() + 
  geom_text(aes(label = tasaConversion), position = position_dodge(0.7), vjust= -1) + my_font()




#FACEBOOK
facebookPO <- paidMedia %>% filter(sourceMedium == "facebook / cpa") %>%
mutate(cambioLeads =round((Solicitudes/lag(Solicitudes))-1,2),cambioTasa =round((tasaConversion/lag(tasaConversion))-1,2))

#GOOGLE ADWORDS
GooglePO <- paidMedia %>% filter(sourceMedium == "google / cpc") %>%
  mutate(cambioLeads =round((Solicitudes/lag(Solicitudes))-1,2),cambioTasa =round((tasaConversion/lag(tasaConversion))-1,2))




setwd("C:/Users/s6114349/Documents")
l <- list("Conversiones"= Conversiones)
write.xlsx(l, file = "Plazo Online- Raw Data.xlsx")
getwd()
