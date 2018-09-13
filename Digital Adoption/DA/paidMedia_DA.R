library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
ga_auth()

scotiabank <- 86666711

segment_DA <- "gaid::_GqIZjcjTDm0vdz7hp_9HA"
seg_DA_ALL <- segment_ga4("Adopcion Digital", segment_id = segment_DA)
dailyAll <- google_analytics(scotiabank,
                               date_range = c("2018-03-05","2018-04-15"),
                               metrics = c("sessions","goal12Completions"),
                               dimensions = c("month", "sourceMedium"),
                               segments = seg_DA_ALL,
                               anti_sample = TRUE)


paidMedia_DA <- dailyAll %>% 
  filter(sourceMedium %in% c("facebook / cpa", "google / cpc", "instagram / cpa") ) %>%
  mutate(Solicitudes=goal12Completions, tasaConversion=round(Solicitudes/sessions,2)) %>%
  select(month,sourceMedium ,sessions,tasaConversion,Solicitudes) 

#TOTALES
TOTALES_PAID <- paidMedia_DA %>%
  group_by(sourceMedium) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1))


my_color_DA <- function() {
  scale_fill_brewer(palette = "Set2")
}
my_font_DA <- function() {
  theme(text=element_text(size=16))
}


#Solicitudes TOTALES por canales de trafico
t3 <- ggplot(TOTALES_PAID, aes(sourceMedium,Solicitudes)) 
t3 + geom_bar(width=0.7, stat = "identity") + theme_light() + my_color_DA() + 
  geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1) + my_font_DA()


#Solicitudes TOTALES por canales de trafico MENSUAL
t31 <- ggplot(paidMedia_DA, aes(sourceMedium,Solicitudes, fill= month)) 
t31 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + my_color_DA() + 
  geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1) + my_font_DA()


#FACEBOOK
 facebookDA <- paidMedia_DA %>% filter(sourceMedium == "facebook / cpa") %>%
  mutate(PorcentajeCambio =round((Solicitudes/lag(Solicitudes))-1,2))
  
sum(facebookDA$Solicitudes)
sum(facebookDA$sessions)
   
    
 #GOOGLE ADWORDS
 GoogleDA <- paidMedia_DA %>% filter(sourceMedium == "google / cpc") %>%
   mutate(PorcentajeCambio =round((Solicitudes/lag(Solicitudes))-1,2))
 
 sum(GoogleDA$Solicitudes)
 sum(GoogleDA$sessions)
  
 #Instagram
 instagramDA <- paidMedia_DA %>% filter(sourceMedium == "instagram / cpa") %>%
   mutate(PorcentajeCambio =round((Solicitudes/lag(Solicitudes))-1,2))
 
 sum(instagramDA$Solicitudes)
 sum(instagramDA$sessions)
 
 setwd("/Users/Invitado/Desktop/R Scripts")
 l <- list("paidMedia_S"= paidMedia_S)
 write.xlsx(l, file = "Adelanto Sueldo - Raw Data.xlsx")
 getwd()
 