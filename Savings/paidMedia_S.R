library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
ga_auth()

scotiabank <- 86666711


#TOTAL
dailyAll <- google_analytics(scotiabank,
                               date_range = c("2018-07-01","2018-07-31"),
                               metrics = c("sessions","goal8Completions", "goal9Completions", "goal6Completions", "goal7Completions"),
                               dimensions = c("month", "sourceMedium"),
                               anti_sample = TRUE)


TOTALES <- dailyAll %>% 
  select(month,sessions, goal8Completions,goal9Completions,goal6Completions, goal7Completions) %>%
  group_by(month)%>%
  summarise(
    totalSavings=sum(goal8Completions+goal9Completions+goal6Completions+goal7Completions))%>%
  mutate(
    Free=round(totalSavings*0.8,0),
    Travel=round(totalSavings*0.13,0),
    Power= round(totalSavings*0.07,0)
    ) %>%
  select(month, Free, Travel, Power)
  
#PAUTA
segment_Savings <- "gaid::DH9DCJTZTJG9qZD0H37C2g"
seg_S_ALL <- segment_ga4("Savings", segment_id = segment_Savings)
dailyAllP <- google_analytics(scotiabank,
                             date_range = c("2018-07-01","2018-07-31"),
                             metrics = c("sessions", "goal8Completions", "goal9Completions", "goal6Completions", "goal7Completions"),
                             dimensions = c("month", "sourceMedium"),
                             segments = seg_S_ALL,
                             anti_sample = TRUE)


paidMedia_S <- dailyAllP %>% 
  filter(
    sourceMedium %in% c("facebook / cpa", "google / cpc","facebook / reach", "instagram / cpa")) %>%
  select(month, sourceMedium,sessions, goal8Completions, goal9Completions, goal6Completions, goal7Completions)%>%
  mutate(
    solicitudTotal=goal8Completions+goal9Completions+goal6Completions+goal7Completions, 
    tasaConversion=round(solicitudTotal/sessions,2))


PAUTA <- paidMedia_S %>% 
  select(month,sessions, goal8Completions, goal9Completions, goal6Completions,goal7Completions) %>%
  group_by(month)%>%
  summarise(
    totalSavingsP=sum(goal8Completions+goal9Completions+goal6Completions+goal7Completions)) %>%
  mutate(
    Free=round(totalSavingsP*0.8,0),
    Travel=round(totalSavingsP*0.13,0),
    Power= round(totalSavingsP*0.07,0)
  ) %>%
  select(month, Free, Travel, Power)


my_color <- function() {
  scale_fill_brewer(palette = "PuRd") 
}
my_font <- function() {
  theme(text=element_text(size=16))
}
my_time <- function() {scale_x_discrete(breaks=c("01", "02", "03"),
                       labels=c("Enero", "Febrero", "Marzo"))}

#Solicitudes TOTALES por canales de trafico POR SEMANA
u3 <- ggplot(paidMedia_S, aes(sourceMedium,solicitudTotal, fill = month)) 
u3 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + my_color() + my_font() + geom_text(aes(label = solicitudTotal, y= solicitudTotal + 0.05), position = position_dodge(0.7), vjust= -1) 


paidMedia_S %>%
  filter(month == "03")

mensualSavings <- paidMedia_S %>%
  select(month, sourceMedium,sessions, solicitudTotal, tasaConversion)%>%
  group_by(month)%>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(solicitudTotal)) %>%
  mutate(tasa=round(Solicitudes/Visitas*100,1))


u33 <- ggplot(mensualSavings, aes(month, Solicitudes)) 
u33 + geom_bar(width=0.7, stat = "identity", fill = "#E72564") + theme_light() + my_color() + geom_text(aes(label = Solicitudes, y= Solicitudes + 0.05), position = position_dodge(0.7), vjust= -1) + my_font() + my_time()


#FACEBOOK
 facebookSavings <- paidMedia_S %>% filter(sourceMedium == "facebook / cpa") %>%
  mutate(PorcentajeCambio =round((solicitudTotal/lag(solicitudTotal))-1,2))
 
 #GOOGLE ADWORDS
 GoogleSavings <- paidMedia_S %>% filter(sourceMedium == "google / cpc") %>%
   mutate(PorcentajeCambio =round((solicitudTotal/lag(solicitudTotal))-1,2))
 
 sum(facebookSavings$sessions)
 sum(GoogleSavings$sessions)
 
 
 
 setwd("/Users/Invitado/Desktop/R Scripts")
 l <- list("paidMedia_S"= paidMedia_S)
 write.xlsx(l, file = "Adelanto Sueldo - Raw Data.xlsx")
 getwd()
 