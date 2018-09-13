library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
ga_auth()

scotiabank <- 86666711


#DATOS DE CAMPANA PLAZA ONLINE  - GOOGLE ADWORDS
#DATOS DIARIOS
campanaDA <- dim_filter(dimension = "adwordsCampaignID", operator = "REGEXP", expressions="1041093542|1038973829|1042013973|1045733014")
DAads <- filter_clause_ga4(list(campanaDA))
weeklyAdwords <- google_analytics_4(scotiabank,
                                      date_range = c("2018-01-01","yesterday"),
                                      metrics = c("sessions", "adCost","goal12Completions"),
                                      dimensions = c("month", "week"),
                                      dim_filters = DAads,
                                       anti_sample = TRUE)

AdwordsData <- weeklyAdwords %>%
  mutate(Solicitudes=goal12Completions,tasaConversion=round(Solicitudes/sessions,2)) %>%
  select(month,sessions, Solicitudes, tasaConversion) %>%
  filter(!Solicitudes == 0) %>%
  arrange(-Solicitudes) 

sum(weeklyAdwords$adCost)


#ANALISIS DE CAMPANAS
campanaDA <- dim_filter(dimension = "adwordsCampaignID", operator = "REGEXP", expressions="1041093542|1038973829|1042013973|1045733014")
DAads <- filter_clause_ga4(list(campanaDA))
campanaAdwords <- google_analytics_4(scotiabank,
                                   date_range = c("2018-01-01","yesterday"),
                                   metrics = c("sessions", "adCost","goal12Completions"),
                                   dimensions = c("campaign", "adGroup", "month", "week"),
                                   dim_filters = DAads,
                                   anti_sample = TRUE)

campanaAdwords$Ncampana <- str_extract(campanaAdwords$campaign, "GSP|All|Desktop|Movil")

campanaAds <- campanaAdwords %>% 
  mutate(Solicitudes=goal12Completions,tasaConversion=round(Solicitudes/sessions,2)) %>%
  select(month, Ncampana, adCost,sessions, Solicitudes, tasaConversion) %>%
  group_by(Ncampana) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes), Costo =sum(adCost), CPL = Costo/Solicitudes)



my_font <- function() {
  theme(text=element_text(size=16))
}  


kl1 <- ggplot(campanaAds, aes(reorder(Ncampana, -Solicitudes), Solicitudes))
kl1 + geom_bar(width=0.7, stat = "identity", fill = "#E0776D") + theme_light() + geom_text(aes(label = Solicitudes), vjust= -1) + my_font()




setwd("C:/Users/s6114349/Desktop/Christian/Reporte Weekly/R Scripts")
l <- list("Conversiones"= Conversiones)
write.xlsx(l, file = "Plazo Online- Raw Data.xlsx")
getwd()
