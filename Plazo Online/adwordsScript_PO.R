library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(wesanderson)
library(stringr)
ga_auth()

scotiabank <- 86666711

#ANALISIS DE CAMPANAS
campanaPO <- dim_filter(dimension = "adwordsCustomerID", operator = "REGEXP", expressions = "5393799670")
POads <- filter_clause_ga4(list(campanaPO))
campanaAdwords <- google_analytics_4(scotiabank,
                                   date_range = c("2018-01-01","yesterday"),
                                   metrics = c("impressions", "adClicks", "sessions", "adCost","goal2Completions"),
                                   dimensions = c("month", "campaign", "adGroup"),
                                   dim_filters = POads,
                                   anti_sample = TRUE)


campanaAdwords$NCampana <- str_extract(campanaAdwords$campaign, "Display_Abierto|Branded_Abierto|Generic_Abierto|Competitive_Abierto|GSP|BCP_Abierto|Interbank_Abierto|Generic_Simulador|Generic_Informacion|GDN")


  campanaAds <- campanaAdwords %>% 
    na.omit() %>%
    mutate(Solicitudes=goal2Completions, Cost=round(adCost,0), tasaConversion=round(goal2Completions/sessions,3)) %>% 
    select(month, NCampana,adGroup,  sessions,  Cost , Solicitudes,tasaConversion) %>%
    mutate(CPL=Cost/Solicitudes) %>%
    filter(!Solicitudes == 0 )


campana1 <- campanaAds %>%
  group_by(NCampana, month) %>%
  summarise(Visitas=sum(sessions), Solicitudes=sum(Solicitudes), Costo=sum(Cost)) %>%
  mutate(tasaConversion=round(Solicitudes/Visitas,2), CPL=Costo/Solicitudes)



my_font <- function() {
  theme(text=element_text(size=16))
}



k1 <- ggplot(campanaAds, aes(reorder(NCampana, -Solicitudes), Solicitudes, fill = month))
k1 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + my_font() + 
  geom_text(aes(label = Solicitudes), position = position_dodge(0.7), vjust= -1) 


y <- ggplot(campana1, aes(reorder(NCampana, -CPL), CPL, fill = month))
y + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + my_font() + 
  geom_text(aes(label = CPL), position = position_dodge(0.7), vjust= -1) 




setwd("C:/Users/s6114349/Desktop/Christian/Reporte Weekly/R Scripts")
l <- list("Conversiones"= Conversiones)
write.xlsx(l, file = "Plazo Online- Raw Data.xlsx")
getwd()
