library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(stringr)
ga_auth()


scotiabank <- 86666711

#ANALISIS DE CAMPANAS
campanaS <- dim_filter(dimension = "campaign", operator = "REGEXP", expressions="(.*)_Adelanto_Sueldo_(.*)")
Sads <- filter_clause_ga4(list(campanaS))
Adwords_ADS <- google_analytics(scotiabank,
                               date_range = c("2018-01-01","2018-05-31"),
                               metrics = c("sessions", "goal3Completions", "adCost"),
                               dimensions = c("month","campaign","adGroup", "adContent"),
                               dim_filters = Sads,
                               anti_sample = TRUE)

Adwords_ADS$NCampana <- str_extract(Adwords_ADS$campaign, "GSP|GDN|SEM")

camp_ADS <- Adwords_ADS %>%
  group_by(NCampana, month) %>%
  summarise(Solicitudes = sum(goal3Completions), Visitas = sum(sessions), adCost = sum(adCost)) %>%
  mutate(CPL= adCost/Solicitudes, TC =Solicitudes/Visitas) 
  

  
my_font <- function() {
  theme(text=element_text(size=16))
}


i <- ggplot(camp_ADS, aes(reorder(NCampana, -Solicitudes) , Solicitudes, fill = month))
i + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() + geom_text(aes(label = Solicitudes, y= Solicitudes + 0.05), position = position_dodge(0.7), vjust= -1) + my_font()
