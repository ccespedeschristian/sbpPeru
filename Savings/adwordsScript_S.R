library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(stringr)
ga_auth()

scotiabank <- 86666711

#ANALISIS DE CAMPANAS
campanaS <- dim_filter(dimension = "campaign", operator = "REGEXP", expressions="(.*)_Savings_(.*)")
Sads <- filter_clause_ga4(list(campanaS))
campanaAdwords <- google_analytics(scotiabank,
                                   date_range = c("2018-01-01","2018-05-31"),
                                   metrics = c("sessions", "calcMetric_SavingsLeads", "adCost"),
                                   dimensions = c("campaign","adGroup",  "month"),
                                   dim_filters = Sads,
                                   anti_sample = TRUE)

campanaAdwords$NCampana <- str_extract(campanaAdwords$campaign, "GSP|Remarketing|Travel|Competition|Generic|Competition_BCP|Competition_BBVA|Free|Power")
campanaAdwords$Type <- str_extract(campanaAdwords$campaign, "GSP|SEM|GDN|Video")


campanaAds <- campanaAdwords %>% 
  mutate( solicitudTotal=calcMetric_SavingsLeads, tasaConversion=round(solicitudTotal/sessions,2), Share=solicitudTotal/sum(solicitudTotal)*100) %>% 
  select(month, Type, sessions, solicitudTotal, tasaConversion) %>%
  group_by(Type, month) %>%
  summarise(Solicitudes=sum(solicitudTotal))
  

my_color <- function() {
  scale_fill_brewer(palette = "PuRd") 
}
my_font <- function() {
  theme(text=element_text(size=16))
}
my_time <- function() {scale_x_discrete(breaks=c("01", "02", "03", "04"),
                                        labels=c("Enero", "Febrero"))}

kl1 <- ggplot(campanaAds, aes(reorder(Type, -Solicitudes) , Solicitudes, fill = month))
kl1 + geom_bar(width=0.7, stat = "identity", position = "dodge") + my_color() + theme_light() + geom_text(aes(label = Solicitudes, y= Solicitudes + 0.05), position = position_dodge(0.7), vjust= -1)

campanaAdwords %>% 
  mutate(solicitudTotal=calcMetric_SavingsLeads, CPL=adCost/calcMetric_SavingsLeads) %>% 
  select(month, adGroup, sessions, solicitudTotal,  adCost) %>% 
  mutate(Cambio_Solicitud =round((solicitudTotal/lag(solicitudTotal))-1,2), Cambio_gasto=round((adCost/lag(adCost))-1,2))



#gsp - adwords

#ANALISIS DE CAMPANAS
campanaS <- dim_filter(dimension = "campaign", operator = "REGEXP", expressions="DF_AO_Savings_GSP_BBDD")
Sads <- filter_clause_ga4(list(campanaS))
AdwordsGSP <- google_analytics(scotiabank,
                                   date_range = c("2018-01-01","2018-02-01"),
                                   metrics = c("sessions", "calcMetric_SavingsLeads", "adCost"),
                                   dimensions = c("campaign","adGroup", "adContent",  "month"),
                                   dim_filters = Sads,
                                   anti_sample = TRUE)


anunciosGSP <- AdwordsGSP %>%
  select(month, anuncio = adContent, sessions, solicitudTotal = calcMetric_SavingsLeads,  Cost=adCost) %>%
  mutate(tasaConversion =solicitudTotal/sessions, CPL= Cost/solicitudTotal )

gsp <- ggplot(anunciosGSP, aes(reorder(anuncio, -solicitudTotal) , solicitudTotal, fill = month))
gsp + geom_bar(width=0.7, stat = "identity", position = "dodge") + my_color() + theme_light() + geom_text(aes(label = solicitudTotal, y= solicitudTotal + 0.05), position = position_dodge(0.7), vjust= -1)





setwd("C:/Users/s6114349/Desktop/Christian/Reporte Weekly/R Scripts")
l <- list("Conversiones"= Conversiones)
write.xlsx(l, file = "Plazo Online- Raw Data.xlsx")
getwd()
