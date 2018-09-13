library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(stringr)
ga_auth()


scotiabank <- 86666711


simular <- dim_filter(dimension = "eventCategory", operator = "REGEXP", expressions="Adelanto_de_Sueldo")
simular1 <- filter_clause_ga4(list(simular))
eventSimular <- google_analytics_4(scotiabank,
                                   date_range = c("2017-09-01","yesterday"),
                                   metrics = c("uniqueEvents"),
                                   dimensions = c("month", "eventAction"),
                                   dim_filters = simular1,
                                   anti_sample = TRUE)



  
  str_replace(eventSimular$eventAction, "Simular(.*)","Simular")

k <- grep("Simular(.*)",eventSimular$eventAction , ignore.case = TRUE)
eventSimular$eventAction[k] <- "Simular"
k1 <- grep("Solicitar(.*)",eventSimular$eventAction , ignore.case = TRUE)
eventSimular$eventAction[k1] <- "Solicitar"


eventSimular %>% group_by(month, eventAction) %>% summarise(Counts=sum(uniqueEvents))


o <- ggplot(eventSimular, aes(month, uniqueEvents, fill=eventAction)) 
o +  geom_bar(width=0.7, stat = "identity", position = "stack") + scale_fill_brewer(palette = "Dark2")

#facebook
segment_FB <- "gaid::6GSwPnZhRLuzJxvZbK57Qg"
seg_Facebook <- segment_ga4("Plazo_Online", segment_id = segment_FB)
simularFB <- dim_filter(dimension = "eventCategory", operator = "REGEXP", expressions="Plaza_Online_Renovado")
simular1FB <- filter_clause_ga4(list(simularFB))
eventSimularFB <- google_analytics_4(scotiabank,
                                   date_range = c("2017-10-01","2017-12-31"),
                                   metrics = c("uniqueEvents"),
                                   dimensions = c("month", "eventAction"),
                                   dim_filters = simular1FB,
                                   segments = seg_Facebook,
                                   anti_sample = TRUE)

h <- grep("Simular-up",eventSimularFB$eventAction , ignore.case = TRUE)
eventSimularFB$eventAction[h] <- "Simular"
h1 <- grep("Solicitar-(.*)",eventSimularFB$eventAction , ignore.case = TRUE)
eventSimularFB$eventAction[h1] <- "Solicitar"


ok <- ggplot(eventSimularFB, aes(month, uniqueEvents, fill=eventAction)) 
ok +  geom_bar(width=0.7, stat = "identity", position = "stack") + scale_fill_brewer(palette = "Set1")

eventSimularFB %>% group_by(month, eventAction) %>% summarise(Counts=sum(uniqueEvents))

#google adwords
segment_adwords <- "gaid::aHIhDZ3uTUqNuvYStJyVWA"
seg_adwords <- segment_ga4("Plazo_Online", segment_id = segment_adwords)
simularAds <- dim_filter(dimension = "eventCategory", operator = "REGEXP", expressions="Plaza_Online_Renovado")
simular1Ads <- filter_clause_ga4(list(simularAds))
eventSimularAds<- google_analytics_4(scotiabank,
                                     date_range = c("2017-10-01","2017-12-31"),
                                     metrics = c("uniqueEvents"),
                                     dimensions = c("month", "eventAction"),
                                     dim_filters = simular1Ads,
                                     segments = seg_adwords,
                                     anti_sample = TRUE)


j <- grep("Simular-up",eventSimularAds$eventAction , ignore.case = TRUE)
eventSimularAds$eventAction[j] <- "Simular"
j1 <- grep("Solicitar-(.*)",eventSimularAds$eventAction , ignore.case = TRUE)
eventSimularAds$eventAction[j1] <- "Solicitar"



jk <- ggplot(eventSimularAds, aes(month, uniqueEvents, fill=eventAction)) 
jk +  geom_bar(width=0.7, stat = "identity", position = "stack") + scale_fill_brewer(palette = "Accent")

eventSimularAds %>% group_by(month, eventAction) %>% summarise(Counts=sum(uniqueEvents))

