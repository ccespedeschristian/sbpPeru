library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
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


#volumen de leads
y1 <- ggplot(eventSimular, aes(reorder(month, -uniqueEvents),uniqueEvents, fill =eventAction))
y1 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light() 
