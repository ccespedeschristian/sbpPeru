library(googleAnalyticsR)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidytext)
library(RColorBrewer)
ga_auth()

library(googleLanguageR)
gl_auth(json_file = ("C:/Users/s6114349/Desktop/Christian/Reportes/R Scripts/DEMO NPL/client_secret.json"))   


scotiabank <- 86666711

#ANALISIS DE CAMPANAS
campanaADS <- dim_filter(dimension = "campaign", operator = "REGEXP", expressions = "SEM_ADS_Brand")
POads <- filter_clause_ga4(list(campanaADS))
queryAdwords <- google_analytics(scotiabank,
                                     date_range = c("2018-02-01","yesterday"),
                                     metrics = c("impressions", "adClicks", "sessions", "adCost","goal2Completions"),
                                     dimensions = c("adMatchedQuery"),
                                     dim_filters = POads,
                                     anti_sample = TRUE)


queryTranslate <- gl_translate(queryAdwords$adMatchedQuery, target = "en")


bing <- get_sentiments("bing")

queryTranslate %>%
  inner_join(lexicon)

