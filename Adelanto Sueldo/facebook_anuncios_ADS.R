library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)

Adelanto_Sueldo <- read_excel("C:/Users/s6114349/Desktop/Adelanto_Sueldo.xlsx")

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
my_color <- function() {
  scale_fill_brewer(palette = "Greens")
}


Adelanto_Sueldo$month <- format(Adelanto_Sueldo$Reporting_Ends, format = "%b")

Adelanto_Sueldo$Ad <- str_extract(Adelanto_Sueldo$Ad_Name, "concierto|pareja|viaje|ViajeCusco|RegalodePapa")
Adelanto_Sueldo$Formato <- str_extract(Adelanto_Sueldo$Ad_Name, "video|estatico")


Anuncios <- Adelanto_Sueldo %>%
  select(month, Ad, Formato, Delivery, Impressions, Link_Clicks,Frequency, Relevance_Score, CPM, Cost)

Ad_mensual <- Anuncios %>%
  group_by(month, Ad) %>% 
  summarise(Impressions = sum(Impressions), Clicks = sum(Link_Clicks), adCost = sum(Cost), Frequencia = mean(Frequency), CPM = mean(CPM), RS = mean(Relevance_Score)) %>%
  mutate(CTR=(Clicks/Impressions)*100) 
a <- round_df(Ad_mensual,1) 

Anuncios_mensual <- Anuncios %>%
  group_by(month) %>% 
  summarise(Impressions = sum(Impressions), Clicks = sum(Link_Clicks), adCost = sum(Cost), Frequencia = mean(Frequency), CPM = mean(CPM), RS = mean(Relevance_Score)) %>%
  mutate(CTR=(Clicks/Impressions)*100, PorcentajeCambio =round((CPM/lag(CPM))-1,2))

b <- round_df(Anuncios_mensual,1)

k <- ggplot(a, aes(reorder(Ad, -CTR),CTR, fill = month)) 
k + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light()  + 
  geom_text(aes(label = CTR), position = position_dodge(0.7), vjust= -1)  + my_color()

