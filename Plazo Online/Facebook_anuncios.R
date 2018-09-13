library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
PO_Enero <- read_excel("C:/Users/s6114349/Desktop/plazo_online_Marzo.xls")

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

PO_Enero$month <- format(PO_Enero$Reporting_Ends, format = "%b")
 
PO_Enero$Ad <- str_extract(PO_Enero$Ad_Name, "eurotrip|reencuentro|hawai|boda|rusia|mudanza|roadtrip|papa")
PO_Enero$Formato <- str_extract(PO_Enero$Ad_Name, "video|estatico")

Anuncios <- PO_Enero %>%
  select(month, Ad, Formato, Delivery, Impressions, Link_Clicks,Frequency, Relevance_Score, CPM, Amount_Spent)

Ad_mensual <- Anuncios %>%
  group_by(month, Ad) %>% 
  summarise(Impressions = sum(Impressions), Clicks = sum(Link_Clicks), adCost = sum(Amount_Spent), Frequencia = mean(Frequency), CPM = mean(CPM), RS = mean(Relevance_Score)) %>%
  mutate(CTR=(Clicks/Impressions)*100) %>%
  filter(!Ad %in% c("PAPA", "BODA"))

a <- round_df(Ad_mensual,1)  

Anuncios_mensual <- Anuncios %>%
  group_by(month) %>% 
  summarise(Impressions = sum(Impressions), Clicks = sum(Link_Clicks), adCost = sum(Cost), Frequencia = mean(Frequency), CPM = mean(CPM), RS = mean(Relevance_Score)) %>%
  mutate(CTR=(Clicks/Impressions)*100, PorcentajeCambio =round((CPM/lag(CPM))-1,2))

b <- round_df(Anuncios_mensual,1)  

Formato_mensual <- Anuncios %>%
  group_by(Formato, month) %>% 
  summarise(Impressions = sum(Impressions), Clicks = sum(Link_Clicks), adCost = sum(Cost), Frequencia = mean(Frequency), CPM = mean(CPM), RS = mean(Relevance_Score)) %>%
  mutate(CTR=(Clicks/Impressions)*100)

c <- round_df(Formato_mensual,1)  


c1 <- ggplot(c, aes(reorder(Formato, -CTR),CTR, fill = month)) 
c1 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light()  + 
  geom_text(aes(label = CTR), position = position_dodge(0.7), vjust= -1) 


k <- ggplot(a, aes(reorder(Ad, -CTR),CTR, fill = month)) 
k + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light()  + 
  geom_text(aes(label = CTR), position = position_dodge(0.7), vjust= -1)  + my_color()



  my_color <- function() {
    scale_fill_brewer(palette = "Greens")
  }
