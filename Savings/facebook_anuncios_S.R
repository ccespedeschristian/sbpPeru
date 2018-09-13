library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)

Savings <- read_excel("C:/Users/s6114349/Desktop/Savings.xlsx")

Savings$month <- format(Savings$Reporting_Ends, format = "%b")
Savings$formato <-  str_extract(Savings$Ad_Name, "ppl|multiproducto")
Savings$producto <- str_extract(Savings$Ad_Name, "generico|travel|power|CSueldo|free")

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

Savings_Anuncios <- Savings %>%
  select(month, producto, formato, Delivery, Impressions, Link_Clicks,Frequency, Relevance_Score, CPM, Cost)

Anuncios_mensual_Savings <- Savings_Anuncios %>%
  group_by(month) %>% 
  summarise(Impressions = sum(Impressions), Clicks = sum(Link_Clicks), adCost = sum(Cost), Frequencia = mean(Frequency), CPM = mean(CPM), RS = mean(Relevance_Score)) %>%
  mutate(CTR=(Clicks/Impressions)*100, PorcentajeCambio =round((CPM/lag(CPM))-1,2))

v <- round_df(Anuncios_mensual_Savings,1)  


Ad_mensual_savings <- Savings_Anuncios %>%
  group_by(month, producto) %>% 
  summarise(Impressions = sum(Impressions), Clicks = sum(Link_Clicks), adCost = sum(Cost), Frequencia = mean(Frequency), CPM = mean(CPM), RS = mean(Relevance_Score)) %>%
  mutate(CTR=(Clicks/Impressions)*100) %>%
  filter(!producto == "NA")

n <- round_df(Ad_mensual_savings,1)

n1 <- ggplot(n, aes(reorder(producto, -CTR),CTR, fill = month)) 
n1 + geom_bar(width=0.7, stat = "identity", position = "dodge") + theme_light()  + 
  geom_text(aes(label = CTR), position = position_dodge(0.7), vjust= -1)  + my_color()


Formato_mensual_savings <- Savings_Anuncios %>%
  group_by(month, formato) %>% 
  summarise(Impressions = sum(Impressions), Clicks = sum(Link_Clicks), adCost = sum(Cost), Frequencia = mean(Frequency), CPM = mean(CPM), RS = mean(Relevance_Score)) %>%
  mutate(CTR=(Clicks/Impressions)*100)

p <- ggplot(Formato_mensual_savings, aes(reorder(formato, -CTR),CTR, fill = month))
p +  geom_col(width=0.7, position = "dodge") + theme_light() + 
  geom_text(aes(label = CTR), position = position_dodge(0.7), vjust= -1) + my_font()