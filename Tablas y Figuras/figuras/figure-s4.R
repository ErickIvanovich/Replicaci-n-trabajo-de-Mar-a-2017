#Stephanie M. Ramos Camacho
#Figure S4

#codigo recopilado para usarse en la creacion de esta figura
source("/Users/stephanie.marie/Documents/Semestre 1 2021-2022/CCOM 3031/Proyecto Final_Huracán María/base.R")

#instalando los paquetes para las librearias
#install.packages("tidyr")
#install.packages("magrittr") 
#install.packages("dplyr")

#libreria a usarse
#library(tidyr)
#library(ggplot2)
#library(dplyr)
#library(magrittr)
lib_eval("tidyverse")

#archivo para los datos para el codigo
mainHH <- read.RDS("/Users/stephanie.marie/Documents/Semestre 1 2021-2022/CCOM 3031/Proyecto Final_Huracán María/hh_main.RDS")

#proporcion faltante luego de enero 26(year despues de Maria)
#crea la variable con la data del estudio de las casas
figs4 <- mainHH %>%
  subset(flag == 1) %>%
  mutate(house_status = ifelse(house_status == "---", "1", house_status)) %>%
  {table(.$house_status, .$strata)} %>% prop.table() %>% as.data.frame()%>%
  #crea el ggplot con la data de las casas y strata
  ggplot(aes(x=Var2, y=Freq)) +
  geom_bar(stat="identity") +
  #labels de los ejes de la grafica
  xlab("Lejanía/Distancia") + 
  ylab("Proporción de hogares sin consentimiento") + 
  scale_x_discrete("Lejanía/Distancia", expand = c(0, 0), breaks = 1:8,
                   labels = c("1: Menor \nRemoto", "2", "3", "4",
                              "5", "6", "7", "8: Mayor \nRemoto"))+
  theme_classic()

#guarda y de ser necesario reescribe el pdf
ggsave("figure-s4.pdf", figs4, "pdf", "/Users/stephanie.marie/Documents/Semestre 1 2021-2022/CCOM 3031/Proyecto Final_Huracán María/",
       units="in", width=5, height = 3, scale=2)

