#Stephanie M. Ramos Camacho
#Figure S4

#codigo recopilado para usarse en la creacion de esta figura
#sustituir con el path donde este guardado el file "base.R"
source("Adjusted and Cleaned Data/base.R")

lib_eval("tidyverse")

#archivo para los datos para el codigo
#sustituir con el path donde este guardado el file "hh_main.RDS"
mainHH <- readRDS("Adjusted and Cleaned Data/hh_main.RDS")

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

figs4

#guarda y de ser necesario reescribe el pdf
#sustituir con el path donde se quiera guardar el file de pdf "figure-s4.pdf"
ggsave("figure-s4.pdf", figs4, "pdf", "/../",
       units="in", width=5, height = 3, scale=2)

