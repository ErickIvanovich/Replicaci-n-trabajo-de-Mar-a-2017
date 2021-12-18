#Stephanie M. Ramos Camacho
#Figure S6
#analizar las muertes cercana a vecinos por strata

#sustituir con el path donde este guardado el file "base.R"
source("/../base.R")

#libreria a usarse
lib_eval("tidyverse")

#archivo para los datos para el codigo
#sustituir con el path donde este guardado el file "hh_main.RDS"
households <- readRDS("../hh_main.RDS")

#limpiando la data para el analisis de los vecinos
households$mort_neighbor_f <- ordered(households$mort_neighbor,
                                      levels = c(99,0,1,2,3,4,5,6),
                                      labels = c("No se sabe", 
                                                 "0","1","2","3","4","5", "7"))
households$neighbors_num_f <- ordered(households$neighbors_num,
                                      levels = c(1,2,3,4,5),
                                      labels = c(9, 25, 50, 100, 200))

#variable para hacer la grafica con ggplot de las defunciones
figs6 <- households %>%
  subset(!mort_neighbor == "---")%>% 
  mutate(n_rate = as.numeric(as.character(mort_neighbor_f))/as.numeric(as.character(neighbors_num_f))) %>%
  mutate(n_rate = ifelse(is.na(n_rate), 0, n_rate)) %>%
  {aggregate(n_rate~strata, data = ., FUN = mean)} %>%
  mutate(n_rate = n_rate*1000) %>% 
  ggplot(aes(y=n_rate, x=strata)) + 
  geom_bar(stat="identity") + 
  scale_x_discrete("Lejanía/Distancia", limits=c(1:8), 
                   labels=c("1 - Menor Remoto", "2", "3", "4",
                            "5", "6", "7", "8 - Mayor Remoto")) +
  ylab("Número de Defunciones por cada 1,000 Personas") + 
  #ggtitle
  theme_classic() + 
  geom_hline(yintercept = 14.4, color="red", linetype =2) + 
  geom_label(aes(0,14.4,label = "Tasa Estimada de Mortalidad después de un Huracán", vjust = -1,hjust=.1))

#guarda y de ser necesario reescribe el pdf
#sustituir con el path donde se quiera guardar el file pdf "figure-s6.pdf"
ggsave("figure-s6.pdf", figs6, "pdf", "/../",
       units="in", width=3, height = 3, scale=2)
