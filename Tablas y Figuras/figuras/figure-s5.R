#Stephanie M. Ramos Camacho
#Figure S5

#sustitur con el path donde este guardado el file "base.R"
source("/../base.R")

lib_eval("ggridges")
lib_eval("tidyverse")

#archivo para los datos para el codigo
#sustitur con el path donde este guardado el file "hh_main.RDS"
mainHH <- readRDS("/../hh_main.RDS")

#Impacto de lejania para agua y electricidad
#Escoge la data a usarse para la figura s5 y lo guarda en esta variable
figs5Data <- mainHH %>%
  rowwise() %>%
  mutate_at(vars(electricity.sept, electricity.oct, electricity.nov, electricity.dec,
                 water.sept, water.oct, water.nov, water.dec,
                 cell.sept, cell.oct, cell.nov, cell.dec), 
            funs(case_when(
              is.na(as.numeric(.)) ~ 0.0,
              as.numeric(.) == 0 ~ 0.0,
              as.numeric(.) == 1 ~ 4,
              as.numeric(.) == 2 ~ 7.5,
              as.numeric(.) == 3 ~ 22.5,
              as.numeric(.) == 4 ~ 30.0,
              TRUE ~ 0.0))) %>%
  ungroup() %>%
  mutate(util_3 = electricity.sept+electricity.oct+electricity.nov+electricity.dec) %>%
  mutate(util_1 = water.sept+water.oct+water.nov+water.dec) %>%
  mutate(util_2 = cell.sept+cell.oct+cell.nov+cell.dec) %>%
  dplyr::select(strata,util_1,util_2,util_3) %>%
  gather(key,value,-strata) %>%
  mutate(key = factor(key,
                      levels=c("util_1", "util_2", "util_3"),
                      labels=c("Agua", "Cobertura Celular", "Electricidad")))

#grafica del ggplot con la data de la variable guardada
figs5 <- ggplot(figs5Data, aes(x = value, y = as.factor(strata))) + 
  geom_density_ridges(scale = 1) + facet_wrap(~key) + theme_classic() +
  scale_y_discrete(name="Lejanía/Distancia",limits=rev(levels(as.factor(figs5Data$strata)))) +
  xlab("Numbero de Días")+
  #ggtittle
  theme(plot.title = element_text(hjust=0.5)) 

#guarda y de ser necesario reescribe el pdf
#sustitur con el path donde se quiera guardar el file pdf "figure-s5.pdf"
ggsave("figure-s5.pdf", figs5, "pdf", "/../",
       units="in", width=4, height = 5, scale=2)
