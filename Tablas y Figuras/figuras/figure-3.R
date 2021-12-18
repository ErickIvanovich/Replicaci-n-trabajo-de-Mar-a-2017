## Figura 3

source("Adjusted and Cleaned Data/base.R")

# Importar Librerías

lib_eval("tidyverse")
lib_eval("ggsci")
lib_eval("cowplot")

# Cargar la data de los archivos RDS 

load('Adjusted and Cleaned Data/ACS2016.Rdata')
individuals <- readRDS("Adjusted and Cleaned Data/ind_hh.RDS")
left_df1 <- readRDS("Adjusted and Cleaned Data/left_df.RDS")
where_df <- readRDS("Adjusted and Cleaned Data/where_df.RDS")
hh_main <- readRDS("Adjusted and Cleaned Data/hh_main.RDS")


# Unir individuos con la strata de los hogares

resources_df <- hh_main %>% 
  group_by(strata) %>% 
  dplyr::select(strata, contains(".sept"), contains(".oct"), 
                contains(".nov"), contains(".dec")) %>% 
  mutate_all(.funs = function(x) {
    case_when(x == 1 ~ 1, 
              x == 2 ~ 8, 
              x == 3 ~ 15, 
              x == 4 ~ 30, 
              TRUE ~ 0)
  }
  ) %>% 
  transmute(Agua = water.sept + water.oct + water.nov + water.dec,
            Electricidad = electricity.sept + electricity.oct +
              electricity.nov + electricity.dec, 
            Celular = cell.sept + cell.oct + cell.nov + cell.dec) 


keycol <- "resource"
valuecol <- "values"
gathercols <- c("Agua", "Celular", "Electricidad")

res_df_long <- gather_(resources_df, keycol, valuecol, gathercols)
res_df_long$resource <- factor(res_df_long$resource, levels = c("Agua", "Celular", "Electricidad"))

## 3a: Tiempo sin acceso a los servicios de agua, servicio de señal celular y electricidad en comparación a la lejanía entre
# la residencia y la ciudad más cercana

fig3a <- ggplot(res_df_long, aes(x=factor(strata), y=values)) + 
  geom_boxplot() +
  #geom_jitter(width = 0.2, alpha = 0.1) +
  facet_grid(~resource) +
  theme_classic() + 
  scale_x_discrete("Categoría de Lejanía", expand = c(0, 0)) + 
  scale_y_continuous("Número de días sin acceso") + 
  theme(panel.spacing.x = unit(1, "cm"))


## 3b: Proporción de la muestra que reportó al menos un día sin servicio médico de acuerdo a los factores presentados

households <- readRDS("Adjusted and Cleaned Data/hh_main.RDS")
individuals <- readRDS("Adjusted and Cleaned Data/individuals.RDS")
deaths <- readRDS("Adjusted and Cleaned Data/deaths.RDS")
left <- readRDS("Adjusted and Cleaned Data/left_df.RDS")

# Análisis de acceso médico: Al menos un día sin cuidado

cats <- c("No pudo obtener medicamentos", "No pudo obtener equipo respiratorio", "Carreteras dañadas", "Instalaciones cerradas",
          "Médicos indisponibles", "No pudo costear cuidado", "Dificultades de transportación", "Falta de servicio del 911", "No pudo hacerse diálisis")


figure_access <- households %>%
  rowwise() %>%
  mutate_at(vars(access_med.no_911, access_med.no_transport, access_med.roads_damaged, 
                 access_med.facility_closed, access_med.no_doctors, access_med.no_dialysis,
                 access_med.no_resp_mach, access_med.no_meds, access_med.couldnt_afford
  ), 
  funs(case_when(
    as.numeric(.) == 99 ~ 0,
    as.numeric(.) == 0 ~ 0,
    as.numeric(.) == 1 ~ 1,
    as.numeric(.) == 2 ~ 1,
    as.numeric(.) == 3 ~ 1,
    as.numeric(.) == 4 ~ 1,
    as.numeric(.) == 5 ~ 1,
    TRUE ~ 0))) %>%
  ungroup() %>%
  dplyr::select(access_med.no_911, access_med.no_transport, access_med.roads_damaged, 
                access_med.facility_closed, access_med.no_doctors, access_med.no_dialysis,
                access_med.no_resp_mach, access_med.no_meds, access_med.couldnt_afford)




figure_df <- figure_access %>% colMeans() %>% as.data.frame()
names(figure_df) <- c("Freq")
figure_df$Var1 <- rownames(figure_df)  
figure_df <- figure_df[order(-figure_df$Freq),]



fig3b <- ggplot(figure_df, aes(reorder(Var1, Freq), Freq)) + 
  geom_bar(stat="identity") +
  theme_classic()  +
  scale_x_discrete("",labels = cats[order(figure_df$Freq)]) +
  scale_y_continuous("Proporción de la muestra", expand = c(0, 0.01)) +
  coord_flip()

# Unir gráficas fig3a y fig3b

fig3 <- plot_grid(fig3a, fig3b, labels = c("A", "B"), nrow = 2)

# Desplegar gráficas

fig3

#ggsave('../figures/figure3.pdf', fig3, 
#       scale = 1.25, width = 5.5, height = 6)