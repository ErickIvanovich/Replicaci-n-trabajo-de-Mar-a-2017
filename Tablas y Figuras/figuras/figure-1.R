## Figura 1

source("Adjusted and Cleaned Data/base.R")

# ## Importar librerías relevantes

lib_eval("tidyverse")
lib_eval("ggsci")
lib_eval("grid")
lib_eval("gridExtra")

# Cargar la data de los archivos RDS 

load('Adjusted and Cleaned Data/ACS2016.Rdata')
age_df <- readRDS("Adjusted and Cleaned Data/cleaned_age_counts_acs_vs_survey.RDS")
hh_df <- readRDS("Adjusted and Cleaned Data/cleaned_hh_size_acs_vs_survey.RDS")
individuals <- readRDS("Adjusted and Cleaned Data/individuals.RDS")
hh_main <- readRDS("Adjusted and Cleaned Data/hh_main.RDS")

## Gráfica de comparación de edades

fig1a <- ggplot(age_df, aes(x = age_cat, y = prop, fill = src_cat)) + 
  geom_bar(stat = 'identity', alpha = .85, position = 'dodge', width = .8) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size=8), 
        legend.position = "none") + 
  scale_fill_nejm(name = NULL) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = seq(0, .16, .04), color = "white", alpha = .5) +
  labs(x = "Grupo de Edades", y = "Proporción", 
       title = "\na) Distribución de Edades")

## Gráfica de comparación del tamaño del núcleo familiar

fig1b <- ggplot(hh_df, aes(x = hh_cat, y = prop, fill = src_cat)) + 
  geom_bar(stat = 'identity', alpha = .85, position = 'dodge', width = .8) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size=8), 
        legend.position = c(1, .99), 
        legend.justification = c(1, 1)) + 
  scale_fill_nejm(name = NULL) + 
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  geom_hline(yintercept = c(.1, .2, .3), color = "white", alpha = .2) +
  labs(x = "Cantidad de personas en el hogar", y = NULL, 
       title = "\nb) Tamaño de núcleo familiar")

# Unir fig1a y figb en una sola gráfica

fig1 <- grid.arrange(
  top = textGrob(
    "",
    gp = gpar(fontsize = 14)
  ),
  fig1a, fig1b,
  layout_matrix = t(as.matrix(c(1,2)))
)

#ggsave("figure1.pdf", fig1, "pdf", "../figures/",
#        units="in", width=5, height = 3, scale=2)