## Figura 2

# Importar librerías relevantes

source("Adjusted and Cleaned Data/base.R")
lib_eval("tidyverse")
lib_eval("ggsci")
lib_eval("patchwork")

# Cargar la data de los archivos RDS 

load("Adjusted and Cleaned Data/ACS2016.Rdata")
individuals <- readRDS("Adjusted and Cleaned Data/ind_hh.RDS")
where_df <- readRDS("Adjusted and Cleaned Data/where_df.RDS")
hh_main <- readRDS("Adjusted and Cleaned Data/hh_main.RDS")

# Unir individuos con la strata de los hogares
ind_hh <- individuals %>% 
  left_join(
    hh_main %>% 
      dplyr::select(hh_id, strata)
  ) %>% 
  mutate(
    left_final_cat = factor(left_final, 
                            levels = rev(c(1:6, 0, "---")), 
                            labels = rev(c("Alguna otra\nparte de PR",
                                           "Florida", 
                                           "Nueva York", 
                                           "Texas", 
                                           "Otro\nEstado", 
                                           "Otro\nPaís", 
                                           "No se sabe", 
                                           NA)), 
                            ordered = TRUE))

left_df <- ind_hh %>% 
  mutate(left = ifelse(grepl("---", left_final, fixed = TRUE), 0, 1),
         age_cat = base::cut(age, 
                             c(seq(0, 85, 10), Inf), 
                             include.lowest = TRUE, 
                             right = FALSE, 
                             ordered = TRUE)) %>% 
  group_by(left) %>% 
  mutate(total_n = n()) %>% 
  group_by(age_cat, left_final_cat, left) %>% 
  summarize(
    total_n = first(total_n), 
    n_obs = n(), 
    p_obs = n() / total_n)
levels(left_df$age_cat)[length(levels(left_df$age_cat))] <- "80+"

left_df2 <- left_df[left_df$left == 0, ] 
left_df2$left_final_cat2 <- "Estaba en el hogar/Falleció en el 2017"
class(left_df2$left_final_cat2)

fig2 <- ggplot() + 
  geom_bar(data = left_df %>% filter(left == 1), 
           aes(x = as.integer(age_cat) + .225, y = p_obs, 
               fill = left_final_cat), 
           stat = "identity", alpha = .85, width = .45) + 
  scale_fill_manual(name = "Se fue en el 2017", values = rev(pal_nejm()(7))) + 
  geom_bar(data = left_df2 %>% filter(left == 0), 
           aes(x = as.integer(age_cat) - .225, y = p_obs, colour = left_final_cat2),
           stat = "identity", alpha = 1, width = .45) + 
  scale_colour_manual(name="",values="grey") +
  theme_classic() + 
  theme(#axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), 
    legend.title=element_text(size = 9),
    legend.position = c(1, .99), 
    legend.justification = c(1, 1)) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = 1:9,
                     labels = levels(left_df$age_cat)) +
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(0, .3, .05), 
                     labels = c("0.0", "", "0.1", "", "0.2", "", "0.3")) +
  geom_hline(yintercept = seq(0, .3, .05), 
             color = "white", alpha = .5) + 
  labs(x = "Grupo de edades", y = "Proporción")

# Desplegar gráfica

fig2

#ggsave('../figures/figure2.pdf', fig2, 
#      scale = 1.25, width = 5.5, height = 6)