#Kenny J. Davila Salgado
#Figure 4

#Debe sustituir con el path donde este guardado el file "base.R"
source("Adjusted and Cleaned Data/base.R")

#Librerias a usar
lib_eval("ggplot2")
lib_eval("ggsci")
lib_eval("dplyr")
lib_eval("scales")
lib_eval("cowplot")

## estimados del plot
#Santos-Lozada y Howard (2018) : Calculando 95% CI 

# estimados de parte de Santos-Lozada y Howard (2018)
sep.exp.ll <- 2296
sep.exp.ul <- 2469

oct.exp.ll <- 2380
oct.exp.ul <- 2476

sep.exp = 2383
oct.exp = 2428


sep.ll <- 2900
sep.ul <- 3074

oct.ll <- 2995
oct.ul <- 3091

sep.est <- 2987
oct.est <- 3042

# Calculando los estrechos maximos y minimos para un 95% CI
alexis <- sep.est + oct.est - sep.exp - oct.exp 
alexis.ul <- sep.ul + oct.ul - sep.exp.ll - oct.exp.ll
alexis.ll <- sep.ll + oct.ll - sep.exp.ul - oct.exp.ul

excess_df <- data.frame(cbind(c("Estimado no ajustado de la encuesta", "Estimado ajustado de la encuesta", "Conteo de muerte oficial", "Santos-Lozada y Howard (2018)", "New York Times"),
                              c(4645,5740, 64, alexis, 1052), c(793, 1506, NA, alexis.ll, NA), c(8498,9889, NA, alexis.ul, NA)))
names(excess_df) <- c("type", "estimate", "lower", "upper")
excess_df$estimate <- as.numeric(as.character(excess_df$estimate))
excess_df$days <- 102
excess_df$days[c(4:5)] <- 41
excess_df <- excess_df[order(excess_df$days, excess_df$estimate),]
excess_df$month <- c("Sep - Oct 31","Sep - Oct 31", "Sep - Dec 31","Sep - Dec 31","Sep - Dec 31")
excess_df$month <- factor(excess_df$month, levels = excess_df$month[c(1,3)])


excess_df$lower <- as.numeric(as.character(excess_df$lower))
excess_df$upper <- as.numeric(as.character(excess_df$upper))
excess_df$type <- factor(excess_df$type, levels = excess_df$type)

# Figura 4a
point.excess.df <- ggplot(data = excess_df, aes(x = type, y = estimate, color = type)) +
  geom_point(size = 3) +
  scale_color_manual(name = "", values = rev(pal_nejm()(7))) +
  #scale_color_brewer(name = "",palette="Dark2") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width = 0.1) +
  geom_text(label = as.character(excess_df$estimate), size = 3, vjust = -0.2, nudge_x = 0.2) +
  theme_classic() +
  theme(#axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5), 
    legend.title=element_text(size = 9),
    legend.position = c(0.35, .99), 
    legend.justification = c(1, 1)) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_y_continuous("Estimado de exceso de muertes", limits = c(0, NA), expand = c(0.01,0)) +
  facet_grid(~month, scales = "free_x", space = "free_x", switch = "x") +
  theme(strip.placement = "bottom") +
  theme(panel.spacing.x = unit(1, "cm"),
        strip.background = element_blank())

#Debe sustituir con el path donde este guardado el file

### Figura 4b
deaths <- readRDS("Adjusted and Cleaned Data/deaths.RDS")

deaths_fig_df <- deaths %>%
  mutate(cod = case_when(
    cause_of_death == "---" ~ 0,
    cause_of_death == 0 ~ 0,
    cause_of_death == 10 ~ 3,
    cause_of_death >=1 & cause_of_death <= 7 ~ 1, 
    cause_of_death == 8 ~ 2,
    cause_of_death == 9 ~ 1,
    cause_of_death == 99 ~ 4
  )) %>%
  mutate(date = case_when(
    (mo >= 1 & mo <= 8) | (mo >= 10 & mo <= 12) ~ as.Date(paste0("2017-",as.integer(mo),"-15")),
    mo == 9.1 ~ as.Date("2017-09-15"),
    mo == 9.2 ~ as.Date("2017-09-25")
  )) %>% 
  dplyr::select(age, date, cod) 



deaths_fig <- ggplot(data = deaths_fig_df, aes(x = date, y = age, group = factor(cod))) + 
  geom_point(aes(shape = factor(cod), color=factor(cod)), size=3) +
  scale_x_date(labels = date_format("%b"), date_breaks = "1 month") +
  scale_shape_manual(name="Causa de Muerte",
                     breaks=c(0,1,2, 3,4),
                     values=c(16,15,17,18,19),
                     labels=c("\nNo relacionado\n al Huracán",
                              "\nDirectamante\n causada por Huracán",
                              "\nIndirectamante\n causada por Huracán",
                              "\nSuicidio",
                              "\nOtra")) +
  scale_color_manual(name="Causa de Muerte",
                     breaks=c(0,1,2, 3,4),
                     labels=c("\nNo relacionado\n al Huracán",
                              "\nDirectamante\n causada por Huracán",
                              "\nIndirectamante\n causada por Huracán",
                              "\nSuicidio",
                              "\nOtra"),
                     values=c("#0072B5FF","#BC3C29FF","#E18727FF","#20854EFF","#6F99ADFF")) + 
  xlab("Mes") + ylab("Edad (Años)") + #ggtitle("Figure 4: Household Members reported to have died in 2017 by Cause and Month") +
  geom_vline(xintercept=as.Date("2017-09-20"), colour="indianred4", linetype=2) +
  geom_text(aes(x=as.Date("2017-09-20"), label="Huracán \nMaría", y=45), 
            colour="indianred4", angle=90, size = 4) +
  geom_hline(yintercept = 79.50, colour = "grey58", linetype=2) + 
  geom_text(aes(x= as.Date("2017-04-23"),y =79.95, label ="Promedio de muertes en \nPuerto Rico (2016)"),
            colour="grey58", size = 4)+
  theme_classic()+
  theme( 
    plot.title = element_text(hjust = 0.5),
    text=element_text(size=10),panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    legend.justification = c(1, 1))



deaths_fig4 <- plot_grid(point.excess.df, deaths_fig, labels = c("A", "B"), nrow = 2)

deaths_fig4

#ggsave('Adjusted and Cleaned Data/figure4.pdf', deaths_fig4, 
#       scale = 1.25, width = 7, height = 7)
