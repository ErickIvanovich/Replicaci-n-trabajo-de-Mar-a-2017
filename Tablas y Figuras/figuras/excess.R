source("Adjusted and Cleaned Data/base.R")

# Importar Librerías

lib_eval("magrittr")
lib_eval("dplyr")
lib_eval("survey")

# Cargar la data de los archivos RDS

hh_main <- readRDS("Adjusted and Cleaned Data/hh_main.RDS")
deaths <- readRDS("Adjusted and Cleaned Data/deaths.RDS")
deaths_official <- readRDS("Adjusted and Cleaned Data/deaths_official.RDS")
weighted_pop_est <- readRDS("Adjusted and Cleaned Data/pop_est.RDS")
adj_rates <- readRDS("Adjusted and Cleaned Data/adj_rates.RDS")

# Data cruda

deaths_after_hurricane <- 
  deaths %>% 
  subset(as.numeric(died_month) >= 10 | 
           (died_month == 9 & died_b_p_hurricane == 2)) %>%
  nrow

deaths_before_hurricane <-
  deaths %>% 
  subset(as.numeric(died_month) <= 8 | 
           (died_month == 9 & died_b_p_hurricane == 1)) %>%
  nrow


total_pop <- sum(hh_main$hh_size)

adj_pop <- (total_pop-deaths_before_hurricane) * (102/365)

rate <- deaths_after_hurricane / adj_pop

se <- sqrt(deaths_after_hurricane)/adj_pop

ll <- rate - 1.96*se

ul <- rate + 1.96*se

# Calcular muertes de exceso usando las proporciones
pop_in_2016 <- 3406520
adj_pop_2016 <- pop_in_2016*(102/365)
deaths_in_2016 <- deaths_official %>% 
  subset(Year == 2016) %>%
  dplyr::select(Sep,Oct,Nov,Dec) %>%
  mutate(Sep = Sep*(1/3)) %>% sum
deaths_after_hurricane_2016 <- deaths_in_2016 

rate_2016 <- deaths_after_hurricane_2016/adj_pop_2016

# Diferencia entre proporciones
diff <- rate - rate_2016
diff_ll <- ll - rate_2016
diff_ul <- ul - rate_2016

#Cálculos de exceso de muerte
weighted_pop_est*(102/365)*diff
weighted_pop_est*(102/365)*diff_ll
weighted_pop_est*(102/365)*diff_ul


# Cálculos ajustados
# generados en adjust-for-missing-households.RMD
adj_rate <- round(adj_rates$rate_after,1) / 1000
adj_ll <- round(adj_rates$lower_after,1) / 1000
adj_ul <- round(adj_rates$upper_after,1) / 1000

# Cálculos de exceso de muerte
weighted_pop_est*(102/365)*(adj_rate-rate_2016)
weighted_pop_est*(102/365)*(adj_ll-rate_2016)
weighted_pop_est*(102/365)*(adj_ul-rate_2016)