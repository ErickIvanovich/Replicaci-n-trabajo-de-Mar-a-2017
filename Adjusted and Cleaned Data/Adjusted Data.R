library(tidyverse)
library(lubridate)

#Loading data
deaths <- readRDS("deaths.RDS")
households <- readRDS("hh_main.RDS")
individuals <- readRDS("ind_hh.RDS")
official <- readRDS("official_long.RDS")
load("ACS2016.Rdata")

#Comparing the range of time during and after the hurricane in terms of fractions 
#of the 2017 year.
days_before <-  difftime(ymd("2017-09-20"), ymd("2016-12-31"), "days") %>% 
  as.numeric() 
time_before <- days_before/365
time_after <- (365 - days_before)/365

#Gathering household id and sizes.
household_id_size <- households %>% 
  dplyr::select(hh_id, hh_size) %>%  #Selecting the household id and size columns.
  filter(!is.na(hh_size) & hh_size>0)  #Filtering for NA's and negative numbers.

#Gathering household death 
household_deaths <- deaths %>%
  mutate(death_after = mo > 9.1 ) %>%
  group_by(hh_id) %>%
  summarize(tot_before = sum(!death_after), tot_after = sum(death_after)) %>%
  ungroup() #Summing the deaths before and after. Then, grouping them. 

#Creating the id a number variable in both dataframes.
household_deaths$hh_id <- as.numeric(household_deaths$hh_id)
household_id_size$hh_id <- as.numeric(household_id_size$hh_id)

#Calculating the death rates in all household sizes.
rateshh_size <- left_join(household_id_size, household_deaths, by = "hh_id") %>%
  mutate(household_size = cut(hh_size, c(0,1,2,4,Inf), labels = c("1","2","3-4","5+"))) %>%
  group_by(household_size) %>% #grouping by hh size.
  summarize(total_households = n(), N = sum(hh_size), deaths_before = sum(tot_before, na.rm = T),
            deaths_after = sum(tot_after, na.rm = T)) %>% #Summing different variables.
  mutate(rate_before = deaths_before/N*1000 / years_before,
         rate_after = deaths_after/N*1000 / years_after) #rates by the thousands.
rateshh_size %>% knitr::kable()

#Adjusting for the single person household. 
#Using the mortality data before and after Maria we can indirectly estimate this rate.
population_by_year <- readRDS("deaths_official.RDS") %>%dplyr::select(Year, Popv17)
names(population_by_year) <- c("year","pop")
days_month <- c(31,28,31,30,31,30,31,31,30,31,30,31)

#Using the population by year data and testing every month it is easier to approximate
# the real rate of deaths on a sigle person household. 
offi_rates <- official %>% left_join(population_by_year, by = "year") %>%
  mutate(days = days_month[month]) %>% #Inputing days on a month.
  mutate(days = ifelse(year %% 4 == 0 & month ==2, days + 1, days)) %>%
  mutate(rate = deaths / pop * 365/days * 1000) %>% #Calculating the rate.
  group_by(year) %>% #Grouping the data and adding the rates before, and during sept. 2017.
  summarize(before_rate = sum(deaths*(month<9) + deaths*(month==9)*2/3) / pop[1] * 1000 *
              sum(days)/(sum(days*(month<9))+20),
            after_rate = sum(deaths*(month>9) + deaths*(month==9)*1/3) / pop[1] * 1000 *
              sum(days)/(sum(days*(month>9))+10))

#Looking at the confidence intervals of the population estimate starting with 
# data from 2016.
pr_pop <- population_by_year %>% subset(year == 2016) %>% {.$pop}
res <- rateshh_size %>% summarize(survey_deaths = sum(deaths_after), 
            N = sum(N), rate = round(survey_deaths/N*1000/years_after,1))

#Recovering the adjusted rates for each household size given the estimations for
# household size = 1.
rateshh_size_adj <- rateshh_size
rateshh_size_adj$rate_before[1] <- rateshh_size_adj$rate_after[1] <- official_rates %>%
  filter(year == 2017) %>% .$before_rate
rateshh_size_adj %>% knitr::kable()

#Accounting for the probability of answering a survey. 
#Since Prob(answering survey | household > 1) > Prob(answering survey | household = 1) 
household_dist <- acs.hh_size %>% 
  mutate(hh_size = cut(hh_size, c(0,1,2,4,Inf), labels = c("1","2","3-4","5+"))) %>%
  group_by(hh_size)%>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  mutate(pop_freq = count / sum(count)) #Calculating the probability/ frequency.

#Creating a dataframe with all estimations and adjusted values. Before and after María.
res_2 <- rateshh_size_adj %>%  left_join(household_dist, by = c("household_size" = "hh_size")) 
res_2 %>% #Summarizing to acquire the rate estimation and its standardized errors. 
  summarize(rate_before = sum(rate_before*pop_freq), 
            se_before = sum(rate_before*pop_freq)/sqrt(sum(deaths_before)),
            rate_after = sum(rate_after*pop_freq), 
            se_after = sum(rate_after*pop_freq)/sqrt(sum(deaths_after))) %>%
  #Creating the 95% confidence intervals for the estimated statistics. 
  mutate(lower_before = rate_before - 1.96*se_before, 
         upper_before = rate_before+ 1.96*se_before,
         lower_after = rate_after - 1.96*se_after, 
         upper_after = rate_after+ 1.96*se_after) -> adj_rates

saveRDS(adj_rates, "adj_rates.RDS")
t(adj_rates)
