library(survey)
library(dplyr)
library(tidyverse)
library(magrittr)

#RDS data used for table 1. 
individuals <- readRDS("individuals.RDS")
hh_main <- readRDS("hh_main.RDS")
weights <- readRDS("final_weights.RDS")

#RDS data used for table 2.
figure3.a <- readRDS("figure3a.RDS")

#RDs data used for table 3
deaths <- readRDS("deaths.RDS")
left <- readRDS("left_df.RDS")
utilities <- readRDS("resources_df.RDS")


### Table 1 
# Data Wrangling for the first table ------------------------------------------
#Creating weight by strata with data of barrio. 
#Furthermore, creating a column named strata_weight.
weights$strata_weight <- weights$barrio_w/13 

#Left_joining data with coincidences on both id and strata; NA otherwise. 
hh_main <- left_join(hh_main, weights, by = c("id", "strata")) 
hh_main$counter <- 1 #Starting count.

# create a column with single final weight for each household 
# This, multiplying strata weight with 
hh_main$hh_w_f <- as.numeric(hh_main$hh_w) * as.numeric(hh_main$strata_weight)

# Left joining data by hh_id adquiring age and gender from the individuals. 
ind_main <- left_join(individuals, hh_main, by = "hh_id")

# Create survey design object for household.
id.form <- ~strata+id
wt.form <- ~1+hh_w_f
dsvy <- svydesign(id = id.form, weights = wt.form, data = hh_main, nest = T)


# Create survey design object for individuals in a house hould.
id.form <- ~strata+id
wt.form <- ~1+hh_w_f
dsvy2 <- svydesign(id = id.form, weights = wt.form, data = ind_main, nest = T)


# Table S1 estimates and standardized errors  -----------------------------------

#population 
    #estimate by survey
    pop.est <- svytotal(~hh_size, dsvy, na.rm = T, vartype = "se")[1] 
    pop.se <- SE(svytotal(~hh_size, dsvy)) #standardized error by survey for population
    #Saving the population estimate. 
    saveRDS(pop.est, "pop_est.RDS")

#households 
    #obtaining the estimated surveyed estimate for house hold.
    hh <- unlist(svytotal(~count, dsvy, na.rm = T))[1]
    #standardized error by survey for household.
    hh.se <- SE(svytotal(~count, dsvy, na.rm = T))

#age 
    median.age <- unlist(svyquantile(~age, dsvy2, quantiles = 0.5, na.rm=TRUE, se = TRUE))[1]

# proportion female
    #Obtaining estimated female deaths estimation by the hundreds.
    prop.female <- svymean(~gender, dsvy2, na.rm = TRUE)[2]*100
    #standardized error by survey for prop. of fem. by survey.
    fem.se <- SE(svymean(~gender, dsvy2, na.rm = TRUE))[2]*100 

# hh size 
    #Mean for the household deaths.
    mean.hh <- unlist(svymean(~hh_size, dsvy)[1])
    #Standarized error for this estimation by survey.
    size.se <-SE(svymean(~hh_size, dsvy))


# Table 1 ---------------------------------------------------------------------
tableS1 <- as.data.frame(cbind(
  c("Households", "Population", "Median Age", "Proportion Female", "Mean Household Size"),
  c(hh, pop.est, median.age, prop.female, mean.hh),
  c(hh.se, pop.se, NA, fem.se, size.se)))

#Customization for tanble S1 with z = 1.96, equivalent to a 95% confidence interval.    
names(tableS1) <- c("Variable", "WeightedEst", "SE")
tableS1$lower <- as.numeric(as.character(tableS1$WeightedEst)) - 1.96*as.numeric(as.character(tableS1$SE))
tableS1$upper <- as.numeric(as.character(tableS1$WeightedEst)) + 1.96*as.numeric(as.character(tableS1$SE))

#Saving the results for table 1. 
saveRDS(tableS1, "tableS1.RDS")

# Table 2 ----------------------------------------------------------------------

#En la tabla queremos el valor en función de data y key. 
#Además, queremos el promedio y desviación estándar de estos valores. 
tableS2 <- figure3.a %>% 
  aggregate(value~strata+key, data=., FUN=function(x) (paste (mean(x) %>% round(0), 
                                                              sd(x)%>% round(0) )))


#Data Wrangling for the third table -------------------------------------------

#analysis of medical access: at least one day without care. 
#Getting every string of access in each household. There are 11 "access." in total.
access <- names(households) %>%  
  {starts_with(match = "access_med", vars = .)} %>%
  {households[,.]} %>% 
  names

### NOTE | NOTE | NOTE | NOTE | NOTE | NOTE | NOTE | NOTE | NOTE | NOTE | NOTE 
###
### This next analysis evaluates 24 obs of about 9000+ rows, it takes time to run.
### 
### NOTE | NOTE | NOTE | NOTE | NOTE | NOTE | NOTE | NOTE | NOTE | NOTE | NOTE 

#Creating a table with row names = strings gathered in the variable access before. 
table1_access <- households %>%
  rowwise() %>%
  mutate_at(vars(access_med.no_911, access_med.no_transport, access_med.roads_damaged, 
                 access_med.facility_closed, access_med.no_doctors, access_med.no_dialysis,
                 access_med.no_resp_mach, access_med.no_meds, access_med.couldnt_afford,
                 access_med.ot_reasons), 
            funs(case_when( as.numeric(.) == 99 ~ 0, as.numeric(.) == 0 ~ 0, as.numeric(.) == 1 ~ 1,
              as.numeric(.) == 2 ~ 1, as.numeric(.) == 3 ~ 1, as.numeric(.) == 4 ~ 1,
              as.numeric(.) == 5 ~ 1, TRUE ~ 0))) %>%
  ungroup() %>% #dplyr::select indicates that you want the "select" from the dplyr library.
  dplyr::select(access_med.no_911, access_med.no_transport, access_med.roads_damaged, 
                access_med.facility_closed, access_med.no_doctors, access_med.no_dialysis,
                access_med.no_resp_mach, access_med.no_meds, access_med.couldnt_afford,
                access_med.ot_reasons, strata) #Select every "access." in household data.


#Table 3 ----------------------------------------------------------------------

#In this table we are aggregating all variables. 
table_df <- aggregate(. ~ strata, data = table1_access, FUN =mean) %>% 
  t %>% as.data.frame() 
#Next we are finding the standarized error. 
table_se <- aggregate(. ~ strata, data = table1_access, FUN = function(x) sd(x)/sqrt(length(x)))
#Finally, we are adding the confidenci interval at 95% for the statistics found. 
table_s1 <- aggregate(. ~ strata, data = table1_access,  
                      FUN = function(x) paste0(signif(mean(x),2), 
                        " (", signif(mean(x) - 1.96*(sd(x)/sqrt(length(x))),2),
                        ", ", signif(mean(x) + 1.96*(sd(x)/sqrt(length(x))),2),
                        ") ")) #The "(", ",", ")" are customization for the table.

#We are adquiring the 95% confidence intervals for the statistics in utilities.
utilities %>%
  mutate(est_ci = paste0(signif(values,2),
    " (", signif(values - 1.96*(sd/sqrt(length)),2),
    ", ", signif(values + 1.96*(sd/sqrt(length)),2),
    ")" )) -> utilities



