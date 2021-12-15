library(survey)
library(dplyr)

#RDS data used for tables 1 to 3. 
individuals <- readRDS("individuals.RDS")
hh_main <- readRDS("hh_main.RDS")
weights <- readRDS("final_weights.RDS")

### Table 1 
# Data Wrangling for the first table ------------------------------------------
#Creating weight by strata with data of barrio. 
#Furthermore, creating a columnnamed strata_weight.
weights$strata_weight <- weights$barrio_w/13 

#Left_joining data with coincidences on both id and strats; NA otherwise. 
hh_main <- left_join(hh_main, weights, by = c("id", "strata")) 
hh_main$counter <- 1 #Starting count.

# create a column with single final weight for each household 
# This, multiplying strata weight with 
hh_main$hh_w_f <- as.numeric(hh_main$hh_w) * as.numeric(hh_main$strata_weight)

# Left joining data by hh_id adquiring age and gender from the individuals. 
ind_main <- left_join(individuals, hh_main, by = "hh_id")

# create survey design object
id.form <- ~strata+id
wt.form <- ~1+hh_w_f
dsvy <- svydesign(id = id.form, weights = wt.form, data = hh_main, nest = T)


# create survey design object
id.form <- ~strata+id
wt.form <- ~1+hh_w_f
dsvy2 <- svydesign(id = id.form, weights = wt.form, data = ind_main, nest = T)


# Table S1 estimates and standarized errors  -----------------------------------

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

#Customization for tanble S1 with z = 1.96 confidence interval.    
names(tableS1) <- c("Variable", "WeightedEst", "SE")
tableS1$lower <- as.numeric(as.character(tableS1$WeightedEst)) - 1.96*as.numeric(as.character(tableS1$SE))
tableS1$upper <- as.numeric(as.character(tableS1$WeightedEst)) + 1.96*as.numeric(as.character(tableS1$SE))

#Saving the results for table 1. 
saveRDS(tableS1, "tableS1.RDS")

#Data Wrangling for the second table ------------------------------------------
#Table 2

