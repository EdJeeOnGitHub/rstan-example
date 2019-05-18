## Bayesian Treatment of Savings Constraints and Microenterprise Development:
## Evidence from a Field Experiment in Kenya.
## Dupas, Pascaline; Robinson, Jonathan, 2015 

## Dataverse download link https://doi.org/10.7910/DVN/EXL9DX

library(dplyr) # data manipulation
library(ggplot2) # plotting
library(rstan) # stan
library(haven) # reading .dta files
library(tidyr) # cleaning data
library(broom) # pretty regression output

rstan_options(auto_write = TRUE)
options(mc.cores = 4)

use_precompiled_models <- FALSE

##### Data
savings_df <- read_dta(file = "data/dataset_savingsAEJ.dta") %>% 
  mutate(active = ifelse(first6_num_trans_savings > 1, TRUE, FALSE),
         treatment_bg_boda = treatment*bg_boda,
         active_bg_boda = active*bg_boda,
         bg_boda_wave2 = (bg_boda & wave2),
         bg_malevendor_wave2 = (bg_malevendor & wave2),
         bg_malevendor_wave3 = (bg_malevendor & wave3),
         treament_bg_malevendor = treatment*bg_malevendor,
         active_bg_malevendor = active*bg_malevendor,
         literate_swahili = ifelse((bg_kis_read == 1 & bg_kis_write ==1),
                                   1,
                                   NA),
         literate_swahili = ifelse((bg_kis_read ==0 | bg_kis_write == 0), 
                                   0,
                                   literate_swahili),
         wave = case_when(
           wave1 == 1 ~ 1,
           wave2 == 1 ~ 2,
           wave3 == 1 ~ 3
         ))

table_5_df <- savings_df %>% 
  filter(!(not_traced_account_opening == 1)) %>% 
  mutate(first6_dep_savingsK = first6_dep_savings / 1000,
         rosca_contribK = bg_rosca_contrib_lyr / 1000,
         bg_animalsvalue  = bg_animalsvalue  / 1000,
         bg_durvalue_hh  =bg_durvalue_hh  /1000,
         bg_totalinc_lastweek  =bg_totalinc_lastweek  /1000,
         per_invest_choice2=per_invest_choice2/100,
         per_missing = (is.na(per_invest_choice2)))


na_to_clean <-  c("per_invest_choice2",
                  "per_somewhat_patient",
                  "per_hyperbolic",
                  "per_maximpat",
                  "per_pat_now_impat_later",
                  "per_time_consistent")
table_5_df <- table_5_df %>% 
  mutate_at(na_to_clean,
            replace_na,
            0) %>% 
  mutate(malevendor_married = bg_malevendor * bg_married,
         female_married = abs(1-bg_gender)*bg_married,
         lntotalplus1 = ifelse(treatment == 1, log(first6_dep_savings + 1), NA))


y_i <- table_5_df %>% 
  filter(treatment == 1) %>% 
  select(first6_dep_savings) %>% 
  pull() 

model_1 <- table_5_df %>% 
  filter(treatment == 1) %>% 
  lm(data = ., 
     formula = lntotalplus1 ~ bg_femalevendor + bg_malevendor + wave2 + wave3) 


model_2 <- table_5_df %>% 
  filter(treatment == 1) %>% 
  lm(data = .,
     formula = lntotalplus1 ~
       bg_femalevendor + 
       bg_malevendor +
       bg_educ +
       literate_swahili +
       bg_age + 
       bg_married +
       female_married +
       malevendor_married +
       rosca_contribK +
       bg_animalsvalue +
       bg_durvalue_hh +
       wave2 +
       wave3)


model_3 <- table_5_df %>% 
  filter(treatment == 1) %>% 
  lm(data = .,
     formula = lntotalplus1 ~
       bg_femalevendor +
       bg_malevendor +
       bg_educ + 
       literate_swahili +
       bg_age +
       bg_married +
       female_married +
       malevendor_married +
       rosca_contribK +
       bg_animalsvalue +
       bg_durvalue_hh + 
       wave2 + 
       wave3 +
       per_invest_choice2 +
       per_somewhat_patient + 
       per_hyperbolic +
       per_pat_now_impat_later  +
       per_maximpat + 
       per_missing)

model_1 %>% 
  tidy()


model_2 %>% 
  tidy() %>% 
  filter(p.value < 0.05)
model_3 %>% 
  tidy() %>% 
  filter(p.value < 0.05)

#####

stan_df <- table_5_df %>% 
  select(first6_dep_savings,
         bg_femalevendor ,
         bg_malevendor ,
         bg_educ , 
         literate_swahili ,
         bg_age ,
         bg_married ,
         female_married ,
         malevendor_married ,
         rosca_contribK ,
         bg_animalsvalue ,
         bg_durvalue_hh , 
         per_invest_choice2 ,
         per_somewhat_patient , 
         per_hyperbolic ,
         per_pat_now_impat_later  ,
         per_maximpat , 
         per_missing
  ) %>% 
  na.omit() %>% 
  mutate(constant = 1,
         first6_dep_savings = first6_dep_savings + 1)  




