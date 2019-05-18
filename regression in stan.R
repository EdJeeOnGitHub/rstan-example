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
library(tidybayes) # tidy model draws
library(tibble) # nice dataframes
library(ggridges) # density plots
library(forcats) # factor manipulation
library(shinystan) # diagnostics 

rstan_options(auto_write = TRUE)
options(mc.cores = 4)

use_precompiled_models <- FALSE

##### Data Cleaning #####

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

table_5_df %>% 
  ggplot(aes(x = first6_dep_savings)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Savings Post-Treatment")


table_5_df %>% 
  ggplot(aes(x = lntotalplus1)) +
  geom_histogram() +
  theme_minimal() +
  labs(title = "Log(Savings+1) Post-Treatment")

##### Replicating Table V using Frequentist Regression #####

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

##### Bayesian Treatment #####


square_predictors <- function(x){
  x^2
}

predictors_plus_noise <- function(x){
  N <- length(x)
  x <- x + rnorm(N)
  return(x)
}

stan_df <- table_5_df %>% 
  select(first6_dep_savings,
         bg_femalevendor,
         bg_malevendor,
         bg_educ, 
         literate_swahili,
         bg_age,
         bg_married,
         female_married,
         malevendor_married,
         rosca_contribK,
         bg_animalsvalue,
         bg_durvalue_hh, 
         per_invest_choice2,
         per_somewhat_patient, 
         per_hyperbolic,
         per_pat_now_impat_later,
         per_maximpat, 
         per_missing,
         wave2,
         wave3
  ) %>% 
  na.omit() %>% # Dropping 3 NA values
  filter(first6_dep_savings > 0) %>% 
    mutate_if(~(n_distinct(.) > 2),
            list(sq = square_predictors,
                 noise = predictors_plus_noise)) %>%  # Squaring Predictors 
  select(-first6_dep_savings_sq)
y_i <- stan_df$first6_dep_savings # For PPCs later

stan_data_list <- list(
  N = nrow(stan_df),
  K = ncol(stan_df) - 1, # accounting for Y 
  x = stan_df %>% select(-first6_dep_savings) %>% as.matrix(),
  y = stan_df$first6_dep_savings
)


##### Correlated Posterior #####

if (use_precompiled_models){
  linear_model_simple <- readRDS(file = "stan/precompiled/linear regression.rds")
} else {
  linear_model_simple <- stan_model(file = "stan/linear regression.stan")
  
}


model_draws_simple <- sampling(linear_model_simple,
                        stan_data_list,
                        chains = 4,
                        iter = 1000)
# model_draws_simple %>% launch_shinystan()

##### QR decomposition #####

##### Go through QR algebra here

if (use_precompiled_models){
  linear_model_QR <- readRDS(file = "stan/precompiled/linear regression QR.stan")
} else {
  linear_model_QR <- stan_model("stan/linear regression QR.stan")
}


model_draws_QR <- sampling(linear_model_QR,
                           stan_data_list,
                           chains = 4,
                           iter = 4000) # Large Speed improvement so set to 4k.


# model_draws_QR %>% launch_shinystan()


##### Model Results #####
term_id <- stan_df %>% 
  select(-first6_dep_savings) %>% 
  colnames() %>% 
  enframe("K", "term")

tidy_draws_QR <- model_draws_QR %>% 
  spread_draws(beta[K]) %>% 
  left_join(term_id,
            by = "K")

tidy_draws_QR %>%
  filter(!grepl("_sq", term)) %>% 
  filter(!grepl("_noise", term)) %>% 
  ggplot(aes(x = beta,
             y = term,
             fill = term)) +
  geom_density_ridges(alpha = 0.4, scale = 4) +
  xlim(-5, 5) +
  guides(fill = "none") +
  theme_ridges()

model_draws_QR %>% 
  spread_draws(beta[K]) %>% 
  filter(K <= 20) %>% 
  median_qi() %>% 
  left_join(
    term_id,
    by = "K"
  )

model_draws_QR %>% 
  spread_draws(beta[K]) %>% 
  filter(K <= 20) %>% 
  median_qi() %>% 
  left_join(
    term_id,
    by = "K"
  ) %>% 
  filter(!grepl("_sq", term)) %>% 
  filter(!grepl("_noise", term)) %>% 
  arrange(beta) %>% 
  mutate(term = factor(term),
         term = fct_reorder(term, beta)) %>% 
  ggplot(aes(x = beta,
             xmin = .lower,
             xmax = .upper,
             colour = term,
             y = term)) +
  geom_pointintervalh() +
  guides(colour = "none") +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "longdash")



##### Live Code GLMs, robust noise models and rstanarm time depending ##### 


stan_df_logit <- table_5_df %>% 
  select(first6_dep_savings,
         bg_femalevendor,
         bg_malevendor,
         bg_educ, 
         literate_swahili,
         bg_age,
         bg_married,
         female_married,
         malevendor_married,
         rosca_contribK,
         bg_animalsvalue,
         bg_durvalue_hh, 
         per_invest_choice2,
         per_somewhat_patient, 
         per_hyperbolic,
         per_pat_now_impat_later,
         per_maximpat, 
         per_missing,
         wave2,
         wave3
  ) %>% 
  na.omit() %>% # Dropping 3 NA values
  mutate(any_deposited = ifelse(first6_dep_savings > 0, 1, 0))



stan_data_list_logit <- list(
  N = nrow(stan_df_logit),
  K = ncol(stan_df_logit) - 1, # accounting for Y 
  x = stan_df_logit %>% select(-first6_dep_savings, any_depositied) %>% as.matrix(),
  y = stan_df_logit$any_deposited
)

# Either rstanarm or .stan from scratch.
