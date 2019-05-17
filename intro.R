## This script accompanies the slides used in the first presentation 


##---------------------------------------------------
library(dplyr)
school_data <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
school_data %>% 
  as_tibble() %>% 
  mutate(school = row_number()) %>% 
  select(-J) 


##---------------------------------------------------
library(rstan)
options(mc.cores = parallel::detectCores()) # How many cores to use. 
rstan_options(auto_write = TRUE) # Saves time on model re-compilation.
set.seed(1234)

# Compile the model
eight_school_centred_stan <- stan_model(file = "stan/eight_school_centred.stan")
# Fit the model
model_draws_centred <- sampling(eight_school_centred_stan,
                        data = school_data,
                        iter = 2000,
                        chains = 4)

##------------------------------------------
library(shinystan)
launch_shinystan(model_draws_centred)


##--------------------------------------------------------
eight_school_non_centred_stan <- stan_model(file = "stan/eight_school_non_centred.stan")
model_draws_ncp <- sampling(eight_school_non_centred_stan,
                        data = school_data,
                        iter = 2000,
                        chains = 4,
                        control = list(adapt_delta = 0.95))

##----------------------------------------------------
library(tidybayes)
library(purrr)

dt_c <- model_draws_centred %>% # centred data 
  spread_draws(theta[j], tau) %>% # tidybayes grabbing the variables we want 
  filter(j == 1) # Just theta[1]

sampler_params <- get_sampler_params(model_draws_centred,  inc_warmup=FALSE) %>%
  map_df(as_tibble) # This gives us the MCMC diagnostics

dt_c <- bind_cols(dt_c,
                  sampler_params) %>% 
  mutate(divergent = factor(divergent__),
         model = "Centred") # Adding the draws and diagnostics together in one df

# Same with the NCP model:
dt_ncp <- model_draws_ncp %>% 
  spread_draws(theta[j], tau) %>% 
  filter(j == 1)
sampler_params_ncp <-  get_sampler_params(model_draws_ncp,  inc_warmup=FALSE) %>%
  map_df(as_tibble)


dt_ncp <- bind_cols(dt_ncp,
                  sampler_params_ncp) %>% 
  mutate(divergent = factor(divergent__),
         model = "NCP")

dt_plot <- bind_rows(dt_c,
                     dt_ncp) # Combining NCP and Centred model for plotting
dt_plot %>% 
  ggplot(aes(x = theta, y = log(tau), colour = divergent, alpha = divergent)) +
  geom_point() +
  scale_color_manual(values = c("grey","red")) +
  theme_minimal() +
  facet_wrap(~model) 

##--------------------------------------------------------
plot(model_draws_ncp, pars = "theta") # Default plotting

## ------------------------------------------------------------------------
tidy_no_pooling <- school_data %>% # Creating a df with just original y and adding
  as_tibble() %>%  # confidence intervals
  mutate(j = factor(row_number()),
         conf.high = y + sigma*1.96,
         conf.low = y - sigma*1.96,
         model = "No Pooling") %>% 
  select(-J,
         theta = y)

## ---- echo = TRUE, eval = FALSE------------------------------------------
library(ggstance)

tidy_ncp_draws <- model_draws_ncp %>% 
  spread_draws(theta[j])  # Tidy model draws.

tidy_ncp_draws %>% 
  median_qi() %>% 
  to_broom_names() %>% 
  mutate(model = "BHM",
         j = factor(j)) %>% 
  bind_rows(tidy_no_pooling) %>% # Adding original data
  ggplot(aes(x = theta, xmin = conf.low, xmax = conf.high,
             y = j,
             colour = model)) +
  geom_pointrangeh(position = position_dodgev(height = 0.3)) +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "longdash") 

## ------------------------------------------------------------------------
library(ggridges)
tidy_ncp_draws %>% 
  ungroup() %>% 
  mutate(j = factor(j)) %>% 
  ggplot(aes(x = theta,
             y = j,
             fill = j)) +
  stat_density_ridges(scale = 2,
                      quantile_lines = TRUE,
                      quantiles = c(0.025,
                                    0.5,
                                    0.975),
                      alpha = 0.7) +
  theme_ridges() +
  labs(caption = "Note: 95% credibility intervals and posterior median displayed.")

## ggmcmc version -------------------------------------------------------
## # ggmcmc creates PDF with a bunch of diagnostic plots similar to shinystan
## library(ggmcmc)
## gg_s <- ggs(model_draws_ncp)
## ggmcmc(gg_s)


##-----------------------------
library(bayesplot)
bayesplot_draws <- extract(model_draws_ncp) # grabbing draws for bayesplot
ppc_dens_overlay(y = tidy_no_pooling$theta,
                 yrep = bayesplot_draws$Y_rep[1:500, ]) # Only show 500 reps

##--------------------------------------------------------
ppc_stat(tidy_no_pooling$theta,
         bayesplot_draws$Y_rep[1:500, ],
         stat = "mean")


##--------------------------------------------------------
ppc_stat(tidy_no_pooling$theta,
         bayesplot_draws$Y_rep[1:500, ],
         stat = "min")


##--------------------------------------------------------

ppc_stat(tidy_no_pooling$theta,
         bayesplot_draws$Y_rep[1:500, ],
         stat = "max")

