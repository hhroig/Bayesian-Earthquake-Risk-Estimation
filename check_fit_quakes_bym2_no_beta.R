## Diagnosing Rstan BYM2 fit

## Load Data (stan fit object)
library(rstan)

load("quakes_zero_inflated_fit_beta0.RData")      # stan fit!
load("zero_inflated_dataframe.RData")   # observed data in a data frame!

## Shiny through web-browser

# for Graphical Posterior Predictive Checks:
y = zero_infl_quakes$y

library(shinystan)
launch_shinystan(quakes_stanfit)

## CHECKING DIVERGENCES

library(magrittr)
library(purrr)
library(tibble)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsci)

n_chains = 3       # number of chains 
warmups = 5000     # number of warmup samples
max_treedepth = 10 # it's just the default value

my_diagnostics <- rstan::get_sampler_params(quakes_stanfit) %>% 
  set_names(1:n_chains) %>% 
  map_df(as_data_frame,.id = 'chain') %>% 
  group_by(chain) %>% 
  mutate(iteration = 1:length(chain)) %>% 
  mutate(warmup = iteration <= warmups)


my_diagnostics %>% 
  group_by(warmup, chain) %>% 
  summarise(percent_divergent = mean(divergent__ >0)) %>% 
  ggplot() +
  geom_col(aes(chain, percent_divergent, fill = warmup), position = 'dodge', color = 'black') + 
  scale_y_continuous(labels = scales::percent, name = "% Divergent Runs")  + 
  scale_fill_npg()

# Results: We see then that across all chains, we had no divergences during the sampling period (after the warm-ups), which is what we want to see!

## CHECKING TREEDEPTH PROBLEMS

my_diagnostics %>% 
  ggplot(aes(iteration, treedepth__, color = chain)) + 
  geom_line() + 
  geom_hline(aes(yintercept = max_treedepth), color = 'red') + 
  scale_color_locuszoom()

# To interpret Results: By default, max_treedepth is set to 10. So, we should check and make sure
# that our model isn’t bumping up against max_treedepth a bunch. If it is, that means that the 
# model is selecting candidate draws based on hitting this cap, rather than properties of the 
# posterior probability.

# Results: Looks like we’re good, some treedepth touches the maximum allowabe but
# is not the usual behavior...

## PARAMETER DIAGNOSTICS

print(quakes_stanfit , pars=c("beta0", "eta[1]", "eta[200]", "eta[366]",  
                               "mu[1]", "mu[22]", "mu[366]"), probs=c(0.025, 0.5, 0.975))

# Results: summary statistics on each model parameter... also included generated quantities!

# What I do really care about are: 
#  - n_eff: the effective sample size
#  - Rhat: the “Gelman and Rubin potential scale reduction statistic”

# Let's extract this summary in a different way:

quakes_summary <- summary(quakes_stanfit)$summary %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_data_frame()

quakes_summary %>% head()

# Check n_eff!

x.inter.value = n_chains*1000 # number of chains times drops after warmup!

quakes_summary %>% 
  ggplot(aes(n_eff)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = x.inter.value), color = 'red')

# Results: Most of our parameters have a fairly high n_eff, though we see a few are somewhat lower...

# Check Rhat!
# More or less Rhat tells you whether or not each of the chains has reached a stable 
# posterior distribution, despite starting at different starting values. Gelman 
# recommends that Rhat for each parameter be less than 1.1

quakes_summary %>% 
  ggplot(aes(Rhat)) + 
  geom_histogram() + 
  geom_vline(aes(xintercept = 1.1), color = 'red')

# Results: Looks like we’re good! They are all < 1.1

## CHECKING ESTIMATES!
library(stringr)

# Checking parameter estimates without using "summary"
# Warning: densities estimations included and we deal with count data!

bh_mcmc <- quakes_stanfit %>% 
  rstan::extract()

library(tidyr)
mu.response <- bh_mcmc[ 'mu'] %>% 
  map_df(as_data_frame, .id = 'variable') %>% 
  gather(observation,value, -variable)

ggplot() + 
  geom_density(data = mu.response, aes(value,fill = 'Posterior Predictive'), alpha = 0.5) + 
  geom_density(data = zero_infl_quakes, aes(y, fill = 'Observed'), alpha = 0.5)
