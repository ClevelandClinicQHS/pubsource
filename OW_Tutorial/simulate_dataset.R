# Created: 2024-03-27
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Generates the simulated data set
# - Motivated by this SAS code: https://www2.stat.duke.edu/~fl35/OW/OW_survival_Demo.sas
# - Distributions generated according to https://pubmed.ncbi.nlm.nih.gov/22361330/

# Load packages
library(tibble)
library(readr)

# Set the seed for reproducibility
set.seed(44113322)

# Set the overall sample size
n <- 5000

# Generate the sample with age, sex, and ejection fraction
sim_dat <- 
  tibble(
    
    # Add the patient characteristics (observed)
    age = rnorm(n, mean = 73, sd = 6),
    male = rbinom(n, size = 1, prob = 0.68),
    ejection_fraction = rnorm(n, mean = 51, sd = 6),
    
    # Compute the TRUE propensity score (unobserved)
    log_odds_ps_true = 0.1*age + 0.7*male - 0.05*ejection_fraction - 5.75,
    ps_true = 1 / (1 + exp(-log_odds_ps_true)),
    
    # Generate the realized treatment assignment (observed)
    treated = rbinom(n, size = 1, prob = ps_true),
    
    # Compute the TRUE overlap weight for the realized treatment (unobserved)
    OW_true = treated * (1-ps_true) + (1-treated) * ps_true,
    
    ## Generate the survival time from Exponential distributions
    
    # Baseline hazard constant over time (unobserved)
    lambda = -log(0.56) / 10, # <-- This is h(t) for the CONTROL group which produces S(10) = 0.56
    
    # Define the TRUE linear predictor of confounder + treatment effects on the outcome
    log_lambda_control = log(lambda) + 0.30 * (age - mean(age)) + 0.4 * male - 0.07 * (ejection_fraction - mean(ejection_fraction)),
    log_lambda_treatment = log_lambda_control + log(0.70), # <- This is the true treatment effect of HR = 0.70 (treatment benefit)
    
    # Generate potential outcomes (survival times) under each treatment scenario (we only (kind-of) observe one of these)
    time_treat = rexp(n, rate = exp(log_lambda_treatment)),
    time_control = rexp(n, rate = exp(log_lambda_control)),
    
    # The ACTUAL event time outcome depends on the treatment ACTUALLY observed for the patient (observed, if not censored)
    actual_event_time = treated * time_treat + (1 - treated) * time_control,
    
    # Generate a random censor time (observed, if before event time)
    censor_time = runif(n, min = 0, max = 12),
    
    # Calculate the observed event or censor time (what we'd actually observe; either had the event, or censored first)
    time = pmin(actual_event_time, censor_time),
    
    # Calculate the event status (TRUE if event observed, FALSE if censored)
    event = as.numeric(actual_event_time < censor_time)
    
  ) 

## Estimate the propensity scores from the observed data

# Use a logistic regression model
ps_model <-
  glm(
    formula = treated ~ age + male + ejection_fraction,
    data = sim_dat,
    family = "binomial"
  )

# Add scores to data set
sim_dat$ps_est <- predict(ps_model, type = "response")

## Add IPTW and OW
sim_dat$IPTW_est <- with(sim_dat, treated * (1 / ps_est) + (1 - treated) * (1 / (1 - ps_est)))
sim_dat$OW_est <- with(sim_dat, treated * (1 - ps_est) + (1 - treated) * ps_est)

# Write out data set to current working directory
sim_dat |> write_rds(file = "sim_dat.rds")