# Created: 2024-03-27
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Generates the simulated data set
# - Motivated by this SAS code: https://www2.stat.duke.edu/~fl35/OW/OW_survival_Demo.sas

# Load packages
library(tibble)
library(readr)

# Set the seed for reproducibility
set.seed(44113322)

# Set the overall sample size
n <- 5000

# Age and sex are independent
age <- rnorm(n, mean = 50, sd = 10)
male <- rbinom(n, size = 1, prob = 0.6)

# Income is positively associated with age + sex
income <- pmax(rnorm(n = n, age + 10 * male, sd = 13), 0)

# Generate the sample with age, sex, and income
sim_dat <- 
  tibble(
    
    # Add the patient characteristics (observed)
    age = age,
    male = male,
    income = income,
    
    # Compute the TRUE propensity score (unobserved)
    log_odds_ps_true = 0.03*age - 0.7*male + 0.025*income - 2.5,
    ps_true = 1 / (1 + exp(-log_odds_ps_true)),
    
    # Generate the realized treatment assignment (observed)
    treated = rbinom(n, size = 1, prob = ps_true),
    
    # Compute the TRUE overlap weight for the realized treatment (unobserved)
    OW_true = treated * (1-ps_true) + (1-treated) * ps_true,
    
    
    ## Generate the survival time from Weibull distributions
    
    # Baseline hazard constant over time (unobserved)
    lambda = 1*365/0.09,
    
    # Define TRUE linear predictor (unobserved)
    phi = 0.20 * age - 0.22 * male - 0.007 * income - 3,
    scale_treat = lambda * exp(-(phi - 0.36)), # THIS IS THE TRUE CAUSAL TREATMENT EFFECT
    scale_control = lambda * exp(-phi),
    
    # Generate potential outcomes (survival times) under each treatment scenario (we only (kind-of) observe one of these)
    time_treat = rweibull(n, shape = 1, scale = scale_treat),
    time_control = rweibull(n, shape = 1, scale = scale_control),
    
    # The ACTUAL event time outcome depends on the treatment ACTUALLY observed for the patient (observed, if not censored)
    actual_event_time = treated * time_treat + (1 - treated) * time_control,
    
    # Generate a random censor time (observed, if before event time)
    censor_time = runif(n),
    
    # Calculate the observed event or censor time (what we'd actually observe; either had the event, or censored first)
    time = 10 * pmin(actual_event_time, censor_time),
    
    # Calculate the event status (TRUE if event observed, FALSE if censored)
    event = as.numeric(actual_event_time < censor_time)
    
  ) 

# Write out data set to current working directory
sim_dat |> write_rds(file = "sim_dat.rds")