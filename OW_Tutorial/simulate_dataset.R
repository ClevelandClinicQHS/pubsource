# Created: 2024-07-16
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Creates the analysis data set

# Load packages
library(tidyverse)

# Set the seed for reproducibility
set.seed(44113322)

# Set the overall sample size (off + on pump)
n <- 3424

# Generate the sample
sim_dat <-
  tibble(
    
    # Add the patient characteristics according to Benedetto et al
    Age = rnorm(n, 73.5, 7),
    Female = rbinom(n, 1, .285),
    NYHA = rbinom(n, 1, .422),
    MI = rbinom(n, 1, .384),
    PCI = rbinom(n, 1, .052),
    IDDM = rbinom(n, 1, .095),
    Smoking = rbinom(n, 1, .1),
    Creatine = rbinom(n, 1, .0584),
    COPD = rbinom(n, 1, .152),
    CVA = rbinom(n, 1, .069),
    PVD = rbinom(n, 1, .260),
    NVD = sample(c(1,2,3), n, replace = TRUE, prob = c(.042, .204, .754)),
    LMD = rbinom(n, 1, .314),
    LVEF = rbinom(n, 1, .166),
    CardiogenicShock = rbinom(n, 1, .026),
    IABP = rbinom(n, 1, .040),
    Emergency = rbinom(n, 1, .059),
    BMI = rnorm(n, 27, 5),
    YOP = rnorm(n, 2005.48, 5) |> round(),
    Trainee = rbinom(n, 1, .248),
    
    # Compute the TRUE propensity score (unobserved)
    log_odds_ps_true = 
      1.5 * (Age - mean(Age)) / sd(Age) + 
      .5 * Female +
      1.25 * NYHA + 
      1.25 * MI +
      1.25 * PCI + 
      1.25 * IDDM + 
      1.25 * Smoking + 
      1.25 * Creatine + 
      1.25 * COPD + 
      1.25 * CVA + 
      1.25 * PVD + 
      .5 * (NVD - mean(NVD)) / sd(NVD) + 
      1.25 * LMD + 
      LVEF + 
      CardiogenicShock + 
      IABP + 
      Emergency + 
      .01 * (BMI - mean(BMI)) / sd(BMI) + 
      -.05 * (YOP - mean(YOP)) / sd(YOP) + 
      .5 * Trainee + 
      -2.7,
    ps_true = 1 / (1 + exp(-log_odds_ps_true)),
    
    # Generate the realized treatment assignment (observed)
    treated = rbinom(n, size = 1, prob = ps_true),
    
    # Compute the TRUE overlap weight for the realized treatment (unobserved)
    OW_true = treated * (1-ps_true) + (1-treated) * ps_true,
    
    # Define the TRUE linear predictor of confounder + treatment effect on the outcome
    log_odds_outcome_control =
      .75 * (Age - mean(Age)) / sd(Age) + 
      .5 * Female +
      .5 * NYHA + 
      .5 * MI +
      PCI + 
      IDDM + 
      Smoking + 
      Creatine + 
      COPD + 
      CVA + 
      PVD + 
      .5 * (NVD - mean(NVD)) / sd(NVD) + 
      LMD + 
      LVEF + 
      CardiogenicShock + 
      IABP + 
      Emergency + 
      .1 * (BMI - mean(BMI)) / sd(BMI) + 
      -.05 * (YOP - mean(YOP)) / sd(YOP) + 
      .5 * Trainee + 
      -5,
    log_odds_outcome_treatment = log_odds_outcome_control - 0.25,
    
    # Generate potential outcomes under each treatment scenario 
    death_control = rbinom(n, 1, 1 / (1 + exp(-log_odds_outcome_control))),
    death_treatment = rbinom(n, 1, 1 / (1 + exp(-log_odds_outcome_treatment))),
      
    # Identify the observed outcome
    death = treated * death_treatment + (1 - treated) * death_control
    
  )

## Estimate the propensity scores from the observed data

# Use a logistic regression model
ps_model <-
  glm(
    formula = treated ~ .,
    data = sim_dat |> select(-c(log_odds_ps_true, ps_true, OW_true, log_odds_outcome_control, log_odds_outcome_treatment, death_control, death_treatment, death)),
    family = "binomial"
  )

# Add scores to data set
sim_dat$ps_est <- predict(ps_model, type = "response")

## Add IPTW and OW
sim_dat$IPTW_est <- with(sim_dat, treated * (1 / ps_est) + (1 - treated) * (1 / (1 - ps_est)))
sim_dat$OW_est <- with(sim_dat, treated * (1 - ps_est) + (1 - treated) * ps_est)

# Write out data set to current working directory
sim_dat |> write_rds(file = "sim_dat.rds")
sim_dat |> write_csv(file = "sim_dat.csv")