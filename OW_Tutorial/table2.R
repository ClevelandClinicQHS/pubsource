# Created: 2024-04-02
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Hazard ratio estimates

# Load packages
library(tidyverse)
library(survival)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

sim_dat |> 
  
  # Indicate crude weight
  add_column(Unadjusted = 1) |>
  
  # Send weights down the rows
  pivot_longer(
    cols = c(Unadjusted, IPTW_est, OW_est),
    names_to = "Type",
    values_to = "Weight"
  ) |>
  
  # Keep a few columns
  select(
    Type,
    Weight,
    treated,
    time,
    event
  ) |>
  
  # Add the observed counterfactuals to get the true ATE/ATO
  bind_rows(
    sim_dat |>
      
      # Add the weight for ATE
      add_column(
        IPTW_true = 1
      ) |>
      
      # Send the true weights down the rows
      pivot_longer(
        cols = c(IPTW_true, OW_true),
        names_to = "Type",
        values_to = "Weight"
      ) |>
      
      # Send counterfactual event times down the rows
      pivot_longer(
        cols = c(time_treat, time_control),
        names_to = "Treated",
        values_to = "Time"
      ) |>
      
      # Add some columns
      mutate(
        Time = pmin(Time, censor_time),
        Event = as.numeric(Time < censor_time),
        Treated = 
          case_when(
            Treated == "time_treat" ~ 1,
            TRUE ~ 0
          )
      ) |>
      
      # Keep a few columns
      select(
        Type,
        Weight,
        treated = Treated,
        time = Time,
        event = Event
      )
  ) |>
  
  mutate(
    Weight = Weight / sum(Weight),
    .by = c(Type, treated)
  ) |>
  
  # Nest by type
  nest(.by = Type) |>
  
  # Obtain the HR's
  mutate(
    model = 
      data |>
      map(
        function(.dat) {
          
          # Fit the models
          temp_mod_cox <- 
            coxph(
              formula = Surv(time, event) ~ factor(treated),
              data = .dat,
              weights = Weight,
              robust = TRUE
            )
          temp_mod_exp <- 
            survreg(
              formula = Surv(time, event) ~ factor(treated),
              data = .dat,
              weights = Weight,
              robust = TRUE,
              dist = "exponential"
            )
          
          # Get the estimates
          temp_est_cox <- temp_mod_cox$coefficients[[1]]
          temp_est_exp <- temp_mod_exp$coefficients[[2]]
          
          # Get the CIs
          temp_ci_cox <- confint(temp_mod_cox)
          
          # Return in a data frame
          tibble(
            Estimate = temp_est_cox,
            Lower = temp_ci_cox[[1]],
            Upper = temp_ci_cox[[2]],
            ExpHR = -1*temp_est_exp
          ) 
          
        }
      )
  ) |>
  
  # Unnest the data
  select(-data) |>
  unnest(cols = model) |>
  
  # Clean-up
  mutate(
    across(
      -Type,
      \(x) round(exp(x), 2)
    ),
    Summary = 
      case_when(
        str_detect(Type, "_true$") ~ as.character(Estimate),
        TRUE ~ paste0(Estimate, " (", Lower, ", ", Upper, ")")
      ),
    Type = 
      case_match(
        Type,
        "Unadjusted" ~ "Unadjusted",
        "IPTW_est" ~ "IPTW-adjusted",
        "OW_est" ~ "OW-adjusted",
        "IPTW_true" ~ "True ATE",
        "OW_true" ~ "True ATO"
      ) |>
      factor() |>
      fct_relevel(
        "True ATE",
        "True ATO",
        "Unadjusted",
        "IPTW-adjusted",
        "OW-adjusted"
      )
  ) |>
  arrange(Type) |>
  select(
    Type,
    Summary
  ) |>
  
  # Make a table
  knitr::kable(
    format = "html"
  ) |> 
  kableExtra::kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "responsive")
  )