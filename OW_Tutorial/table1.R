# Created: 2024-07-17
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Creates Table 1

# Load packages
library(tidyverse)
library(PSweight)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

## Estimate the treatment effects

# Set the PS formula
ps_model_formula <- as.formula(paste0("treated~", paste(names(sim_dat)[1:20], collapse = "+")))

# Robust SE's
unaj_robust <- 
  PSweight(
    ps.formula = treated ~ 1,
    trtgrp = "treated",
    yname = "death",
    data = sim_dat |> as.data.frame()
  )
ipw_robust <- 
  PSweight(
    ps.formula = ps_model_formula,
    trtgrp = "treated",
    yname = "death",
    data = sim_dat |> as.data.frame(),
    weight = "IPW"
  )
ow_robust <- 
  PSweight(
    ps.formula = ps_model_formula,
    trtgrp = "treated",
    yname = "death",
    data = sim_dat |> as.data.frame(),
    weight = "overlap"
  )

# Bootstrapping
set.seed(1234)
B <- 100
unaj_boot <- 
  PSweight(
    ps.formula = treated ~ 1,
    trtgrp = "treated",
    yname = "death",
    data = sim_dat |> as.data.frame(),
    bootstrap = TRUE,
    R = B
  )
ipw_boot <- 
  PSweight(
    ps.formula = ps_model_formula,
    trtgrp = "treated",
    yname = "death",
    data = sim_dat |> as.data.frame(),
    weight = "IPW",
    bootstrap = TRUE,
    R = B
  )
ow_boot <- 
  PSweight(
    ps.formula = ps_model_formula,
    trtgrp = "treated",
    yname = "death",
    data = sim_dat |> as.data.frame(),
    weight = "overlap",
    bootstrap = TRUE,
    R = B
  )

# Coalesce the OR estimates
all_ors <- 
  tibble(
    Weight = c("Unadj", "IPTW", "OW", "Unadj", "IPTW", "OW"),
    SE = c("Robust", "Robust", "Robust", "Bootstrap", "Bootstrap", "Bootstrap")
  ) |>
  
  # Format the output
  mutate(
    Model = 
      list(unaj_robust, ipw_robust, ow_robust, unaj_boot, ipw_boot, ow_boot) |>
      
      # For each model
      map(
        function(.mod) {
          
          # Extract the summary
          temp_sum <- summary(.mod, type = "OR")$estimates
          
          # Extract estimates
          tibble(
            Estimate = temp_sum[, "Estimate"],
            Lower = temp_sum[, "lwr"],
            Upper = temp_sum[, "upr"]
          )
          
        }
      )
  ) |>
  
  # Unnest the data
  unnest(cols = Model) |>
  
  # Convert to OR
  mutate(
    across(
      c(Estimate, Lower, Upper),
      \(x) sprintf("%.2f", exp(x))
    ),
    Summary = paste0(Estimate, " (", Lower, ", ", Upper, ")")
  )

# Differences in proportions
all_diffs <- 
  tibble(
    Weight = c("Unadj", "IPTW", "OW", "Unadj", "IPTW", "OW"),
    SE = c("Robust", "Robust", "Robust", "Bootstrap", "Bootstrap", "Bootstrap")
  ) |>
  
  # Format the output
  mutate(
    Model = 
      list(unaj_robust, ipw_robust, ow_robust, unaj_boot, ipw_boot, ow_boot) |>
      
      # For each model
      map(
        function(.mod) {
          
          # Extract the summary
          temp_sum <- summary(.mod, type = "DIF")$estimates
          
          # Extract estimates
          tibble(
            Estimate = temp_sum[, "Estimate"],
            Lower = temp_sum[, "lwr"],
            Upper = temp_sum[, "upr"]
          )
          
        }
      )
  ) |>
  
  # Unnest the data
  unnest(cols = Model) |>
  
  # Convert to OR
  mutate(
    across(
      c(Estimate, Lower, Upper),
      \(x) sprintf("%.1f", 100 * x)
    ),
    Summary = paste0(Estimate, " (", Lower, ", ", Upper, ")")
  )

## Build the table

# Put data into long format
long_dat <-
  sim_dat |>
  
  # Add unadjusted weight
  add_column(Raw = 1) |>
  
  # Send the weights down
  pivot_longer(
    cols = c(Raw, IPTW_est, OW_est),
    names_to = "Method",
    values_to = "Weight"
  ) |>
  
  # Normalize the weight
  mutate(
    Weight = Weight / sum(Weight),
    .by = c(Method, treated)
  ) |>
  
  # Send the covariates down the rows
  pivot_longer(
    cols = c(Age:Trainee, death),
    names_to = "Factor",
    values_to = "Value"
  ) 

long_dat |>
  
  # Compute the summaries
  summarize(
    Mean = sum(Value * Weight) / sum(Weight),
    N = round(n() * Mean),
    SD = sd(Value),
    .by = 
      c(
        Factor,
        Method,
        treated
      )
  ) |> 
  
  # Clean up
  mutate(
    treated = case_when(treated == 1 ~ "Off-pump", TRUE ~ "On-pump"),
    Summary = 
      case_when(
        Factor %in% c("Age", "NVD", "BMI", "YOP") ~ paste0(round(Mean), " (", round(SD), ")"),
        TRUE ~ paste0(N, " (", round(100 * Mean, 1), ")")
      )
  ) |> 
  
  # Send over the columns
  pivot_wider(
    names_from = treated,
    values_from = Summary,
    id_cols = c(Factor, Method)
  ) |>
  
  # Join to get the SMD's
  inner_join(
    y = 
      long_dat |>
      
      
      # Compute the means
      summarize(
        SMD = smd::smd(Value, g = treated, w = Weight, na.rm = TRUE)$estimate,
        .by = 
          c(
            Factor,
            Method
          )
      ) |> 
      
      # Round to 3 digits
      mutate(
        SMD = round(SMD, 3) |> as.character()
      ),
    by = c("Factor", "Method")
  ) |>
  
  # Send over columns
  pivot_wider(
    names_from = Method,
    values_from = -c(Factor, Method)
  ) |>
  
  # Add the row showing sample size
  add_row(
    Factor = "Total Cases",
    `On-pump_Raw` = sum(sim_dat$treated == 0) |> as.character(),
    `On-pump_IPTW_est` = sum(sim_dat$treated == 0) |> as.character(),
    `On-pump_OW_est` = sum(sim_dat$treated == 0) |> as.character(),
    `Off-pump_Raw` = sum(sim_dat$treated) |> as.character(),
    `Off-pump_IPTW_est` = sum(sim_dat$treated) |> as.character(),
    `Off-pump_OW_est` = sum(sim_dat$treated) |> as.character(),
    SMD_Raw = "",
    SMD_IPTW_est = "",
    SMD_OW_est = "",
    .before = 1
  ) |>
  
  # Rearrange
  select(
    Factor,
    
    # Original sample
    `Off-pump` = `Off-pump_Raw`,
    `On-pump` = `On-pump_Raw`,
    SMD = SMD_Raw,
    
    # IPTW
    `Off-pump ` = `Off-pump_IPTW_est`,
    `On-pump ` = `On-pump_IPTW_est`,
    `SMD ` = SMD_IPTW_est,
    
    # OW
    `Off-pump  ` = `Off-pump_OW_est`,
    `On-pump  ` = `On-pump_OW_est`,
    `SMD  ` = SMD_OW_est,
  ) |>
  
  # Bind OR estimates to table
  bind_rows(
    all_ors |> 
      
      # Send over the columns
      pivot_wider(
        names_from = Weight,
        values_from = Summary,
        id_cols = SE
      ) |>
      
      # Rename for conformity
      select(
        Factor = SE,
        `Off-pump` = Unadj,
        `Off-pump ` = IPTW,
        `Off-pump  ` = OW
      )
  ) |>
  
  # Bind diff estimates
  bind_rows(
    all_diffs |> 
      
      # Send over the columns
      pivot_wider(
        names_from = Weight,
        values_from = Summary,
        id_cols = SE
      ) |>
      
      # Rename for conformity
      select(
        Factor = SE,
        `Off-pump` = Unadj,
        `Off-pump ` = IPTW,
        `Off-pump  ` = OW
      )
  ) |>
  
  # Make a table
  knitr::kable(format = "html") |>
  kableExtra::kable_styling() |>
  kableExtra::add_header_above(c("", "Original sample" = 3, "IPTW-adjusted" = 3, "OW-adjusted" = 3))

# NVD variable
sim_dat |>
  
  # Add unadjusted weight
  add_column(Raw = 1) |>
  
  # Send the weights down
  pivot_longer(
    cols = c(Raw, IPTW_est, OW_est),
    names_to = "Method",
    values_to = "Weight"
  ) |>
  
  # Compute counts
  summarize(
    N = n(),
    Weight = sum(Weight),
    .by = 
      c(
        Method,
        treated,
        NVD
      )
  ) |>
  
  # Compute relative
  mutate(
    Weight = Weight / sum(Weight),
    NewN = round(Weight * sum(N)),
    Summary = paste0(NewN, " (", round(100 * Weight,1), ")"),
    treated = case_when(treated == 1 ~ "Off-pump", TRUE ~ "On-pump"),
    .by = c(Method, treated)
  ) |>
  
  # Send over the columns
  pivot_wider(
    names_from = treated,
    values_from = Summary,
    id_cols = c(Method, NVD)
  ) |>
  
  # Rerrange
  arrange(
    Method,
    NVD
  )

# Compute the SMD
sim_dat |>
  
  # Add unadjusted weight
  add_column(Raw = 1) |>
  
  # Send the weights down
  pivot_longer(
    cols = c(Raw, IPTW_est, OW_est),
    names_to = "Method",
    values_to = "Weight"
  ) |>
  
  
  # Compute the means
  summarize(
    SMD = smd::smd(factor(NVD), g = treated, w = Weight, na.rm = TRUE)$estimate,
    .by = Method
  )
