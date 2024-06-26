# Created: 2024-03-30
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Construct KM curves

# Load packages
library(tidyverse)
library(survival)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

sim_dat |> 
  
  # Indicate null weight
  add_column(Crude = 1) |>
  
  # Send the weights down
  pivot_longer(
    cols = c(Crude, IPTW_est, OW_est),
    names_to = "Type",
    values_to = "Weight"
  ) |>
  
  # Normalize the weights within groups (optional)
  mutate(
    Weight = Weight / sum(Weight),
    .by = c(treated, Type)
  ) |> 
  
  # Nest the data by treatment and weight type
  nest(.by = c(treated, Type)) |>
  
  # Estimate KM curves
  mutate(
    models = 
      data |>
      
      # For each data set
      map(
        function(.dat) {
          
          temp_surv <- 
            
            # Fit the model
            survfit(
              formula = Surv(time, event) ~ 1, 
              data = .dat,
              weights = Weight,
              robust = TRUE
            ) |>
            
            # Get estimates through 10 years
            summary(temp_surv, times = seq(0, 10, .25), extend = TRUE)
          
          # Return the estimates
          tibble(
            Time = temp_surv$time,
            Estimate = temp_surv$surv,
            Lower = temp_surv$lower,
            Upper = temp_surv$upper
          )
          
        }
      )
  ) |>
  
  # Remove the raw data
  select(-data) |>
  
  # Unnest the model output
  unnest(cols = models) |>
  
  # Bind to get true survival curves
  bind_rows(
    sim_dat |>
      
      # Keep a couple columns
      select(
        `1` = time_treat,
        `0` = time_control
      ) |>
      
      # Send the counterfacturals down the rows
      pivot_longer(
        cols = everything(),
        names_to = "treated",
        values_to = "Time",
        names_transform = list(treated = as.numeric)
      ) |>
      
      # Arrange the data by group
      group_by(treated) |>
      arrange(Time, .by_group = TRUE) |>
      
      # Compute the cumulative proportion
      mutate(
        Time = Time, # To account for adjustment made to the observed data in sim_dat
        Estimate = 1,
        Estimate = cumsum(Estimate) / length(Estimate),
        Estimate = 1 - Estimate,
        Lower = Estimate,
        Upper = Estimate
      ) |>
      
      # Filter to 10 years
      filter(Time <= 10) |>
      
      # Indicate group
      add_column(Type = "True")
  ) |>
  
  # Clean up names
  mutate(
    Group = 
      case_when(
        treated == 1 ~ "Treatment",
        TRUE ~ "Control"
      ) |>
      factor() |>
      fct_rev(),
    Type = 
      case_when(
        Type == "IPTW_est" ~ "c. IPTW",
        Type == "OW_est" ~ "d. OW",
        Type == "Crude" ~ "a. Unadjusted Data",
        Type == "True" ~ "b. True Treatment Effect" 
      ) |>
      factor()
    
  ) |>
  
  # Make a plot
  ggplot(
    aes(
      x = Time
    )
  ) + 
  geom_line(
    aes(
      y = Estimate,
      linetype = Group,
      color = Group,
    ),
    linewidth = 1.25
  ) +
  geom_ribbon(
    aes(
      ymin = Lower,
      ymax = Upper,
      fill = Group
    ),
    alpha = .15
  ) +
  facet_wrap(~Type, nrow = 2) +
  scale_x_continuous(
    name = "Years since treatment",
    labels = function(x) round(x)
  ) +
  scale_y_continuous(
    name = "Survival Probability (%)",
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "gray"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold", hjust = 0)
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  scale_fill_manual(
    values = c("#041c3b", "#a19337")
  )