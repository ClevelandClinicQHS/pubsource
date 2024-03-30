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
  
  # Make a redundant weight column
  add_column(Crude = 1) |>
  
  # Normalize the the true OW
  mutate(
    Overlap = OW_true / sum(OW_true),
    .by = treated
  ) |>
  
  # Send weights down the rows
  pivot_longer(
    cols = c(Crude, Overlap),
    names_to = "Type",
    values_to = "Weight"
  ) |>
  
  # Nest the data
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
              weights = Weight
            ) |>
            
            # Get estimates through 10 years
            summary(temp_surv, times = seq(0, 10, .25))
          
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
  
  # Clean up names
  mutate(
    Group = 
      case_when(
        treated == 1 ~ "Treated",
        TRUE ~ "Controlled"
      ) |>
      factor() |>
      fct_rev(),
    Type = 
      case_when(
        Type == "Crude" ~ "Unadjusted Data",
        TRUE ~ "Overlap Weighted Data"
      ) |>
      factor() |>
      fct_rev()
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
  facet_wrap(~Type, nrow = 1) +
  scale_x_continuous(
    name = "Years since treatment",
    labels = function(x) round(x)
  ) +
  scale_y_continuous(
    name = "Survival Probability (%)",
    labels = scales::percent
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
    strip.text = element_text(size = 16, face = "bold")
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  scale_fill_manual(
    values = c("#041c3b", "#a19337")
  )