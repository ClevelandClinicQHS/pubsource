# Created: 2024-07-18
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Plot the OR from the PS model

# Load packages
library(tidyverse)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

# Fit the PS model
ps_model <-
  glm(
    formula = treated ~ .,
    data = 
      sim_dat |> 
      select(-c(log_odds_ps_true, ps_true, OW_true, log_odds_outcome_control, log_odds_outcome_treatment, death_control, death_treatment, death, ps_est, IPTW_est, OW_est)) |>
      mutate(
        across(
          c(Age, NVD, BMI, YOP),
          \(x) scale(x)[,1]
        )
      ),
    family = "binomial"
  )

# Get the 95% CI
ci <- confint(ps_model)

# Put into a data frame
ps_model$coefficients |>
  enframe(
    name = "Term",
    value = "Estimate"
  ) |>
  
  # Remove the intercept
  filter(Term != "(Intercept)") |>
  
  # Join to get the CI
  inner_join(
    y = 
      ci |>
      
      # Convert to data frame
      as_tibble() |>
      
      # Add the term names
      add_column(Term = names(ps_model$coefficients)) |>
      
      # Rename the limits
      rename(
        Lower = `2.5 %`,
        Upper = `97.5 %`
      ),
    by = "Term"
  ) |>
  
  # Convert to OR
  mutate(
    across(
      c(Estimate, Lower, Upper),
      exp
    ),
    Term = 
      case_when(
        Term == "CardiogenicShock" ~ "Cardiogenic shock",
        TRUE ~ Term
      ),
    Term = factor(Term) |> fct_reorder(Estimate),
    Group = 
      case_when(
        Term %in% c("Age", "NVD", "BMI", "YOP") ~ "Per standardized unit increase",
        TRUE ~ "Binary factor"
      )
  ) |>
  
  # Make a plot
  ggplot() + 
  geom_point(
    aes(
      x = Term,
      y = Estimate,
      color = Group,
      shape = Group
    ),
    size = 3
  ) +
  geom_linerange(
    aes(
      x = Term,
      ymin = Lower,
      ymax = Upper,
      color = Group
    ),
    linewidth = 0.75
  ) +
  geom_hline(yintercept = 1) +
  coord_flip() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  xlab("Confounder") +
  ylab("OR (95% CI)")

