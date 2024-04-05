# Created: 2024-04-01
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Love plot and table showing the mean differences in covariates.

# Load packages
library(tidyverse)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

sim_dat |>
  
  # Add unadjusted weight
  add_column(Raw = 1) |>
  
  # Send down the rows
  pivot_longer(
    cols = c(IPTW_est, OW_est, Raw),
    names_to = "Method",
    values_to = "Weight"
  ) |>
  
  # Send the covariates down the rows
  pivot_longer(
    cols = c(age, male, ejection_fraction),
    names_to = "Factor",
    values_to = "Value"
  ) |>
  
  # Compute the means
  summarize(
    SMD = smd::smd(Value, g = treated, w = Weight, na.rm = TRUE)$estimate,
    .by = 
      c(
        Factor,
        Method
      )
  ) |>
  
  # Clean up labels
  mutate(
    Sample = 
      case_when(
        Method == "IPTW_est" ~ "IPTW",
        Method == "OW_est" ~ "OW",
        TRUE ~ "Unadjusted"
      ) |>
      factor() |>
      fct_relevel("Unadjusted", "IPTW", "OW") |>
      fct_rev(),
    Factor = 
      case_when(
        Factor == "age" ~ "Age",
        Factor == "male" ~ "Sex",
        Factor == "ejection_fraction" ~ "Ejection fraction"
      ) |> 
      fct_reorder(
        .x = SMD,
        .fun = \(x) max(abs(x))
      ) |> fct_rev()
  ) |>
  
  # Make a plot
  ggplot() + 
  geom_linerange(
    aes(
      x = Factor,
      ymin = SMD,
      ymax = 0,
      color = Sample,
      linetype = Sample
    ),
    linewidth = 1,
    position = position_dodge(width = .5)
  ) +
  geom_point(
    aes(
      x = Factor,
      y = SMD,
      color = Sample,
      shape = Sample
    ),
    size = 5,
    position = position_dodge(width = .5)
  ) +
  coord_flip() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.y = element_blank()
  ) +
  ylab("Standardized mean difference") +
  guides(
    color = guide_legend(reverse = TRUE),
    linetype = guide_legend(reverse = TRUE),
    shape = guide_legend(reverse = T)
  )

# Make a table
sim_dat |>
  
  # Add unadjusted weight
  add_column(Raw = 1) |>
  
  # Send down the rows
  pivot_longer(
    cols = c(IPTW_est, OW_est, Raw),
    names_to = "Method",
    values_to = "Weight"
  ) |>
  
  # Send the covariates down the rows
  pivot_longer(
    cols = c(age, male, ejection_fraction),
    names_to = "Factor",
    values_to = "Value"
  ) |>
  
  # Compute the means
  summarize(
    Mean = sum(Value * Weight) / sum(Weight),
    .by = 
      c(
        Factor,
        Method,
        treated
      )
  ) |>
  
  # Clean up labels
  mutate(
    Sample = 
      case_when(
        Method == "IPTW_est" ~ "IPTW",
        Method == "OW_est" ~ "OW",
        TRUE ~ "Unadjusted"
      ) |>
      factor() |>
      fct_relevel("Unadjusted", "IPTW", "OW"),
    Factor = 
      case_when(
        Factor == "age" ~ "Age (years)",
        Factor == "male" ~ "Male (%)",
        Factor == "ejection_fraction" ~ "Ejection fraction (%)"
      ),
    Group = 
      case_when(
        treated == 1 ~ "Treatment",
        TRUE ~ "Control"
      ) |>
      fct_rev(),
    Mean = case_when(Factor == "Male (%)" ~ 100*Mean, TRUE ~ Mean),
    Mean = round(Mean, 3)
  ) |>
  
  # Remove some columns
  select(-Method, -treated) |>
  
  # Send over columns
  cheese::stretch(
    key = c(Sample, Group),
    value = Mean 
  ) |>
  
  # Make a graded table
  cheese::grable(at = -Factor, full_width = FALSE)
