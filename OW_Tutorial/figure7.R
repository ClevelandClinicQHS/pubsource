# Created: 2024-04-03
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Plot showing the cumulative contribution of patients

# Load packages
library(tidyverse)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

sim_dat |>
  
  # Send the weights down the rows
  pivot_longer(
    cols = c(IPTW_est, OW_est),
    names_to = "Type",
    values_to = "Weight"
  ) |>
  
  # Arrange by the weights within treatments
  arrange(
    treated,
    Type,
    Weight
  ) |> 
  
  # Indicate the unadjusted weight
  add_column(NullWeight = 1) |>
  
  # Compute the cumulative weight proportions
  mutate(
    Weight = cumsum(Weight) / sum(Weight),
    NullWeight = cumsum(NullWeight) / sum(NullWeight),
    .by = c(treated, Type)
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
        Type == "IPTW_est" ~ "IPTW",
        TRUE ~ "OW"
      ) |>
      factor() 
  ) |>
  
  # Make a plot
  ggplot(
    aes(
      x = Weight,
      y = NullWeight,
      color = Group,
      linetype = Group
    )
  ) + 
  geom_line(
    linewidth = 1.25
  ) +
  geom_hline(yintercept = .5, linetype = 3) +
  geom_vline(xintercept = .5, linetype = 3) +
  facet_wrap(~Type, nrow = 1) +
  scale_x_continuous(
    name = "Cumulative percent of weight (%)",
    labels = scales::percent
  ) +
  scale_y_continuous(
    name = "Cumulative percent of patients (%)",
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
  )