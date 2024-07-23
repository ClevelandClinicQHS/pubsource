# Created: 2024-07-18
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Plot the IPTW and OW distributions and accumulations

# Load packages
library(tidyverse)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

# Weight distribution
wt_distribution <- 
  sim_dat |>
  
  # Send down the rows
  pivot_longer(
    cols = c(IPTW_est, OW_est),
    names_to = "Method",
    values_to = "Weight"
  ) |>
  
  # Make weight bins and clean labels
  mutate(
    Bins = Hmisc::cut2(Weight, cuts = c(seq(0, 1, .1), 1.25, 1.5, 2, 3, 5, 7, 10, 15, 21, 50), formatfun = \(x) round(x, 1)),
    Group = 
      case_when(
        treated == 1 ~ "Off-pump",
        TRUE ~ "On-pump"
      ) |>
      factor(),
    Method = 
      case_when(
        Method == "IPTW_est" ~ "a. IPTW",
        TRUE ~ "b. OW"
      )
  ) |> 
  
  # Compute counts
  summarize(
    Patients = n(),
    .by = c(Group, Method, Bins)
  ) |>
  
  # Make a plot
  ggplot() + 
  geom_col(
    aes(
      x = Bins,
      y = ifelse(Group == "Off-pump", Patients, -Patients),
      fill = Group,
      linetype = Group
    ),
    alpha = .5,
    width = 1,
    color = "black",
    linewidth = .75
  ) +
  facet_wrap(~Method, nrow = 1, scales = "free_x") +
  scale_y_continuous(
    name = "Patient count",
    labels = abs
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "gray"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 45),
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold", hjust = 0)
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  scale_fill_manual(
    values = c("#041c3b", "#a19337")
  ) +
  xlab("Weight")

# Weight accumulation
wt_accumulation <-
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
        treated == 1 ~ "Off-pump",
        TRUE ~ "On-pump"
      ) |>
      factor() |>
      fct_rev(),
    Type = 
      case_when(
        Type == "IPTW_est" ~ "c. IPTW",
        TRUE ~ "d. OW"
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
    strip.text = element_text(size = 16, face = "bold", hjust = 0)
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  )

# Combine the plots
ggpubr::ggarrange(wt_distribution, wt_accumulation, nrow = 2, common.legend = T)

# Obtain retained volumes using symmetric trimming rule of thumb threshold
sim_dat |>
  
  # Compute group counts
  summarize(
    N = n(),
    Excluded = sum(ps_est <= .1 | ps_est >= .9),
    ExclusionRate = Excluded / N,
    .by = treated
  )

# Weight accumulation at particular time points
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
  
  # Filter where 25% of patients are accounted for
  filter(NullWeight >= .249, NullWeight < .251) |>
  select(
    treated, 
    Type,
    Weight,
    NullWeight
  )
 