# Created: 2024-04-01
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Plot the IPTW and OW distributions

# Load packages
library(tidyverse)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

sim_dat |>
  
  # Send down the rows
  pivot_longer(
    cols = c(IPTW_est, OW_est),
    names_to = "Method",
    values_to = "Weight"
  ) |>
  
  # Make weight bins and clean labels
  mutate(
    Bins = Hmisc::cut2(Weight, cuts = c(seq(0, 1, .1), 1.5, 2, 3, 5, 7, 10, 15, 21)),
    Group = 
      case_when(
        treated == 1 ~ "Treatment",
        TRUE ~ "Control"
      ) |>
      factor() |>
      fct_rev(),
    Method = 
      case_when(
        Method == "IPTW_est" ~ "IPTW",
        TRUE ~ "OW"
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
      y = ifelse(Group == "Treatment", Patients, -Patients),
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
    axis.text.x = element_text(size = 8, angle = 45),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 16, face = "bold")
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  scale_fill_manual(
    values = c("#041c3b", "#a19337")
  )
