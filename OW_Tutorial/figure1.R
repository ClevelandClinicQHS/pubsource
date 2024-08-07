# Created: 2024-07-18
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Show the PS distributions in a mirrored histogram

# Load packages
library(tidyverse)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

sim_dat |>
  
  # Make clean group names
  mutate(
    Group = 
      case_when(
        treated == 1 ~ "Off-pump",
        TRUE ~ "On-pump"
      ) |>
      factor(),
    Bins = Hmisc::cut2(ps_est, cuts = seq(0, 1, .1))
  ) |>
  
  # Compute counts
  summarize(
    Patients = n(),
    .by = c(Group, Bins)
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
    linewidth = 1
  ) +
  xlab("Estimated propensity score") +
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
    axis.text.x = element_text(size = 10)
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  scale_fill_manual(
    values = c("#041c3b", "#a19337")
  )