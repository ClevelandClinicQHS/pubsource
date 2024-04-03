# Created: 2024-04-02
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Plot showing the effects on PS

# Load packages
library(tidyverse)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

# Age plot
age_plot <-
  sim_dat |> 
  
  # Make the group
  mutate(
    Group = 
      case_when(
        treated == 1 ~ "Treatment",
        TRUE ~ "Control"
      ) |>
      factor() |>
      fct_rev()
  ) |>
  
  # Make a plot
  ggplot() +
  geom_point(
    aes(
      x = age,
      y = ps_est,
      color = Group
    ),
    alpha = .02
  ) +
  geom_smooth(
    aes(
      x = age,
      y = ps_est,
      color = Group,
      linetype = Group
    ),
    alpha = 0,
    linewidth = 1.25
  ) +
  scale_y_continuous(
    name = "Estimated PS (%)",
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
    axis.text = element_text(size = 12)
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  xlab("Age (years)")

# EF plot
ef_plot <-
  sim_dat |> 
  
  # Make the group
  mutate(
    Group = 
      case_when(
        treated == 1 ~ "Treatment",
        TRUE ~ "Control"
      ) |>
      factor() |>
      fct_rev()
  ) |>
  
  # Make a plot
  ggplot() +
  geom_point(
    aes(
      x = ejection_fraction,
      y = ps_est,
      color = Group
    ),
    alpha = .02
  ) +
  geom_smooth(
    aes(
      x = ejection_fraction,
      y = ps_est,
      color = Group,
      linetype = Group
    ),
    alpha = 0,
    linewidth = 1.25
  ) +
  scale_y_continuous(
    name = "Estimated PS (%)",
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
    axis.text = element_text(size = 12)
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  xlab("Ejection fraction (%)")

# Sex plot
sex_plot <-
  sim_dat |> 
  
  # Make the group
  mutate(
    Group = 
      case_when(
        treated == 1 ~ "Treatment",
        TRUE ~ "Control"
      ) |>
      factor() |>
      fct_rev(),
    Sex = 
      case_when(
        male == 1 ~ "Male",
        TRUE ~ "Female"
      ) |>
      factor() |>
      fct_rev()
  ) |>
  
  # Make a plot
  ggplot() +
  geom_boxplot(
    aes(
      x = Sex,
      y = ps_est,
      fill = Group,
      linetype = Group
    ),
    alpha = .5,
    color = "black",
    linewidth = .75
  ) +
  scale_y_continuous(
    name = "Estimated PS (%)",
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
    axis.text = element_text(size = 12)
  ) +
  scale_fill_manual(
    values = c("#041c3b", "#a19337")
  ) +
  xlab("Sex")

# Combine into single plot
ggpubr::ggarrange(age_plot, sex_plot, ef_plot, nrow = 1, common.legend = TRUE)
