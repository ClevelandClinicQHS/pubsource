# Created: 2024-07-19
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Plot showing the change in weight contribution for confounders

# Load packages
library(tidyverse)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

# Make the plotting data
plot_dat <- 
  sim_dat |>
  
  # Make subgroups
  mutate(
    Age = 
      case_when(
        Age < 65 ~ "64 or less",
        Age < 70 ~ "65-69",
        Age < 75 ~ "70-74",
        Age < 80 ~ "75-80",
        TRUE ~ "80+"
      ),
    across(
      c(COPD, NYHA, LMD, IDDM, MI),
      \(x) case_when(x == 1 ~ "Yes", TRUE ~ "No")
    ),
    NullWeight = 1
  ) |> 
  
  # Send weights down the rows
  pivot_longer(
    cols = c(NullWeight, IPTW_est, OW_est),
    names_to = "Type",
    values_to = "Weight"
  ) |>
  
  # Send factors down the rows
  pivot_longer(
    cols = c(Age, COPD, NYHA, LMD, IDDM, MI),
    names_to = "Factor",
    values_to = "Level"
  ) |> 
  
  # Compute total weights
  summarize(
    Weight = sum(Weight),
    .by = c(Factor, Level, Type, treated)
  ) |>
  
  # Find percent of weight
  mutate(
    Weight = Weight / sum(Weight),
    .by = c(Factor, Type, treated)
  ) |>
  
  # Send over columns
  pivot_wider(
    names_from = Type,
    values_from = Weight
  ) |>
  
  # Clean up levels
  mutate(
    Group = 
      case_when(
        treated == 1 ~ "Off-pump",
        TRUE ~ "On-pump"
      ) |>
      factor() |>
      fct_rev(),
    Factor = 
      case_when(
        Factor == "Age" ~ "Age (years)",
        TRUE ~ Factor
      ),
    Level = 
      Level |>
      fct_relevel(
        "64 or less",
        "Yes"
      )
  )

# IPTW plot
iptw_plot <-
  plot_dat |>
  ggplot() +
  geom_col(
    aes(
      x = Level,
      y = NullWeight,
      fill = Group,
      linetype = Group
    ),
    color = "black",
    linewidth = .75,
    alpha = .15,
    position = "dodge"
  ) + 
  geom_point(
    aes(
      x = Level,
      y = IPTW_est,
      color = Group,
      shape = Group
    ),
    position = position_dodge(width = 1),
    size = 3
  ) +
  geom_line(
    aes(
      x = Level,
      y = IPTW_est,
      color = Group,
      group = Group,
      linetype = Group
    ),
    position = position_dodge(width = 1),
    linewidth = .5
  ) +
  facet_wrap(~Factor, scales = "free_x", strip.position = "bottom") +
  scale_y_continuous(
    name = "Share of weight within group (%)",
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "gray"),
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 14),
    plot.title = element_text( vjust = -2, size = 12, face = "bold.italic")
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  scale_fill_manual(
    values = c("#041c3b", "#a19337")
  ) +
  labs(
    fill = "Unweighted",
    linetype = "Unweighted",
    color = "Weighted",
    shape = "Weighted",
    title = "IPTW"
  )

# OW plot
ow_plot <-
  plot_dat |>
  ggplot() +
  geom_col(
    aes(
      x = Level,
      y = NullWeight,
      fill = Group,
      linetype = Group
    ),
    color = "black",
    linewidth = .75,
    alpha = .15,
    position = "dodge"
  ) + 
  geom_point(
    aes(
      x = Level,
      y = OW_est,
      color = Group,
      shape = Group
    ),
    position = position_dodge(width = 1),
    size = 3
  ) +
  geom_line(
    aes(
      x = Level,
      y = OW_est,
      color = Group,
      group = Group,
      linetype = Group
    ),
    position = position_dodge(width = 1),
    linewidth = .5
  ) +
  facet_wrap(~Factor, scales = "free_x", strip.position = "bottom") +
  scale_y_continuous(
    name = "Share of weight within group (%)",
    labels = scales::percent,
    limits = c(0, 1)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "gray"),
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 14),
    plot.title = element_text( vjust = -2, size = 12, face = "bold.italic")
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  scale_fill_manual(
    values = c("#041c3b", "#a19337")
  ) +
  labs(
    fill = "Unweighted",
    linetype = "Unweighted",
    color = "Weighted",
    shape = "Weighted",
    title = "OW"
  )

# Combine to single plot
ggpubr::ggarrange(iptw_plot, ow_plot, common.legend = TRUE, nrow = 2)

# Here's a way to put into a single plot
plot_dat |>
  
  # Send down the rows
  pivot_longer(
    cols = c(IPTW_est, OW_est),
    names_to = "Type",
    values_to = "Weight"
  ) |>
  
  # Make negative
  mutate(
    across(
      c(NullWeight, Weight),
      \(x) case_when(Type == "IPTW_est" ~ -1*x, TRUE ~ x)
    )
  ) |>
  
  # Make the plot
  ggplot() +
  geom_col(
    aes(
      x = Level,
      y = NullWeight,
      fill = Group,
      linetype = Group,
      group = Group#interaction(Group, Type)
    ),
    color = "black",
    linewidth = .75,
    alpha = .15,
    position = position_dodge(width = 1)
  ) + 
  geom_point(
    aes(
      x = Level,
      y = Weight,
      color = Group,
      shape = Group,
      group = Group#interaction(Group, Type)
    ),
    position = position_dodge(width = 1),
    size = 3
  ) +
  geom_col(
    aes(
      x = Level,
      y = Weight,
      fill = Group,
      group = Group, #interaction(Group, Type),
    ),
    position = position_dodge(width = 1),
    width = .05,
    alpha = .5
  ) +
 
  facet_wrap(~Factor, scales = "free_x", strip.position = "bottom") +
  scale_y_continuous(
    name = "Share of weight within group (%)",
    labels = \(x) scales::percent(abs(x)),
    limits = c(-1, 1),
    breaks = seq(-1, 1, .25)
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "gray"),
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 14)
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  scale_fill_manual(
    values = c("#041c3b", "#a19337")
  ) +
  labs(
    fill = "Unweighted",
    linetype = "Unweighted",
    color = "Weighted",
    shape = "Weighted"
  ) +
  geom_text(
    x = "<65",
    y = .4,
    label = "OW",
    size = 5
  ) +
  geom_text(
    x = "<65",
    y = -.4,
    label = "IPTW",
    size = 5
  ) +
  geom_hline(yintercept = 0, linewidth = 1.5)

# Plotting relative difference between weight difference for IPTW vs. OW
plot_dat |>
  
  # Compute the percentage point difference in weight
  mutate(
    Diff = (OW_est - NullWeight) - (IPTW_est - NullWeight),
    Group = Group |> fct_rev()
  ) |>
  
  # Make a plot
  ggplot() +
  geom_col(
    aes(
      x = Level,
      y = Diff,
      fill = Group,
      group = Group,
    ),
    position = position_dodge(width = .5),
    width = .10,
    alpha = .5
  ) +
  geom_point(
    aes(
      x = Level,
      y = Diff,
      color = Group,
      shape = Group,
      group = Group
    ),
    position = position_dodge(width = .5),
    size = 3
  ) +
  geom_hline(yintercept = 0) +
  facet_wrap(~Factor, scales = "free_y") +
  coord_flip() +
  scale_y_continuous(
    name = "Percentage point difference (%)",
    labels = \(x) paste0(round(x * 100), "%"),
    breaks = seq(-.05, .05, .01)
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 9),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 14),
    axis.ticks.y = element_blank()
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337")
  ) +
  scale_fill_manual(
    values = c("#041c3b", "#a19337")
  ) +
  annotate(
    'curve',
    x = "", 
    y = 0,
    yend = 0.05,
    xend = "",
    linewidth = 1,
    curvature = 0,
    arrow = arrow(length = unit(0.25, 'cm'))
  ) +
  annotate(
    'curve',
    x = "",
    y = 0,
    yend = -0.05,
    xend = "",
    linewidth = 1,
    curvature = 0,
    arrow = arrow(length = unit(0.25, 'cm'))
  ) +
  annotate(
    'text',
    x = "",
    y = .025,
    label = 'More OW',
    fontface = 'bold', 
    size = 3,
    vjust = 1
  ) +
  annotate(
    'text',
    x = "",
    y = -.025,
    label = 'More IPTW',
    fontface = 'bold', 
    size = 3,
    vjust = 1
  )
