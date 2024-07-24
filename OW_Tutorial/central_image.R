# Created: 2024-07-24
# Author: Alex Zajichek
# Project: Overlap weighting tutorial
# Description: Creates the Central Image

# Load packages
library(tidyverse)

# Import data set
sim_dat <- read_rds(file = "sim_dat.rds") # <- assumes your working directory contains the data

# Put data into long format
sim_dat |>
  
  # Add unadjusted weight
  add_column(Raw = 1) |>
  
  # Send the weights down
  pivot_longer(
    cols = c(Raw, IPTW_est, OW_est),
    names_to = "Method",
    values_to = "Weight"
  ) |>
  
  # Normalize the weight
  mutate(
    Weight = Weight / sum(Weight),
    .by = c(Method, treated)
  ) |>
  
  # Send the covariates down the rows
  pivot_longer(
    cols = c(Age:Trainee),
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
  
  # Order factor by largest SMD in unadjusted data
  mutate(
    Factor = 
      case_when(
        Factor == "CardiogenicShock" ~ "Cardiogenic Shock",
        TRUE ~ Factor
      ),
    Factor = Factor |> factor() |> fct_reorder(SMD, \(x) max(abs(x))),
    Method = 
      Method |>
      fct_relevel(
        "Raw",
        "IPTW_est",
        "OW_est"
      ) |>
      fct_recode(
        Unadjusted = "Raw",
        IPTW = "IPTW_est",
        OW = "OW_est"
      ) |>
      fct_rev(),
    SMD = abs(SMD)
  ) |>
  
  # Make a plot
  ggplot(
    aes(
      x = Factor,
      y = SMD,
      color = Method,
      fill = Method
    )
  ) + 
  geom_col(
    width = .25,
    position = position_dodge(width = .5)
  ) +
  geom_point(
    position = position_dodge(width = .5),
    size = 3
  ) +
  coord_flip() +
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(color = "gray"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 10, family = "sans"),
    axis.title = element_text(size = 10, family = "sans"),
    axis.text = element_text(size = 10, family = "sans")
  ) +
  scale_color_manual(
    values = c("#041c3b", "#a19337", "#6f7173")
  ) +
  scale_fill_manual(
    values = c("#041c3b", "#a19337", "#6f7173")
  ) +
  xlab("Confounder") +
  ylab("Standardized mean difference (SMD)") +
  guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE))
