# Created: 2026-01-30
# Author: Alex Zajichek
# Project: Bariatric Surgery vs. GLP-1
# Description: Builds survival curves

# Load packages
library(tidyverse)
library(survival)

## Import datasets

# Outcome dataset
analysis_outcome <- read_rds(file = "data/analysis_outcome.rds")

## Build the plotting data
km_dat <-
  analysis_outcome |>
  group_by(
    Outcome,
    Treatment,
    Imputation
  ) |>
  nest() |>

  # Build KM curves
  mutate(
    KM = data |>
      map(
        function(.dat) {
          # Fit the KM model
          temp_mod <-
            survfit(
              formula = Surv(Time, Status) ~ 1,
              data = .dat |>
                mutate(OverlapWeight = OverlapWeight / sum(OverlapWeight)),
              weights = OverlapWeight
            ) |>

            # Get the estimates at specified times
            summary(times = seq(0, 10, .1))

          # Make a data set
          tibble(
            Time = temp_mod$time,
            Estimate = temp_mod$surv,
            AtRisk = temp_mod$n.risk * nrow(.dat)
          )
        }
      )
  ) |>

  # Remove the data
  select(-data) |>

  # Unravel
  unnest(cols = KM) |>
  ungroup() |>

  # Average over the imputations
  summarize(
    Estimate = mean(Estimate),
    AtRisk = mean(AtRisk),
    .by = c(
      Outcome,
      Treatment,
      Time
    )
  ) |>

  # Relabel
  mutate(
    Treatment = case_when(
      Treatment == "Nonsurgical" ~ "GLP-1RAs",
      TRUE ~ "Metabolic Surgery"
    ) |>
      factor()
  ) |>

  # Nest data
  group_by(Outcome) |>
  nest() |>

  # Build cumulative incidence curves
  mutate(
    CumIncPlot = data |>
      map2(
        .y = as.list(Outcome),
        function(.dat, .outcome) {
          # Label data
          y_lab <- .outcome
          x_breaks <- c(0, 2, 4, 6, 8, 10)
          y_breaks <- seq(0, 1, .20)
          mult <- 1
          plot_label_dat <-
            .dat |>
            filter(Time == 4) |>
            mutate(
              lab_pos = ifelse(
                Treatment == "Metabolic Surgery",
                (1 - Estimate) - .015 * mult,
                (1 - Estimate) + 0.015 * mult
              )
            )

          # Make the KM plot
          p1 <-
            .dat |>
            filter(Time <= 10) |>

            # Make a plot
            ggplot() +
            geom_line(
              aes(
                x = Time,
                y = 1 - Estimate,
                color = Treatment
              ),
              linewidth = 1
            ) +
            geom_text(
              data = plot_label_dat,
              aes(
                x = Time,
                y = lab_pos,
                label = Treatment
              ),
              hjust = .1
            ) +
            scale_x_continuous(
              name = "Years Since Index Date",
              breaks = x_breaks
            ) +
            scale_y_continuous(
              name = paste0("Cumulative incidence of ", y_lab, ", %"),
              breaks = y_breaks,
              limits = c(0, 1),
              labels = function(x) x * 100
            ) +
            theme(
              panel.background = element_blank(),
              legend.position = "none"
            ) +
            scale_color_manual(values = c("#F0A749", "#406A77")) +
            geom_hline(yintercept = 0, color = "black") +
            geom_vline(xintercept = 0, color = "black")

          # Make the grid of number at risk
          p2 <-
            .dat |>
            filter(Time %in% x_breaks) |>
            mutate(Treatment = factor(Treatment)) |>
            ggplot() +
            geom_text(
              aes(
                x = Time,
                y = 1,
                label = round(AtRisk),
                color = Treatment,
                group = Treatment
              ),
              position = position_stack(vjust = .01)
            ) +
            scale_color_manual(values = c("#F0A749", "#406A77")) +
            theme(
              panel.background = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid = element_blank(), # Removed the gaps
              axis.ticks = element_blank(),
              legend.position = "none",
              axis.text = element_text(color = "white"),
              plot.margin = unit(c(-8, 0, 0, 0), "cm")
            ) +
            scale_y_continuous(
              breaks = y_breaks,
              limits = c(0, 15)
            ) +
            xlab("") +
            ylab("")

          cowplot::plot_grid(p1, p2, nrow = 2, rel_heights = c(.95, .25))
        }
      )
  )

cowplot::plot_grid(
  km_dat$CumIncPlot[[1]],
  km_dat$CumIncPlot[[2]],
  km_dat$CumIncPlot[[3]],
  km_dat$CumIncPlot[[4]],
  nrow = 2
)
