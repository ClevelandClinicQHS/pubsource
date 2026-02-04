# Created: 2026-02-04
# Author: Alex Zajichek
# Project: Bariatric Surgery vs. GLP-1
# Description: Estimates event rates per 100 years

# Load packages
library(tidyverse)

## Import datasets

# Outcome dataset
analysis_outcome <- read_rds(file = "data/analysis_outcome.rds")

## Run estimation process

# Set bootstrap settings
set.seed(123)
B <- 2 # ACTUAL IN MANUSCRIPT = 1000

# Run analysis
analysis_eventrateper100_bootstrap_all <-
  analysis_outcome |>

  # Repeat for each group
  group_by(
    Outcome,
    Imputation,
    Treatment
  ) |>
  nest() |>

  # Run bootstrap
  mutate(
    boot = data |>
      map(
        function(.data) {
          1:B |>

            # For each bootstrap
            map_df(
              function(.boot) {
                # 1. Take a bootstrap sample
                temp_dat <- .data[
                  sample(1:nrow(.data), nrow(.data), replace = TRUE),
                ]

                # Gather results
                tibble(
                  B = .boot,
                  Type = c("Unweighted", "Weighted"),
                  EventRate = c(
                    mean(temp_dat$Status),
                    sum(temp_dat$Status * temp_dat$OverlapWeight) /
                      sum(temp_dat$OverlapWeight)
                  ),
                  EventRatePer100Years = 100 *
                    c(
                      sum(temp_dat$Status) / sum(temp_dat$Time),
                      sum(temp_dat$Status * temp_dat$OverlapWeight) /
                        sum(temp_dat$Time * temp_dat$OverlapWeight)
                    )
                )
              }
            )
        }
      )
  ) |>

  # Unnest
  select(-data) |>
  unnest(cols = boot) |>
  ungroup()

### Consolidate estimates, obtain CI's
pooled_estimates <-
  analysis_outcome |>

  # Repeat for each group
  group_by(
    Outcome,
    Imputation,
    Treatment
  ) |>
  nest() |>

  # Get the point estimates
  mutate(
    rates = data |>
      map(
        function(.data) {
          temp_dat <- .data

          # Gather results
          tibble(
            Type = c("Unweighted", "Weighted"),
            EventRate = c(
              mean(temp_dat$Status),
              sum(temp_dat$Status * temp_dat$OverlapWeight) /
                sum(temp_dat$OverlapWeight)
            ),
            EventRatePer100Years = 100 *
              c(
                sum(temp_dat$Status) / sum(temp_dat$Time),
                sum(temp_dat$Status * temp_dat$OverlapWeight) /
                  sum(temp_dat$Time * temp_dat$OverlapWeight)
              )
          )
        }
      )
  ) |>

  # Unnest
  select(-data) |>
  unnest(cols = rates) |>
  ungroup() |>

  # Remove rate column
  select(-EventRate) |>

  # Send over columns
  pivot_wider(
    names_from = Treatment,
    values_from = EventRatePer100Years
  ) |>

  # Compute the difference
  mutate(
    Difference = Nonsurgical - Surgical
  ) |>

  # Send back down the rows
  pivot_longer(
    cols = c(Nonsurgical, Surgical, Difference),
    names_to = "Treatment",
    values_to = "EventRatePer100Years"
  ) |>

  # Average over imputations
  summarize(
    Estimate = mean(EventRatePer100Years),
    .by = c(
      Outcome,
      Type,
      Treatment
    )
  ) |>

  # Join to get bootstrap CI
  inner_join(
    y = analysis_eventrateper100_bootstrap_all |>

      # Remove rate column
      select(-EventRate) |>

      # Send over columns
      pivot_wider(
        names_from = Treatment,
        values_from = EventRatePer100Years
      ) |>

      # Compute the difference
      mutate(
        Difference = Nonsurgical - Surgical
      ) |>

      # Send back down the rows
      pivot_longer(
        cols = c(Nonsurgical, Surgical, Difference),
        names_to = "Treatment",
        values_to = "EventRatePer100Years"
      ) |>

      # Get the lower/upper bound using percentile method (across all imputations)
      summarize(
        BootEstimate = mean(EventRatePer100Years),
        Lower = quantile(EventRatePer100Years, .025),
        Upper = quantile(EventRatePer100Years, .975),
        .by = c(
          Outcome,
          Type,
          Treatment
        )
      ),
    by = c(
      "Outcome",
      "Type",
      "Treatment"
    )
  )

### Make primary table from weighted data
pooled_estimates |>

  # Filter to weighted result
  filter(
    Type == "Weighted"
  ) |>

  # Round data
  mutate(
    across(where(is.numeric), \(x) round(x, 2))
  ) |>

  # Send over the columns
  pivot_wider(
    names_from = Treatment,
    values_from = c(Estimate, BootEstimate, Lower, Upper),
    id_cols = Outcome
  ) |>

  # Rearrange
  arrange(Outcome) |>
  transmute(
    Outcome,
    `Surgical Group` = Estimate_Surgical,
    `Nonsurgical Group` = Estimate_Nonsurgical,
    Estimate = Estimate_Difference,
    `95% Confidence Interval` = paste0(
      "(",
      Lower_Difference,
      ", ",
      Upper_Difference,
      ")"
    )
  )
