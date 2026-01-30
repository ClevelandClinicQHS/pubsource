# Created: 2026-01-30
# Author: Alex Zajichek
# Project: Bariatric Surgery vs. GLP-1
# Description: Builds weighted datasets

# Load packages
library(tidyverse)

## Import data set

# Imputation data
analysis_imputed <- read_rds(file = "data/analysis_imputed.rds")

## Estimate the overlap weights

# Specify the propensity score model
ps_formula <-
  (Treatment == "Surgical") ~
    Sex +
    Age +
    Race +
    Income +
    SmokingStatus +
    BMI +
    BP_SYSTOLIC +
    TG +
    HDLc +
    LDLc +
    HBA1c +
    UACR +
    HYPERTENSION +
    DYSLIPIDEMIA +
    NEUROPATHY +
    HEART_FAILURE +
    CAD +
    COPD +
    AFib +
    PAD +
    CVE +
    MI +
    CountAntiDiabeticsNonInsulin +
    INSULIN +
    LIPID_LOWERING +
    RAAS_INHIBITOR +
    OTHER_ANTIHTN +
    ASPIRIN +
    WARFARIN +
    Location +
    eGFR +
    NEPHROPATHY +
    RETINOPATHY

# Get the weights
analysis_weighted <-
  analysis_imputed |>

  # Nest by imputation set
  group_by(Imputation) |>
  nest() |>

  # Run the model, derive the weights
  mutate(
    data = data |>
      map(
        function(.dat) {
          # Fit the model
          ps_mod <-
            glm(
              formula = ps_formula,
              data = .dat,
              family = "binomial"
            )

          .dat |>

            # Compute overlap weight
            mutate(
              PS = predict(ps_mod, type = "response"),
              OW = case_when(
                Treatment == "Surgical" ~ 1 - PS,
                TRUE ~ PS
              )
            ) |>

            # Compute normalized overlap weight
            mutate(
              OverlapWeight = OW / sum(OW),
              .by = Treatment
            ) |>

            # Remove intermediary measures
            select(-PS, -OW)
        }
      )
  ) |>

  # Expand the data set
  unnest(cols = data) |>
  ungroup()

# Write to file (current directory)
#analysis_weighted |> write_rds(file = "data/analysis_weighted.rds")
#analysis_weighted |> write_csv(file = "data/analysis_weighted.csv")
