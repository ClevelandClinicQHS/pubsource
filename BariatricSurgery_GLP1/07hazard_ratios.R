# Created: 2026-01-30
# Author: Alex Zajichek
# Project: Bariatric Surgery vs. GLP-1
# Description: Estimates hazard ratios

# Load packages
library(tidyverse)
library(survival)

## Import datasets

# Outcome dataset
analysis_outcome <- read_rds(file = "data/analysis_outcome.rds")

## Set the model formula
outcome_formula <-
  Surv(Time, Status) ~
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
    IndexDate +
    Treatment +
    RETINOPATHY

## Fit the models
cox_mods <-
  analysis_outcome |>

  # For each group
  group_by(
    Outcome,
    Imputation
  ) |>
  nest() |>

  # Fit a Cox PH model
  mutate(
    model = data |>
      map(
        function(.dat) {
          # Fit the model
          temp_mod <-
            coxph(
              formula = outcome_formula,
              data = .dat,
              weights = OverlapWeight
            )

          # Gather estimate
          tibble(
            Estimate = temp_mod$coefficients["TreatmentSurgical"],
            SE = sqrt(diag(vcov(temp_mod)))["TreatmentSurgical"],
            EventRate = mean(.dat$Status)
          )
        }
      )
  ) |>

  # Remove data
  select(-data) |>
  ungroup() |>
  unnest(cols = model)

## Build table of pooled HR estimates
# All patients
cox_mods |>

  # Pool estimates over imputations
  summarize(
    N = n(),
    VarWithin = mean(SE^2),
    VarBetween = var(Estimate),
    PooledEstimate = mean(Estimate),
    .by = Outcome
  ) |>

  # Finalize the estimates
  mutate(
    VarTotal = VarWithin + VarBetween + VarBetween / N,
    PooledSE = sqrt(VarTotal),
    PooledLower = PooledEstimate - qnorm(.975) * PooledSE,
    PooledUpper = PooledEstimate + qnorm(.975) * PooledSE,
    ZScore = PooledEstimate / PooledSE,
    PValue = ifelse(
      ZScore < 0,
      2 * pnorm(ZScore),
      2 * pnorm(ZScore, lower.tail = FALSE)
    ),
    across(
      c(PooledEstimate, PooledLower, PooledUpper),
      \(x) round(exp(x), 2)
    )
  ) |>

  # Keep a few columns
  transmute(
    Outcome,
    `HR (95% CI)` = paste0(
      PooledEstimate,
      " (",
      PooledLower,
      ", ",
      PooledUpper,
      ")"
    ),
    PValue = case_when(
      PValue < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(PValue, 3))
    )
  ) |>
  arrange(Outcome)
