# Created: 2026-02-04
# Author: Alex Zajichek
# Project: Bariatric Surgery vs. GLP-1
# Description: Obtains 10-year absolute risk difference estimates

# Load packages
library(tidyverse)
library(survival)

## Import data set

# Raw analysis data
analysis_raw <- read_rds(file = "data/analysis_raw.rds")

# Outcomes
outcomes <- read_rds(file = "data/outcomes.rds")

# Master imputation set
analysis_weighted <- read_rds(file = "data/analysis_weighted.rds")

## Set the model formulas

# PS model
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

# Outcome model
outcome_formula <- update(
  ps_formula,
  Surv(Time, Status) ~ . + IndexDate + Treatment - NEPHROPATHY
)

## Build functions for various workflow components

# Function to build imputation data sets
build_imputation_set <-
  function(.dat) {
    .dat |>

      # Remove the patient identifier from imputation
      select(-PatientID) |>

      # Run the imputation process
      mice::mice(m = 5, printFlag = FALSE) |>

      # Get the imputed data sets
      mice::complete("long") |>
      as_tibble() |>

      # Join to re-attach patient ID
      inner_join(
        y = tibble(
          .id = seq_along(.dat$PatientID),
          PatientID = .dat$PatientID
        ),
        by = ".id"
      ) |>

      # Remove artificial ID
      select(-.id) |>

      # Rerrange
      rename(Imputation = .imp) |>
      relocate(PatientID, .after = Imputation)
  }

# Function to estimate overlap weights
estimate_overlap_weights <-
  function(.dat, ps_formula) {
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

# Function to attach outcomes
attach_outcomes <-
  function(.dat, outcomes) {
    .dat |>

      # Join to get outcome details
      inner_join(
        y = outcomes,
        by = "PatientID",
        relationship = "many-to-many"
      ) |>

      # Rearrange columns
      relocate(
        Outcome,
        Time,
        Status,
        .before = Treatment
      )
  }

# Get 10-year KM estimates
get_km_estimates <-
  function(.dat) {
    .dat |> # Assumes outcome have been attached

      # For each treatment
      nest(.by = c(Outcome, Treatment)) |>

      # Run models for each treatment
      mutate(
        data = data |>
          map(
            function(.trt_dat) {
              # 1. Un-adjusted rates
              temp_mod1 <-
                survfit(
                  formula = Surv(Time, Status) ~ 1,
                  data = .trt_dat
                ) |>
                summary(times = 10, extend = TRUE)

              # 2. Overlap-weighted estimates
              temp_mod2 <-
                survfit(
                  formula = Surv(Time, Status) ~ 1,
                  data = .trt_dat,
                  weights = OverlapWeight
                ) |>
                summary(times = 10, extend = TRUE)

              # Make a table
              tibble(
                Type = c("Unadjusted", "Weighted"),
                Estimate = 1 - c(temp_mod1$surv, temp_mod2$surv)
              )
            }
          )
      ) |>

      # unnest
      unnest(cols = data)
  }

# Function to build Cox output
get_cox_output <-
  function(.dat) {
    temp_dat <- .dat

    # 1. Fit the cox model
    temp_cox_mod <-
      coxph(
        formula = outcome_formula,
        data = temp_dat,
        weights = OverlapWeight,
        robust = TRUE,
        model = TRUE
      ) |>
      suppressWarnings()

    # 2. Extract the baseline hazard function at 10 years
    temp_haz <- basehaz(temp_cox_mod, centered = TRUE)
    temp_haz_10 <- temp_haz$hazard[which.min(abs(10 - temp_haz$time))[1]]

    # 3. Compute 10-year survival assuming all treated and all untreated

    # Set data sets
    surg_dat <- mutate(temp_dat, Treatment = "Surgical")
    glp_dat <- mutate(temp_dat, Treatment = "Nonsurgical")

    # Surgical patients
    tibble(
      Treatment = "Surgical",
      Estimate = 1 -
        exp(
          -temp_haz_10 *
            exp(predict(temp_cox_mod, newdata = surg_dat, type = "lp"))
        )
    ) |>

      # Bind to get non-surgical patients
      bind_rows(
        tibble(
          Treatment = "Nonsurgical",
          Estimate = 1 -
            exp(
              -temp_haz_10 *
                exp(predict(temp_cox_mod, newdata = glp_dat, type = "lp"))
            )
        )
      ) |>

      # Compute the mean value within each treatment
      summarize(
        Estimate = mean(Estimate),
        .by = Treatment
      ) |>

      # Indicate group
      add_column(
        Type = "Doubly-robust"
      )
  }

# Function to get doubly-robust
get_cox_risk_diff <-
  function(.dat, outcome_formula) {
    .dat |>

      # Nest by outcome
      nest(.by = c(Outcome)) |>

      # Get the estimates
      mutate(
        data = data |> map(get_cox_output)
      ) |>

      # unnest
      unnest(cols = data)
  }

# Function to retrieve all estimates
retrieve_all_estimates <-
  function(.boot_dat, .row) {
    cat(.row, " ")

    # 1. Attach the cohorts
    temp_dat <- .boot_dat |> attach_outcomes(outcomes)

    temp_dat |>

      # Get KM estimates
      get_km_estimates() |>

      # Bind to get Cox-based estimates
      bind_rows(get_cox_risk_diff(temp_dat, outcome_formula))
  }

## Build bootstrap data sets

# Set seed for reproducibility
set.seed(123)

# Number of bootstraps
B <- 2 # ACTUAL IN MANUSCRIPT = 2000

# Repeat for each
analysis_imputed_boot <-
  1:B |>

  # Repeat for each iteration
  map_df(
    function(.boot) {
      cat(.boot, " ")

      # 1. Take a random bootstrap sample
      temp_inds <- sample(
        1:nrow(analysis_raw),
        nrow(analysis_raw),
        replace = TRUE
      )
      temp_dat <- analysis_raw[temp_inds, ]

      # 2, Build imputation data set
      build_imputation_set(temp_dat) |>

        # Indicate iteration
        add_column(B = .boot)
    }
  ) |>

  # Rearrange
  relocate(B, .before = everything()) |>

  # Nest the data set
  nest(.by = c(B, Imputation))

## Obtain the overlap weights
analysis_weighted_boot <-
  analysis_imputed_boot |>

  # Append each data set with the overlap weights
  mutate(
    data = data |> map(estimate_overlap_weights, ps_formula)
  )

## Estimate the outcome

# Bootstrap
analysis_all_risk_estimates_boot <-
  analysis_weighted_boot |>

  # For each data set
  mutate(
    data = data |>
      map2(
        .y = as.list(row_number()),
        retrieve_all_estimates
      )
  ) |>

  # Unnest the data
  unnest(cols = data) |>

  # Rearrange
  arrange(
    B,
    Imputation,
    Outcome,
    Treatment,
    Type
  )

# Full data
analysis_all_risk_estimates <-
  analysis_weighted |>

  # Nest by imputation
  nest(.by = Imputation) |>

  # For each data set
  mutate(
    data = data |>
      map2(
        .y = as.list(row_number()),
        retrieve_all_estimates
      )
  ) |>

  # Unnest the data
  unnest(cols = data) |>

  # Rearrange
  arrange(
    Imputation,
    Outcome,
    Treatment,
    Type
  )

## Build a table of risk difference
analysis_all_risk_estimates_boot |>

  # Average over imputations, within each bootstrap
  summarize(
    Estimate = mean(Estimate),
    .by = c(
      B,
      Outcome,
      Treatment,
      Type
    )
  ) |>

  # Send treatments over the columns
  pivot_wider(
    names_from = Treatment,
    values_from = Estimate
  ) |>

  # Compute the difference
  mutate(
    Difference = Nonsurgical - Surgical
  ) |>

  # Send down the rows
  pivot_longer(
    cols = c("Nonsurgical", "Surgical", "Difference"),
    names_to = "Treatment",
    values_to = "Estimate"
  ) |>

  # Get the CI's
  summarize(
    Risk = mean(Estimate),
    Lower = quantile(Estimate, 0.025),
    Upper = quantile(Estimate, 0.975),
    .by = c(
      Outcome,
      Type,
      Treatment
    )
  ) |>

  # Join to get full-cohort point estimate
  inner_join(
    y = analysis_all_risk_estimates |>

      # Average over imputations, within each bootstrap
      summarize(
        Estimate = mean(Estimate),
        .by = c(
          Outcome,
          Treatment,
          Type
        )
      ) |>

      # Send treatments over the columns
      pivot_wider(
        names_from = Treatment,
        values_from = Estimate
      ) |>

      # Compute the difference
      mutate(
        Difference = Nonsurgical - Surgical
      ) |>

      # Send down the rows
      pivot_longer(
        cols = c("Nonsurgical", "Surgical", "Difference"),
        names_to = "Treatment",
        values_to = "Estimate"
      ) |>

      # Change name
      rename(
        PointEstimate = Estimate
      ),
    by = c(
      "Outcome",
      "Type",
      "Treatment"
    )
  ) |>

  # Create summarize
  mutate(
    across(c(Risk, Lower, Upper, PointEstimate), \(x) round(x * 100, 1)),
    Summary = paste0(
      PointEstimate,
      "% (",
      Lower,
      "%, ",
      Upper,
      "%) [",
      Risk,
      "%]"
    )
  ) |>
  select(-Risk, -Lower, -Upper, -PointEstimate) |>

  # Send over columns
  pivot_wider(
    names_from = Treatment,
    values_from = Summary
  )
