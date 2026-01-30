# Created: 2026-01-29
# Author: Alex Zajichek
# Project: Bariatric Surgery vs. GLP-1
# Description: Simulates baseline analytical dataset with synthetic data

# Load packages
library(tidyverse)

# Set attributes
set.seed(123456)
N_trt <- 1657 # Sample size of treated (bariatric surgery)
N_ctrl <- 2275 # Sample size of untreated (GLP-1)

# Make raw analysis dataset
analysis_raw <-
  bind_rows(
    # Bariatric Surgery
    tibble(
      PatientID = 1:N_trt,
      Treatment = "Surgical",

      # Demographics
      IndexDate = rnorm(N_trt, 2014, 2.2),
      Sex = rbinom(N_trt, 1, 0.657), # Female
      Age = rnorm(N_trt, 51, 11.3),
      Race = sample(
        c("Black", "Missing", "Other", "White"),
        N_trt,
        replace = TRUE,
        prob = c(0.20, 0.018, 0.032, 0.75)
      ),
      Income = rnorm(N_trt, 53.7, 19.6),
      SmokingStatus = sample(
        c("Current", "Former", "Missing", "Never"),
        N_trt,
        replace = TRUE,
        prob = c(0.025, 0.39, 0.05, 0.535)
      ),
      Location = rbinom(N_trt, 1, 0.259), # Florida

      # Clinical/Lab
      BMI = rnorm(N_trt, 46.3, 8.8),
      BP_SYSTOLIC = rnorm(N_trt, 130.9, 15.8),
      TG = rnorm(N_trt, 176.7, 141.4),
      HDLc = rnorm(N_trt, 44.5, 11.6),
      LDLc = rnorm(N_trt, 95.8, 33.5),
      HBA1c = rnorm(N_trt, 7.5, 1.6),
      eGFR = rnorm(N_trt, 90.9, 22.8),
      UACR = rnorm(N_trt, 98.5, 362.7),

      # Medication History
      CountAntiDiabeticsNonInsulin = sample(
        c(0, 1, 2, 3, 4),
        N_trt,
        replace = TRUE,
        prob = c(0.326, 0.471, 0.16, 0.042, 0.001)
      ),
      INSULIN = rbinom(N_trt, 1, 0.268),
      LIPID_LOWERING = rbinom(N_trt, 1, 0.492),
      RAAS_INHIBITOR = rbinom(N_trt, 1, 0.54),
      OTHER_ANTIHTN = rbinom(N_trt, 1, 0.53),
      ASPIRIN = rbinom(N_trt, 1, 0.221),
      WARFARIN = rbinom(N_trt, 1, 0.034),

      # Medical History
      HYPERTENSION = rbinom(N_trt, 1, 0.747),
      DYSLIPIDEMIA = rbinom(N_trt, 1, 0.622),
      NEUROPATHY = rbinom(N_trt, 1, 0.092),
      HEART_FAILURE = rbinom(N_trt, 1, 0.056),
      CAD = rbinom(N_trt, 1, 0.073),
      COPD = rbinom(N_trt, 1, 0.069),
      AFib = rbinom(N_trt, 1, 0.045),
      PAD = rbinom(N_trt, 1, 0.031),
      CVE = rbinom(N_trt, 1, 0.016),
      MI = rbinom(N_trt, 1, 0.016),
      NEPHROPATHY = rbinom(N_trt, 1, 0.147),
      RETINOPATHY = rbinom(N_trt, 1, 0.021)
    ) |>

      # Add missingness
      mutate(
        TG = case_when(rbinom(N_trt, 1, 0.11) == 1 ~ NA_real_, TRUE ~ TG),
        HDLc = case_when(rbinom(N_trt, 1, 0.11) == 1 ~ NA_real_, TRUE ~ HDLc),
        LDLc = case_when(rbinom(N_trt, 1, 0.118) == 1 ~ NA_real_, TRUE ~ LDLc),
        HBA1c = case_when(
          rbinom(N_trt, 1, 0.097) == 1 ~ NA_real_,
          TRUE ~ HBA1c
        ),
        UACR = case_when(rbinom(N_trt, 1, 0.628) == 1 ~ NA_real_, TRUE ~ UACR)
      ),

    # GLP-1
    tibble(
      PatientID = (N_trt + 1):(N_trt + N_ctrl),
      Treatment = "Nonsurgical",

      # Demographics
      IndexDate = rnorm(N_ctrl, 2016.3, 1.3),
      Sex = rbinom(N_ctrl, 1, 0.535), # Female
      Age = rnorm(N_ctrl, 56.6, 10.2),
      Race = sample(
        c("Black", "Missing", "Other", "White"),
        N_ctrl,
        replace = TRUE,
        prob = c(0.139, 0.025, 0.03, 0.806)
      ),
      Income = rnorm(N_ctrl, 56.2, 19.5),
      SmokingStatus = sample(
        c("Current", "Former", "Missing", "Never"),
        N_ctrl,
        replace = TRUE,
        prob = c(0.091, 0.385, 0.007, 0.517)
      ),
      Location = rbinom(N_ctrl, 1, 0.124), # Florida

      # Clinical/Lab
      BMI = rnorm(N_ctrl, 38.6, 6.9),
      BP_SYSTOLIC = rnorm(N_ctrl, 128.9, 15.5),
      TG = rnorm(N_ctrl, 177.2, 155.5),
      HDLc = rnorm(N_ctrl, 42.8, 12.3),
      LDLc = rnorm(N_ctrl, 85.6, 33),
      HBA1c = rnorm(N_ctrl, 7.6, 1.5),
      eGFR = rnorm(N_ctrl, 88.6, 21.4),
      UACR = rnorm(N_ctrl, 87.6, 388.9),

      # Medication History
      CountAntiDiabeticsNonInsulin = sample(
        c(0, 1, 2, 3, 4),
        N_ctrl,
        replace = TRUE,
        prob = c(0.223, 0.426, 0.272, 0.067, 0.012)
      ),
      INSULIN = rbinom(N_ctrl, 1, 0.333),
      LIPID_LOWERING = rbinom(N_ctrl, 1, 0.66),
      RAAS_INHIBITOR = rbinom(N_ctrl, 1, 0.653),
      OTHER_ANTIHTN = rbinom(N_ctrl, 1, 0.506),
      ASPIRIN = rbinom(N_ctrl, 1, 0.249),
      WARFARIN = rbinom(N_ctrl, 1, 0.023),

      # Medical History
      HYPERTENSION = rbinom(N_ctrl, 1, 0.76),
      DYSLIPIDEMIA = rbinom(N_ctrl, 1, 0.748),
      NEUROPATHY = rbinom(N_ctrl, 1, 0.166),
      HEART_FAILURE = rbinom(N_ctrl, 1, 0.04),
      CAD = rbinom(N_ctrl, 1, 0.098),
      COPD = rbinom(N_ctrl, 1, 0.066),
      AFib = rbinom(N_ctrl, 1, 0.056),
      PAD = rbinom(N_ctrl, 1, 0.046),
      CVE = rbinom(N_ctrl, 1, 0.017),
      MI = rbinom(N_ctrl, 1, 0.027),
      NEPHROPATHY = rbinom(N_ctrl, 1, 0.142),
      RETINOPATHY = rbinom(N_ctrl, 1, 0.062)
    ) |>

      # Add missingness
      mutate(
        BP_SYSTOLIC = case_when(
          rbinom(N_ctrl, 1, 0.005) == 1 ~ NA_real_,
          TRUE ~ BP_SYSTOLIC
        ),
        TG = case_when(rbinom(N_ctrl, 1, 0.205) == 1 ~ NA_real_, TRUE ~ TG),
        HDLc = case_when(rbinom(N_ctrl, 1, 0.204) == 1 ~ NA_real_, TRUE ~ HDLc),
        LDLc = case_when(rbinom(N_ctrl, 1, 0.204) == 1 ~ NA_real_, TRUE ~ LDLc),
        HBA1c = case_when(
          rbinom(N_ctrl, 1, 0.148) == 1 ~ NA_real_,
          TRUE ~ HBA1c
        ),
        eGFR = case_when(rbinom(N_ctrl, 1, 0.061) == 1 ~ NA_real_, TRUE ~ eGFR),
        UACR = case_when(rbinom(N_ctrl, 1, 0.583) == 1 ~ NA_real_, TRUE ~ UACR)
      )
  ) |>

  # Clean up labels/types
  mutate(
    # Factors
    Sex = case_when(Sex == 1 ~ "Female", TRUE ~ "Male"),
    Location = case_when(Location == 1 ~ "Florida", TRUE ~ "Ohio"),
    across(
      c(Treatment, Sex, Location, Race, SmokingStatus),
      factor
    ),

    # Logical indicators
    across(
      c(
        INSULIN,
        LIPID_LOWERING,
        RAAS_INHIBITOR,
        OTHER_ANTIHTN,
        ASPIRIN,
        WARFARIN,
        HYPERTENSION,
        DYSLIPIDEMIA,
        NEUROPATHY,
        HEART_FAILURE,
        CAD,
        COPD,
        AFib,
        PAD,
        CVE,
        MI,
        NEPHROPATHY,
        RETINOPATHY
      ),
      as.logical
    )
  )

# Write to file (current directory)
#analysis_raw |> write_rds(file = "data/analysis_raw.rds")
#analysis_raw |> write_csv(file = "data/analysis_raw.csv")
