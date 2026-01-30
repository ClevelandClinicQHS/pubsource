# Created: 2026-01-30
# Author: Alex Zajichek
# Project: Bariatric Surgery vs. GLP-1
# Description: Builds imputation datasets

# Load packages
library(tidyverse)

## Import data

# Raw analysis data
analysis_raw <- read_rds(file = "data/analysis_raw.rds")

## Build imputation data sets

# Set the seed for reproducibility
set.seed(123)

# Run imputation (based on baseline information only)
analysis_imputed <-
  analysis_raw |>

  # Remove the patient identifier from imputation
  select(-PatientID) |>

  # Run the imputation process
  mice::mice(m = 5) |>

  # Get the imputed data sets
  mice::complete("long") |>
  as_tibble() |>

  # Join to re-attach patient ID
  inner_join(
    y = tibble(
      .id = seq_along(analysis_raw$PatientID),
      PatientID = analysis_raw$PatientID
    ),
    by = ".id"
  ) |>

  # Remove artificial ID
  select(-.id) |>

  # Rerrange
  rename(Imputation = .imp) |>
  relocate(PatientID, .after = Imputation)

# Write to file (current directory)
#analysis_imputed |> write_rds(file = "data/analysis_imputed.rds")
#analysis_imputed |> write_csv(file = "data/analysis_imputed.csv")
