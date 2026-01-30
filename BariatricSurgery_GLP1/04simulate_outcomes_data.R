# Created: 2026-01-30
# Author: Alex Zajichek
# Project: Bariatric Surgery vs. GLP-1
# Description: Simulates outcome data to use for (mock) analysis

# Load packages
library(tidyverse)

## Import dataset

# Weighted dataset
analysis_weighted <- read_rds(file = "data/analysis_weighted.rds")

## Simulate outcomes data
set.seed(123456)

# Make joinable list of event rates by treatment (taken from Table 2)
event_rates <-
  tribble(
    ~Treatment    , ~Outcome      , ~EventRate ,
    "Surgical"    , "Death"       , 108 / 1657 ,
    "Surgical"    , "MACE"        , 220 / 1657 ,
    "Surgical"    , "Nephropathy" , 218 / 1657 ,
    "Surgical"    , "Retinopathy" , 52 / 1657  ,
    "Nonsurgical" , "Death"       , 153 / 2275 ,
    "Nonsurgical" , "MACE"        , 488 / 2275 ,
    "Nonsurgical" , "Nephropathy" , 440 / 2275 ,
    "Nonsurgical" , "Retinopathy" , 191 / 2275
  )

## Make outcome dataset (1 observed response per outcome per patient)

# Get the initial full join
outcome_temp <-
  analysis_weighted |>

  # Keep 1 row per unique patient
  select(PatientID, Treatment) |>
  distinct() |>

  # Join to attach outcome rates
  inner_join(
    y = event_rates,
    by = "Treatment",
    relationship = "many-to-many"
  )

# Sample outcome indicators/event times
outcomes <-
  outcome_temp |>

  # Simulate outcome (and censoring) indicators based on observed rate; sample time to event
  mutate(
    Status = rbinom(nrow(outcome_temp), 1, EventRate),
    Time = runif(nrow(outcome_temp), 0, 10)
  ) |>

  # Keep necessary columns
  select(
    PatientID,
    Outcome,
    Time,
    Status
  )

## Add outcomes to full analysis dataset
analysis_outcome <-
  analysis_weighted |>

  # Join to get outcomes
  inner_join(
    y = outcomes,
    by = "PatientID",
    relationship = "many-to-many"
  ) |>

  # Reorder columns
  relocate(
    Outcome,
    Time,
    Status,
    .before = Treatment
  )

# Write to file (current directory)
#analysis_outcome |> write_rds(file = "data/analysis_outcome.rds")
#analysis_outcome |> write_csv(file = "data/analysis_outcome.csv")
