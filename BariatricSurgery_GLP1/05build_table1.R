# Created: 2026-01-30
# Author: Alex Zajichek
# Project: Bariatric Surgery vs. GLP-1
# Description: Builds Table 1

# Load packages
library(tidyverse)
library(smd)

## Import datasets

# Raw analysis data
analysis_raw <- read_rds(file = "data/analysis_raw.rds")

# Weighted dataset
analysis_weighted <- read_rds(file = "data/analysis_weighted.rds")

## Define function to compute summary/weighted metrics

# Unweighted summary (with potentially missing data)
unweighted_summary <-
  function(.col) {
    # Extract the class
    col_type <- class(.col)

    # Check type
    if (col_type == "character" | col_type == "factor") {
      # Make a frequency table
      this_sum <-
        table(.col, useNA = 'ifany') |>
        enframe(name = "Level", value = "Summary") |>
        mutate(
          Level = case_when(
            Level == "" ~ "(Missing)",
            TRUE ~ Level
          ),
          Summary = Summary |>
            as.numeric(),
          Summary = paste0(
            Summary,
            " (",
            round(100 * Summary / sum(Summary), 1),
            "%)"
          )
        )

      # Check for indicator columns
    } else if (n_distinct(.col, na.rm = TRUE) <= 2) {
      # Compute the rate
      this_sum <- sum(coalesce(.col, 0))
      this_sum <- paste0(
        this_sum,
        " (",
        round(100 * (this_sum / length(.col)), 1),
        "%)"
      )
      this_sum <- tibble(Level = NA_character_, Summary = this_sum)
    } else {
      # Get the summary stats
      this_sum <- round(c(mean(.col, na.rm = TRUE), sd(.col, na.rm = TRUE)), 1)
      this_sum <- paste0(this_sum[1], " (", this_sum[2], ")")
      this_sum <- tibble(Level = NA_character_, Summary = this_sum)

      # Get missing counts
      missing <- sum(is.na(.col))
      if (missing > 0) {
        this_sum <-
          this_sum |>
          bind_rows(
            tibble(
              Level = "(Missing)",
              Summary = paste0(
                missing,
                " (",
                round(100 * missing / length(.col), 1),
                "%)"
              )
            )
          )
      }
    }

    this_sum
  }

# Weighted quantiles
weighted.quantile <- function(x, w, q) {
  w <- w[order(x)]
  x <- x[order(x)]
  prob <- cumsum(w) / sum(w)
  ps <- which(abs(prob - q) == min(abs(prob - q)))
  return(x[ps][1])
}

# Weighted summaries
weighted_summary <-
  function(.col, .weights) {
    # Get the data type
    this_class <- class(.col)

    if (this_class %in% c("numeric", "integer")) {
      # Compute the weighted summaries
      this_mean <- Hmisc::wtd.mean(.col, .weights)
      this_sd <- Hmisc::wtd.var(.col, .weights, normwt = TRUE) |> sqrt()

      # Put into frame
      tibble(
        Metric = c("Mean", "SD"),
        Value = c(this_mean, this_sd)
      )
    } else if (this_class == "logical") {
      # Compute the weighted average
      tibble(
        Metric = "Percent",
        Value = sum(.col * .weights) / sum(.weights)
      )
    } else {
      # Make weighted proportions
      this_prop <- tapply(.weights, .col, sum)
      this_prop <- this_prop / sum(this_prop)

      # Make a frame
      tibble(
        Metric = "Percent",
        Level = names(this_prop),
        Value = as.numeric(this_prop)
      )
    }
  }

# Convert to factor
analysis_raw <-
  analysis_raw |>

  # Convert to factor
  mutate(
    CountAntiDiabeticsNonInsulin_numeric = CountAntiDiabeticsNonInsulin,
    CountAntiDiabeticsNonInsulin = factor(CountAntiDiabeticsNonInsulin)
  )

analysis_weighted <-
  analysis_weighted |>

  # Convert to factor
  mutate(
    CountAntiDiabeticsNonInsulin_numeric = CountAntiDiabeticsNonInsulin,
    CountAntiDiabeticsNonInsulin = factor(CountAntiDiabeticsNonInsulin)
  )

## Compute the summary tables

# Unweighted (for each treatment)
analysis_raw |>

  # Nest by imputation data set and treatment
  group_by(Treatment) |>
  nest() |>

  # Get summary for each set
  mutate(
    unweighted_summaries = data |>
      map(
        function(.dat) {
          .dat |>

            # Remove columns don't want
            select(-PatientID) |>

            # For each column, get the summary
            map_df(
              unweighted_summary,
              .id = "Factor"
            )
        }
      )
  ) |>

  # Unnest the data
  select(-data) |>
  unnest(cols = unweighted_summaries) |>
  ungroup() |>

  # Indicate group
  add_column(
    Type = "Unweighted"
  ) |>

  # Bind to get the weighted summaries
  bind_rows(
    analysis_weighted |>

      # Nest by imputation data set and treatment
      group_by(Imputation, Treatment) |>
      nest() |>

      # Get summary for each set
      mutate(
        weighted_summaries = data |>
          map(
            function(.dat) {
              .dat |>

                # Remove columns don't want
                select(-PatientID, -OverlapWeight) |>

                # For each column, get the summary
                map_df(
                  weighted_summary,
                  .weights = .dat$OverlapWeight,
                  .id = "Factor"
                )
            }
          )
      ) |>

      # Unnest the data
      select(-data) |>
      unnest(cols = weighted_summaries) |>
      ungroup() |>

      # Average over the imputations
      summarize(
        Value = mean(Value),
        .by = c(
          Factor,
          Level,
          Treatment,
          Metric
        )
      ) |>

      # Send over the columns
      pivot_wider(
        names_from = Metric,
        values_from = Value
      ) |>

      # Consolidate to single summary
      mutate(
        Percent = Percent * 100,
        across(
          c(Mean, SD, Percent),
          \(x) round(x, 1)
        ),
        Summary = case_when(
          is.na(Percent) ~ paste0(Mean, " (", SD, ")"),
          TRUE ~ as.character(Percent)
        )
      ) |>

      # Keep a few columns
      select(
        Factor,
        Level,
        Treatment,
        Summary
      ) |>

      # Indicate group
      add_column(
        Type = "Weighted"
      )
  ) |>

  # Send over the columns
  pivot_wider(
    names_from = c(Treatment, Type),
    values_from = Summary
  ) |>

  # Join to get the SMD's
  left_join(
    y = analysis_raw |>

      # Remove columns don't want
      select(-PatientID, -Treatment) |>

      # Compute the SMD for each column
      map_df(
        function(.col) {
          # Get the SMD
          this_smd <- smd(
            .col,
            g = analysis_raw$Treatment,
            na.rm = TRUE
          )$estimate

          # Make a data frame
          tibble(
            SMD = this_smd
          )
        },
        .id = "Factor"
      ) |>

      # Indicate group
      add_column(
        Type = "Unweighted"
      ) |>

      # Bind to get the weighted SMD's
      bind_rows(
        analysis_weighted |>

          # Nest by imputation data
          group_by(Imputation) |>
          nest() |>

          # Get the SMD for each column
          mutate(
            smds = data |>
              map(
                function(.dat) {
                  .dat |>

                    # Remove columns don't want
                    select(-PatientID, -OverlapWeight, -Treatment) |>

                    # Compute the SMD for each column
                    map_df(
                      function(.col) {
                        # Get the SMD
                        this_smd <- smd(
                          .col,
                          g = .dat$Treatment,
                          w = .dat$OverlapWeight
                        )$estimate

                        # Make a data frame
                        tibble(
                          SMD = this_smd
                        )
                      },
                      .id = "Factor"
                    )
                }
              )
          ) |>

          # Unnest
          select(-data) |>
          unnest(cols = smds) |>
          ungroup() |>

          # Average over imputations
          summarize(
            SMD = mean(SMD),
            .by = Factor
          ) |>

          # Indicate group
          add_column(
            Type = "Weighted"
          )
      ) |>

      # Send over columns
      pivot_wider(
        names_from = Type,
        values_from = SMD,
        names_prefix = "SMD"
      ),
    by = "Factor"
  ) |>

  # Join to get clean labels
  left_join(
    y = tribble(
      ~Factor                                , ~Label                                       ,
      "IndexDate"                            , "Index Date"                                 ,
      "Age"                                  , "Age (years)"                                ,
      "Income"                               , "Income (thousands of dollars)"              ,
      "SmokingStatus"                        , "Smoking Status"                             ,
      "CountAntiDiabeticsNonInsulin"         , "Count of Non-insulin Medications"           ,
      "CountAntiDiabeticsNonInsulin_numeric" , "Count of Non-insulin Medications (numeric)"
    ), # OTHER CLEAN LABEL SOURCES OMMITTED
    by = "Factor"
  ) |>

  # Fill in
  mutate(
    Lookup = Factor,
    Factor = coalesce(Label, Factor)
  ) |>
  select(-Label) |>

  # Rearrange
  select(
    Factor,
    Level,
    Surgical_Unweighted,
    Nonsurgical_Unweighted,
    SMDUnweighted,
    Surgical_Weighted,
    Nonsurgical_Weighted,
    SMDWeighted
  ) |>

  # Arrange
  arrange(Factor) |>

  # Clean up
  mutate(
    across(
      starts_with("SMD"),
      \(x) case_when(duplicated(x) ~ "", TRUE ~ as.character(round(x, 3)))
    ),
    across(everything(), \(x) coalesce(x, "")),
    .by = Factor
  ) |>
  mutate(
    Factor = case_when(duplicated(Factor) ~ "", TRUE ~ Factor)
  )
