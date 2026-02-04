# Bariatric Surgery vs. GLP-1

A real-world evidence (RWE) study comparing macro/micro-vascular outcomes in obese patients receiving bariatric surgery versus those receiving GLP-1 RA.

# Publication

Gasoyan, H., Alavi, M.H., Zajichek, A. et al. [Macrovascular and microvascular outcomes of metabolic surgery versus GLP-1 receptor agonists in patients with diabetes and obesity](https://www.nature.com/articles/s41591-025-03893-3). Nat Med 31, 3341–3349 (2025). https://doi.org/10.1038/s41591-025-03893-3

# Code structure

This section goes through the sequential data and analysis pipelines for each analytic step. You can run the scripts in order to reproduce results, or use the various intermediate datasets stored [here](data).

1. [Simulated Dataset](01simulate_baseline_data.R)

Builds a synthetic dataset to represent the structure of the raw baseline analytical dataset used in the actual analysis. Although this the resulting dataset produces fake data, its metadata (i.e., row count, column names, etc.) matches that of the actual study dataset. Additionally, baseline variables were simulated from a normal distribution to approximately match the mean/SD (or binomial/multinomial distribution to match percentages) as reported in the manuscript (see [Table 1](https://www.nature.com/articles/s41591-025-03893-3/tables/1)).

2. [Missing Data Imputation](02build_imputation_data.R)

Builds imputation datasets based on [Multiple Imputation by Chained Equations (MICE)](https://pmc.ncbi.nlm.nih.gov/articles/PMC3074241/) via the [`mice`](https://cran.r-project.org/web/packages/mice/index.html) package in R. All baseline variables are used in the imputation (no outcome information). Five (5) complete imputation datasets are retained to conduct further analysis downstream.

3. [Weighting](03build_weighted_data.R)

Builds the weighted datasets (independently for each imputation dataset) for analysis. Uses the [overlap weighting](https://pubmed.ncbi.nlm.nih.gov/30189042/) methodology to get case weights in order to estimate the _average treatment effect in the overlap population (ATO)_. Propensity scores for treatment assignment (bariatric surgery or GLP-1 RA) are estimated via an additive logistic regression model with all baseline variables as covariates (except `IndexDate`). Overlap weights are normalized to sum to one (1) within each treatment group.

4. [Simulated Outcomes](04simulate_outcomes_data.R)

Simulates outcome data to use in analytic workflow. These mock outcomes data are built to reflect the four (4) primary time-to-event outcomes addressed in the [manuscript](https://www.nature.com/articles/s41591-025-03893-3): _all-cause mortality_, _MACE_, _nephropathy_, and _retinopathy_. Additionally, outcomes were simulated to approximately reflect the event rates observed in the real data within each treatment group (see [Table 2](https://www.nature.com/articles/s41591-025-03893-3/tables/2)). To do this, binary indicator variables were simulated (from a [Bernoulli](https://en.wikipedia.org/wiki/Bernoulli_distribution) distribution) with a success probability set at each of the respective observed outcome rates. This determined whether a patient experienced the event or was censored. Then, a [Uniform](https://en.wikipedia.org/wiki/Continuous_uniform_distribution) random variable between 0 and 10 was simulated to represent the time to event or censoring/last follow-up (in years).

The output of this script produces a dataset called `analysis_outcome` which is taken to be the full analytic dataset that includes baseline variables, outcomes, imputations, and overlap weights.

5. [Table 1](05build_table1.R)

Uses the raw dataset (`analysis_raw`) and weighted dataset (`analysis_weighted`) to construct [Table 1](https://www.nature.com/articles/s41591-025-03893-3/tables/1) from the [manuscript](https://www.nature.com/articles/s41591-025-03893-3) (with _fake_ data). This includes code for variable summarization and [standardized mean difference (SMD)](https://bsaul.github.io/smd/index.html) calculation. Some formatting steps are ommitted such as "clean" field labels, categorizing and sorting variables, and exporting to a table output with `knitr`. The goal of this is to show the analytical steps taken to construct the table, such as how the SMD's were computed and how statistical summaries were handled across imputation datasets.

6. [Cumulative Incidence](06km_curves.R)

Uses the full analytic dataset (`analysis_outcome`) to estimate weighted cumulative incidence using the [Kaplan-Meier](https://en.wikipedia.org/wiki/Kaplan%E2%80%93Meier_estimator) method (see [Figure 1](https://www.nature.com/articles/s41591-025-03893-3/figures/1) and [Table 2](https://www.nature.com/articles/s41591-025-03893-3/tables/2)). For the figures, certain formatting options (e.g., location of text labels) were omitted as those were tailored to the actual dataset.

7. [Hazard Ratios](07hazard_ratios.R)

Uses the full analytic dataset (`analysis_outcome`) to estimate hazard ratios for the treatment effect (bariatric surgery vs. GLP-1) using an adjusted [Cox Proportional-Hazards](https://en.wikipedia.org/wiki/Proportional_hazards_model) model weighted by the overlap weights (see [Table 2](https://www.nature.com/articles/s41591-025-03893-3/tables/2)). Estimates are pooled across imputation datasets using [Rubin's Rules](https://bookdown.org/mwheymans/bookmi/rubins-rules.html).

8. [Absolute Risk Difference](08risk_difference.R)

Uses the raw (`analysis_raw`), weighted (`analysis_weighted`) and outcomes (`outcomes`) datasets to estimate the 10-year absolute risk difference for each outcome (see [Table 2](https://www.nature.com/articles/s41591-025-03893-3/tables/2)). Adjusted methods are based on [this paper](https://pubmed.ncbi.nlm.nih.gov/19595575/). Confidence intervals are obtained via bootstrapping with the percentile method across imputation datasets. _Note: In the actual manuscript analysis, B=2000 bootstrap iterations were performed. We set this value to B=2 in this script for computational reasons because every step of the analytical workflow is repeated within each bootstrap sample (including imputation and weighting), but feel free to change it._

9. [Event Rate Per 100 Years](09event_rate_per_100.R)

Uses the full analytic dataset (`analysis_outcome`) to estimate the event rate per 100 years of follow-up (see see [Table 2](https://www.nature.com/articles/s41591-025-03893-3/tables/2)). This is done by dividing the total number of events by the total length of follow-up for each group and assessing the difference. Confidence intervals were obtained via bootstrap with the percentile method. _Note: In the actual manuscript analysis, B=1000 bootstrap iterations were performed. We set this value to B=2 in this script for computational reasons._