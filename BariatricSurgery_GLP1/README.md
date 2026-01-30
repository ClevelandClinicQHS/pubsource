# Bariatric Surgery vs. GLP-1

A real-world evidence (RWE) study comparing macro/micro-vascular outcomes in obese patients receiving bariatric surgery versus those receiving GLP-1 RA.

# Publication

Gasoyan, H., Alavi, M.H., Zajichek, A. et al. [Macrovascular and microvascular outcomes of metabolic surgery versus GLP-1 receptor agonists in patients with diabetes and obesity](https://www.nature.com/articles/s41591-025-03893-3). Nat Med 31, 3341–3349 (2025). https://doi.org/10.1038/s41591-025-03893-3

# Code structure

This section goes through the sequential data and analysis pipelines for each analytic step. You can run the scripts in order to reproduce results, or use the various intermediate datasets stored [here](data).

1. [Generate simulated data set](01simulate_baseline_data.R)

Builds a synthetic dataset to represent the structure of the raw baseline analytical dataset used in the actual analysis. Although this the resulting dataset produces fake data, its metadata (i.e., row count, column names, etc.) matches that of the actual study dataset. Additionally, baseline variables were simulated from a normal distribution to approximately match the mean/SD (or binomial/multinomial distribution to match percentages) as reported in the manuscript (see [Table 1](https://www.nature.com/articles/s41591-025-03893-3/tables/1)).

2. [Impute missing data](02build_imputation_data.R)

Builds imputation datasets based on [Multiple Imputation by Chained Equations (MICE)](https://pmc.ncbi.nlm.nih.gov/articles/PMC3074241/) via the [`mice`](https://cran.r-project.org/web/packages/mice/index.html) package in R. All baseline variables are used in the imputation (no outcome information). Five (5) complete imputation datasets are retained to conduct further analysis downstream.

3. [Build weighted datasets](03build_weighted_data.R)

Builds the weighted datasets (independently for each imputation dataset) for analysis. Uses the [overlap weighting](https://pubmed.ncbi.nlm.nih.gov/30189042/) methodology to get case weights in order to estimate the _average treatment effect in the overlap population (ATO)_. Propensity scores for treatment assignment (bariatric surgery or GLP-1 RA) are estimated via an additive logistic regression model with all baseline variables as covariates (except `IndexDate`). Overlap weights are normalized to sum to one (1) within each treatment group.