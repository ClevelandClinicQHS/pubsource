# Bariatric Surgery vs. GLP-1

A real-world evidence (RWE) study comparing macro/micro-vascular outcomes in obese patients receiving bariatric surgery versus those receiving GLP-1 RA.

# Publication

Gasoyan, H., Alavi, M.H., Zajichek, A. et al. [Macrovascular and microvascular outcomes of metabolic surgery versus GLP-1 receptor agonists in patients with diabetes and obesity](https://www.nature.com/articles/s41591-025-03893-3). Nat Med 31, 3341–3349 (2025). https://doi.org/10.1038/s41591-025-03893-3

# Code structure

1. [Generate simulated data set](01sim_baseline_dat.R)

Builds a synthetic dataset to represent the structure of the raw baseline analytical dataset used in the actual analysis. Although this the resulting dataset produces fake data, its metadata (i.e., row count, column names, etc.) matches that of the actual study dataset. Additionally, baseline variables were simulated from a normal distribution to approximately match the mean/SD (or binomial/multinomial distribution to match percentages) as reported in the manuscript (see [Table 1](https://www.nature.com/articles/s41591-025-03893-3/tables/1)).