# Overlap weighting tutorial

* Journal: _The European Journal of Cardiothoracic Surgery_
* Published: 
* Link: 

# Code structure

1. [Generate simulated data set](simulate_dataset.R)

Generates the simulated survival analysis data set for the article (with motivation from this [SAS example](https://www2.stat.duke.edu/~fl35/OW/OW_survival_Demo.sas)). Distributions of event times and patient characteristics were informed by [this paper](https://pubmed.ncbi.nlm.nih.gov/22361330/). The true event times are assumed to follow an Exponential distribution with a 56% baseline 10-year survival rate for the control group, and the assumed hazard ratio of treatment to control is 0.70. It is also assumed that older, male patients with lower ejection fraction are more likely to receive treatment _and_ are at higher risk of the outcome. Patients are censored at a uniformly random time point between 0 and 12 years of follow-up, if it precedes the event time. We also attach the estimated propensity scores (PS), inverse probability of treatment weights (IPTW), and overlap weights (OW) to the data set.

2. [Crude versus true effect plot](figure1.R)

Creates a plot showing the unadjusted Kaplan-Meier curve from the observed data next to the true treatment effect in the counterfactual distributions.

3. [Propensity score distributions](figure2.R)

Creates a plot showing the distributions of the estimated propensity scores by treatment group through a mirrored histogram.

4. [Weighting distributions](figure3.R)

Creates a plot showing the estimated IPTW and overlap weight distributions by treatment group through a mirrored histogram.

5. [Standardized mean differences](figure4.R)

Creates a plot showing the standardized mean differences between treatment groups for the unadjusted, IPTW, and OW data.

6. [Estimated treatment effect](figure5.R)

Creates a plot showing the estimated Kaplan-Meier curves by treatment group for the IPTW and OW adjusted data.

7. [Estimated hazard ratios](table2.R)

Creates a table showing the estimated hazard ratios from a Cox-PH model for the unadjusted, IPTW-adjusted, and OW-adjusted data. Also show the true ATE and ATO.

8. [Effects on propensity score](figure6.R)

Creates a plot showing the relationships between the confounders and estimated propensity scores.

9. [Cumulative contribution of patients](figure7.R)

Creates a plot showing the cumulative contribution of patients used in estimation in the unweighted and weighted samples.

10. [Weight contributions by strata](figure8.R)

Creates a plot showing the proportion of weight accounted by treatment group by strata of each confounder in the unweighted, IPTW-adjusted, and OW-adjusted data.