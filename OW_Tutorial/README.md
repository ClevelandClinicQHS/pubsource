# Overlap weighting tutorial

An article providing an introduction to the overlap weighting (OW) methodology, a comparison to inverse probability of treatment weighting (IPTW), and some strategies for assessing weighting impact, through an applied example in patient survival.

# Publication

Alexander M Zajichek, Gary L Grunkemeier, [Statistical primer: propensity scores used as overlap weights provide exact covariate balance](https://doi.org/10.1093/ejcts/ezae318), European Journal of Cardio-Thoracic Surgery, 2024;, ezae318, https://doi.org/10.1093/ejcts/ezae318

# Code structure

1. [Generate simulated data set](simulate_dataset.R)

Generates the simulated data set for the article, motivated and guided by the CABG dataset used in [this paper](https://academic.oup.com/ejcts/article/53/6/1112/4978231). The data were simulated so to produce large separation in propensity score distributions between the treatment groups. A true odds-ratio of 0.78 was set to serve as reference for the estimation procedures.

2. [Patient characteristics and treatment effect](table1.R)

Creates a table showing the unadjusted, IPTW, and OW summaries of patient characteristics (confounders), standardized mean differences, observed inpatient mortality rates, and estimated treatment effects by treatment group.

3. [Propensity score distributions](figure1.R)

Creates a plot showing the distributions of the estimated propensity scores by treatment group through a mirrored histogram.

4. [Weighting distributions](figure2.R)

Creates a plot showing the estimated IPTW and overlap weight distributions by treatment group through a mirrored histogram, and a plot showing the cumulative contribution of patients used in estimation in the unweighted and weighted samples.

5. [Effects on propensity score](figure3.R)

Creates a plot showing the odds ratios for the confounders in the propensity score model.

6. [Weight contributions by strata](figure4.R)

Creates a plot showing the difference in weight shift from the unweighted sample by treatment group by strata of the top six most influential confounders for IPTW-adjusted, and OW-adjusted data.

7. [SMD plot](central_image.R)

Creates a plot showing the standardized mean difference of confounders in the unadjusted, IPTW adjusted and OW adjusted samples.