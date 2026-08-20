Pooled Analysis of NTD Long-Term Outcomes in Eastern Africa

This repository contains the R code used to conduct the meta-analysis for:

"The Long-Term Outcomes of Neural Tube Defects in Eastern Africa: A Systematic Review and Meta-Analysis"
Preprint: https://doi.org/10.64898/2026.06.26.26356687

Contents
Script	Description
Ntd_forest_final.R	Generates forest plots of pooled prevalence/proportion estimates by outcome, using random-effects meta-analysis (PLOGIT transformation, maximum likelihood tau estimator).
Ntd_Subttype_new.R	Subgroup analysis of outcomes by NTD subtype.
Risk of Bias.R	Risk-of-bias assessment and visualisation across included studies.
Sensitivity.R	Sensitivity analyses assessing the robustness of pooled estimates.
Funnel plot.R	Funnel plots for assessing potential publication bias.
Requirements

Analyses were conducted in R (version version 4.5.3) using the meta package.

Data

The dataset used in these analyses is provided as a supplementary file accompanying the published manuscript/preprint (see link above).

Citation

If you use this code, please cite the associated preprint/manuscript above.
