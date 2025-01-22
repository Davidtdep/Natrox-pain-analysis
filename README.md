# Natrox-pain-analysis

This repository contains the R script used for the analyses in the study on pain reduction in patients treated with Natrox therapy.

## Purpose
This script demonstrates the statistical analysis of pain levels before and after Natrox therapy. The analyses aim to:
- Evaluate overall pain reduction.
- Compare continuous and discontinuous treatments.
- Investigate associations between clinical conditions and pain outcomes.

## Required R packages
The following R packages are necessary to execute the analyses:
- ggplot2
- ggalluvial
- MASS
- dplyr

## Analyses included
This script performs the following analyses:
- **Overall pain reduction**: Paired Wilcoxon signed-rank test for ordinal data.
- **Subgroup analysis**: Wilcoxon tests for continuous and discontinuous treatment groups.
- **Comparative effectiveness**: Comparison of pain reduction between the two groups (Mann-Whitney U test or t-test).
- **Logistic regression**: Binary logistic regression to estimate odds ratios for "No Pain" based on clinical conditions.
- **Visualization**: Sankey plots illustrating pain transitions before and after treatment.

## How to use
1. Download or clone this repository.
2. Prepare a dataset (`data.csv`) with the format described in the script.
3. Open the script (`main_analysis.R`) in RStudio.
4. Install the required R packages (see above).
5. Run the script to replicate the analyses.

## Data availability
The data file (`data.csv`) used in this study is not publicly available but can be provided upon reasonable request. The script demonstrates the workflow for the analyses without requiring access to the raw data.

## License
This repository is licensed under CC0 1.0 Universal (Public Domain). The code is free to use, modify, and distribute without restrictions.
