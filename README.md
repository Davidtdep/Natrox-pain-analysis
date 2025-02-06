# Natrox-pain-analysis

This repository contains the R script used for the analyses in the study on pain reduction in patients treated with Natrox therapy.

## Purpose
This script demonstrates the statistical analysis of pain levels before and after Natrox therapy. The analyses aim to:
- Evaluate overall pain reduction.
- Compare continuous and discontinuous treatments.
- Investigate associations between clinical conditions and pain outcomes.
- Analyze wound size reduction (length, width, area) before and after therapy.
- Assess effect sizes (Cramér's V and Wilcoxon r).

## Required R packages
The following R packages are necessary to execute the analyses:
- **ggplot2**
- **ggalluvial**
- **MASS**
- **dplyr**
- **tidyr**
- **ggpubr**
- **rcompanion**
- **effsize**

## Analyses included
This script performs the following analyses:

1. **Data cleaning**  
   - Reading the CSV data file.  
   - Fixing inconsistencies in certain columns.

2. **Overall pain reduction**  
   - Paired Wilcoxon signed-rank test for ordinal data (pain levels before vs. after).

3. **Subgroup analysis by treatment continuity**  
   - Wilcoxon tests for continuous vs. discontinuous Natrox treatment groups.

4. **Comparative effectiveness**  
   - Comparison of absolute pain reduction between the two groups (using Mann-Whitney U or t-test, based on normality and variance).

5. **Proportion analysis**  
   - Calculation of the proportion of patients with "No Pain" after treatment.  
   - Fisher’s exact test to compare "No Pain" vs. "Other" pain outcomes between groups.

6. **Binary logistic regression**  
   - Estimation of odds ratios (ORs) for "No Pain" vs. "Pain," analyzing individual clinical conditions (e.g., diabetes, hypertension).

7. **Duration-based (tertile) analysis**  
   - Partitioning wound duration before Natrox and treatment duration into tertiles.  
   - Performing wound-size analyses (length, width, area) within each tertile group.

8. **Wound size reduction**  
   - Comparing initial vs. final wound measurements (length, width, area) across the entire dataset, by tertile groups, and by treatment continuity.  
   - Applying paired t-tests or Wilcoxon tests based on normality checks.

9. **Visualization**  
   - **Sankey plots** illustrating transitions from "pain_level_before_natrox" to "pain_level_after_natrox," both overall and by treatment continuity group.  
   - **Bar plots** with error bars for wound size (length, width, area) reductions by tertiles, with significance markers.

10. **Effect size calculations**  
    - **Cramér's V** for the contingency table of pain levels before vs. after.  
    - **Wilcoxon r** for wound size changes.

## How to use
1. **Download or clone this repository.**  
2. **Prepare a dataset** (`data.csv`) with the format and column names described in the script.  
3. **Open the script** (`main_analysis.R`) in RStudio (or another R environment).  
4. **Install the required R packages** (see above).  
5. **Run the script** to replicate the analyses and generate the output (statistical results, plots, etc.).

## Data availability
The data file (`data.csv`) used in this study is not publicly available but can be provided upon reasonable request. The script demonstrates the workflow for the analyses without requiring access to the raw data.

## License
This repository is licensed under CC0 1.0 Universal (Public Domain). The code is free to use, modify, and distribute without restrictions.
