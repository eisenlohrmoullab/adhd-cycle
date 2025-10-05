# ADHD Cycle Hormone Analysis Documentation

## Overview

This documentation describes the two new R Markdown analysis files created for testing the effects of 5-day rolling averages of estradiol (E2) and progesterone (P4) on daily ADHD and related outcomes.

## New Analysis Files

### 1. ADHDCYCLE_7_GAMM_Hormones.Rmd

**Purpose**: Generalized Additive Mixed Models (GAMMs) analysis

**Key Features**:
- Uses `gamm4` package for mixed-effects GAMs
- Models outcome ~ s(E2.5roll.d) + s(P4.5roll.d) + ti(E2.5roll.d, P4.5roll.d)
- Includes smooth functions for E2 and P4 effects
- Tensor product interaction term for E2×P4 interaction
- Random intercept by participant: (1|id)

**Outputs**:
- Model summaries saved as text files (one per outcome)
- CSV table with all model results
- HTML formatted results table
- Marginal effects plots for each outcome (E2 and P4 effects)
- Interaction surface plots showing E2×P4 interaction

### 2. ADHDCYCLE_8_MLM_Hormones.Rmd

**Purpose**: Multilevel Linear Models (MLM) analysis

**Key Features**:
- Uses `lme4` and `lmerTest` packages
- Models outcome ~ E2.5roll.d + P4.5roll.d + E2.5roll.d:P4.5roll.d + (1|id)
- Linear effects for E2 and P4
- Linear interaction term for E2×P4
- Random intercept by participant: (1|id)

**Outputs**:
- Model summaries saved as text files (one per outcome)
- CSV table with all model results (coefficients, SEs, t-values, p-values)
- HTML formatted results table
- Forest plot of coefficients across all outcomes
- Predicted effects plots for each outcome (E2 and P4 effects)
- Interaction plots showing E2 effect at different P4 levels

## Predictor Variables

Both analyses use the same predictors:

1. **E2.5roll.d**: 5-day rolling average of estradiol, person-centered
2. **P4.5roll.d**: 5-day rolling average of progesterone, person-centered
3. **E2.5roll.d:P4.5roll.d**: Interaction term

## Outcome Variables

Both analyses test effects on the following daily outcomes:

### ADHD Symptoms
- CSS_Inatt: CSS Inattention Severity
- CSS_HypImp: CSS Hyperactivity/Impulsivity Severity
- CSS_Inatt_Count: CSS Inattention Count
- CSS_Hyp_Count: CSS Hyperactivity Count
- CSS_Imp_Count: CSS Impulsivity Count

### Executive Function
- score_pinball: Working Memory (Pinball task)
- score_robot: Response Inhibition (Robot Factory task)
- BDEFS_WM_avg: Working Memory Symptoms (BDEFS)
- BDEFS_RI_avg: Response Inhibition Symptoms (BDEFS)

### Mood & Premenstrual Symptoms (DRSP)
- DRSP_1 through DRSP_23 (various premenstrual and mood symptoms)

## Data Requirements

Both scripts expect:
- **Data file**: `data/adhd_daily_scaled_20250929.rdata`
- **Data object**: `cycle_df_scaled` (loaded from the .rdata file)
- **Required variables**: 
  - Outcome variables (see list above)
  - E2.5roll.d
  - P4.5roll.d
  - id (participant identifier)

## Running the Analyses

### Prerequisites

Install required R packages:

```r
# For GAMM analysis
install.packages(c("tidyverse", "mgcv", "gamm4", "marginaleffects", 
                   "broom", "knitr", "kableExtra", "ggplot2", 
                   "patchwork", "glue"))

# For MLM analysis
install.packages(c("tidyverse", "lme4", "lmerTest", "broom.mixed", 
                   "performance", "emmeans", "knitr", "kableExtra", 
                   "ggplot2", "patchwork", "glue", "sjPlot"))
```

### Execution

1. **Ensure data file exists**: Place the cleaned dataset at `data/adhd_daily_scaled_20250929.rdata`

2. **Run GAMM analysis**:
   ```r
   rmarkdown::render("ADHDCYCLE_7_GAMM_Hormones.Rmd")
   ```

3. **Run MLM analysis**:
   ```r
   rmarkdown::render("ADHDCYCLE_8_MLM_Hormones.Rmd")
   ```

## Output Structure

All outputs are saved with date prefix (e.g., `20251005_*`) to the following folders:

### GAMM Outputs
```
output/20251005_GAMM_Hormones/
├── model_summaries/
│   ├── 20251005_CSS_Inatt_GAMM_summary.txt
│   ├── 20251005_CSS_HypImp_GAMM_summary.txt
│   └── ... (one per outcome)
├── marginal_effects_plots/
│   ├── 20251005_CSS_Inatt_marginal_effects.png
│   ├── 20251005_CSS_HypImp_marginal_effects.png
│   └── ... (one per outcome)
├── interaction_plots/
│   ├── 20251005_CSS_Inatt_interaction.png
│   ├── 20251005_CSS_HypImp_interaction.png
│   └── ... (one per outcome)
├── 20251005_GAMM_results_table.csv
└── 20251005_GAMM_results_table.html
```

### MLM Outputs
```
output/20251005_MLM_Hormones/
├── model_summaries/
│   ├── 20251005_CSS_Inatt_MLM_summary.txt
│   ├── 20251005_CSS_HypImp_MLM_summary.txt
│   └── ... (one per outcome)
├── predicted_effects_plots/
│   ├── 20251005_CSS_Inatt_predicted_effects.png
│   ├── 20251005_CSS_HypImp_predicted_effects.png
│   └── ... (one per outcome)
├── interaction_plots/
│   ├── 20251005_CSS_Inatt_interaction.png
│   ├── 20251005_CSS_HypImp_interaction.png
│   └── ... (one per outcome)
├── 20251005_MLM_results_table.csv
├── 20251005_MLM_results_table.html
└── 20251005_MLM_coefficient_plot.png
```

## Model Specifications

### GAMM Model
```r
outcome ~ s(E2.5roll.d, bs='tp', k=5) + 
          s(P4.5roll.d, bs='tp', k=5) + 
          ti(E2.5roll.d, P4.5roll.d, bs='tp', k=5)
Random: ~ (1|id)
Method: REML
```

### MLM Model
```r
outcome ~ E2.5roll.d + P4.5roll.d + E2.5roll.d:P4.5roll.d + (1|id)
Method: REML
Optimizer: bobyqa
```

## Interpretation Notes

### GAMM Results
- **edf (Effective degrees of freedom)**: Higher values indicate more complex non-linear relationships
- **p-values**: Test whether the smooth term is significantly different from zero
- **R²**: Proportion of variance explained by the model
- **Deviance Explained**: Percentage of deviance explained

### MLM Results
- **Fixed Effects Coefficients**: Linear effect sizes for E2, P4, and their interaction
- **R² Marginal**: Variance explained by fixed effects only
- **R² Conditional**: Variance explained by fixed + random effects
- **ICC**: Intraclass correlation - proportion of variance due to between-person differences

## Differences Between GAMM and MLM

| Feature | GAMM | MLM |
|---------|------|-----|
| E2 & P4 Effects | Non-linear (smooth functions) | Linear |
| Interaction | Non-parametric surface | Parametric product term |
| Interpretation | More flexible, captures non-linearity | Simpler, easier interpretation |
| Best for | Exploratory analysis, complex patterns | Confirmatory analysis, clear hypotheses |

## Publication-Ready Output

Both analyses produce:
1. **Tables**: HTML formatted tables suitable for manuscripts
2. **Figures**: High-resolution (300 dpi) PNG files
3. **Text summaries**: Complete model output for supplementary materials

## Date Prefix

All output files use the format `YYYYMMDD_*` where the date corresponds to when the analysis was run. This ensures:
- Clear version control
- No overwriting of previous analyses
- Easy tracking of when analyses were performed

Current date prefix: **20251005**

## Troubleshooting

### Common Issues

1. **Data file not found**: 
   - Ensure `data/adhd_daily_scaled_20250929.rdata` exists
   - Update the `data_file` path in the scripts if needed

2. **Missing variables**:
   - Check that E2.5roll.d and P4.5roll.d exist in the dataset
   - Verify outcome variables match the dataset column names

3. **Package not installed**:
   - Run the installation commands in the Prerequisites section
   - Use `install.packages()` for any missing packages

4. **Model convergence issues**:
   - Some outcomes may have convergence warnings
   - Check model summaries for problematic models
   - Consider simplifying the model or checking data quality

## Contact

For questions about these analyses, contact the repository owner.

## Date Created

October 5, 2025
