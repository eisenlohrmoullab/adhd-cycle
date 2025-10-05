# Quick Start Guide: Hormone Analysis

## Prerequisites

### 1. Data File
Ensure you have the cleaned dataset at:
```
data/adhd_daily_scaled_20250929.rdata
```

### 2. Required R Packages
Run this in R to install all needed packages:

```r
# Install CRAN packages
install.packages(c(
  # Core packages
  "tidyverse", "knitr", "kableExtra", "ggplot2", 
  "patchwork", "glue",
  
  # For GAMM analysis
  "mgcv", "gamm4", "marginaleffects", "broom",
  
  # For MLM analysis  
  "lme4", "lmerTest", "broom.mixed", "performance", 
  "emmeans", "sjPlot"
))
```

## Quick Validation

Before running the analyses, validate your data:

```r
source("scripts/validate_hormone_analysis_data.R")
```

This will check:
- ✓ Data file exists
- ✓ Required variables present
- ✓ Data completeness
- ✓ Summary statistics

## Running the Analyses

### Option 1: In RStudio

1. Open `ADHDCYCLE_7_GAMM_Hormones.Rmd` or `ADHDCYCLE_8_MLM_Hormones.Rmd`
2. Click "Knit" button
3. Wait for analysis to complete
4. View HTML output

### Option 2: From R Console

```r
# GAMM analysis
rmarkdown::render("ADHDCYCLE_7_GAMM_Hormones.Rmd")

# MLM analysis
rmarkdown::render("ADHDCYCLE_8_MLM_Hormones.Rmd")
```

### Option 3: From Command Line

```bash
# GAMM analysis
Rscript -e "rmarkdown::render('ADHDCYCLE_7_GAMM_Hormones.Rmd')"

# MLM analysis
Rscript -e "rmarkdown::render('ADHDCYCLE_8_MLM_Hormones.Rmd')"
```

## What Gets Created

### GAMM Analysis Output
```
output/20251005_GAMM_Hormones/
├── model_summaries/           # Text files with full model output
├── marginal_effects_plots/    # E2 and P4 effect plots
├── interaction_plots/         # E2×P4 interaction surfaces
├── 20251005_GAMM_results_table.csv   # Spreadsheet format
└── 20251005_GAMM_results_table.html  # Publication-ready table
```

### MLM Analysis Output
```
output/20251005_MLM_Hormones/
├── model_summaries/           # Text files with full model output
├── predicted_effects_plots/   # E2 and P4 effect plots
├── interaction_plots/         # E2 effect at different P4 levels
├── 20251005_MLM_results_table.csv    # Spreadsheet format
├── 20251005_MLM_results_table.html   # Publication-ready table
└── 20251005_MLM_coefficient_plot.png # Forest plot of all effects
```

## Outputs Are Publication-Ready

All outputs are formatted for publication:
- **Tables**: HTML formatted with proper styling
- **Plots**: 300 DPI PNG files with professional formatting
- **Text**: Complete model summaries for supplementary materials

## Date Prefix

All outputs use current date: `20251005_*`

This ensures:
- No overwriting previous analyses
- Clear version tracking
- Easy organization

## Expected Runtime

- GAMM analysis: ~10-30 minutes (depending on data size)
- MLM analysis: ~5-15 minutes (faster than GAMM)

## Troubleshooting

### "Data file not found"
- Check: `data/adhd_daily_scaled_20250929.rdata` exists
- Or update `data_file` path in the .Rmd files

### "Package not found"
- Run the installation commands above
- Restart R session

### "Missing variables"
- Run validation script: `source("scripts/validate_hormone_analysis_data.R")`
- Check which variables are missing

### Model convergence warnings
- Some outcomes may produce warnings
- Check model summaries for details
- Often models are still usable

## Key Differences: GAMM vs MLM

| Feature | GAMM | MLM |
|---------|------|-----|
| **Speed** | Slower | Faster |
| **Flexibility** | Non-linear effects | Linear effects only |
| **Interpretation** | More complex | Simpler |
| **Best for** | Exploratory | Confirmatory |
| **When to use** | Unknown relationships | Testing hypotheses |

### Recommendation
Run **both analyses** to:
1. Use GAMM to explore non-linear patterns
2. Use MLM for clear, interpretable effect sizes
3. Compare results between approaches

## Need More Details?

See `ANALYSIS_DOCUMENTATION.md` for:
- Complete technical specifications
- Detailed output descriptions
- Interpretation guidelines
- Troubleshooting details

## Questions?

Contact repository owner: Tory Eisenlohr-Moul, PhD

---

**Created**: October 5, 2025  
**Updated**: October 5, 2025
