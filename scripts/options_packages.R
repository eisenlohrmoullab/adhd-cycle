# Script to set up packages and options 
# ---- Set Options ----

options(
  scipen = 999,   # Suppress scientific notation
  digits = 3      # Set display precision for numerical output
)

# ---- Package Loading ----

# Utilities and Core Tidyverse Packages
library(beepr)          # Sound notifications
library(tidyverse)      # Core data manipulation, cleaning, and visualization tools
library(janitor)        # Data cleaning (e.g., column renaming, removing empty rows)
library(skimr)          # Quick summaries of data
library(forcats)        # Tools for handling categorical (factor) data
library(fuzzyjoin)      # Joining data frames with fuzzy matching
library(stringdist)     # String distance metrics
library(stringr)        # String manipulation
library(MatchIt)        # Propensity score matching
library(broom)          # Tidying model outputs
library(RecordLinkage)  # Record linkage and deduplication
library(dlookr)         # Data exploration and quality assurance
library(powerjoin)      # Joining data frames with power
library(data.table)     # Fast data manipulation

# Data Import and File Handling
library(haven)          # Importing SPSS, Stata, and SAS files
library(readxl)         # Importing Excel files
library(readr)          # Importing text files
# library(readODS)      # For importing OpenDocument Spreadsheets (ODS)

# Data Visualization Enhancements
library(ggdist)         # Distribution and uncertainty visualization
library(ggforce)        # Additional ggplot2 functionality
library(ggrepel)        # Non-overlapping text labels in ggplot2
library(sjPlot)         # Plotting statistical models (e.g., mixed models)
library(corrplot)       # Correlation matrix visualizations
library(gridExtra)      # Arranging multiple plots in a grid
library(see)            # Model diagnostics and visualization
library(DescTools)      # Descriptive statistics and plotting tools
library(ggExtra)        # Additional features for ggplot2 (e.g., marginal plots)
library(patchwork)      # Arrange multiple ggplots
library(plotly)         # Interactive plotting
library(ggstatsplot)    # Visualizing statistical tests
library(insight)        # Visualizing model diagnostics
library(ggvenn)         # Venn diagrams with ggplot2


# Data Exploration and Missing Data Visualization
library(visdat)         # Visualizing data structure and missing data

# Time Series and Date Handling
library(zoo)            # Rolling averages, time series analysis
library(lubridate)      # Date and time manipulation
# library(timetk)       # Advanced time series manipulation
# library(tibbletime)   # Time-aware tibbles (tidy time series)
# library(tsibble)      # Tidy temporal data frames for time series

# Mixed-Effects Models and Generalized Additive Models (GAMs)
library(lme4)           # Mixed-effects models (linear and generalized)
library(lmerTest)       # Adds p-values for lme4 models
library(nlme)           # Nonlinear mixed-effects models
library(emmeans)        # Estimated marginal means (EMMs) for model outputs
library(broom.mixed)    # Tidying mixed model results
library(performance)    # Model performance metrics (e.g., R-squared for mixed models)
library(pbkrtest)       # Bootstrap and Kenward-Roger methods for mixed models
library(mgcv)           # Generalized additive models (GAMs)
# library(gamm4)        # Mixed-effects generalized additive models (GAMMs)
library(marginaleffects) # Tools for calculating marginal effects from regression models

# Psychometrics and Statistical Tools
library(psych)          # Descriptive statistics and psychometrics
library(rmcorr)         # Repeated measures correlations
library(careless)       # Detect careless responses in survey data
library(responsePatterns) # Response pattern analysis

# Project Management and GitHub Integration
library(usethis)        # GitHub and project setup
library(gitcreds)       # Git credential management

# Conflict Management Between Packages
library(conflicted)     # Manage conflicts between package namespaces
# library(renv)         # Managing reproducible environments

# ---- Conflict Management ----

# Check for conflicts and set preferred functions 
conflict_scout()
conflicts_prefer(
  dplyr::filter, 
  tidyr::complete, 
  tidyr::expand, 
  dplyr::lag, 
  lme4::lmer, 
  psych::alpha, 
  dplyr::select, 
  psych::ICC,
  dplyr::first
)

# Print a message describing what has been done with this code
cat("Packages and options have been set up, and preferences have been delared with the conflicted package.\n")