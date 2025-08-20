# Function to estimate ICC with user-specified dataset, outcome variable, and clustering variable
library(lme4)

estimate_icc <- function(data, outcome_var, cluster_var) {
  # Fit a simple mixed-effects model to estimate ICC
  formula <- as.formula(paste(outcome_var, "~ 1 + (1 |", cluster_var, ")"))
  model <- lmer(formula, data = data)
  
  # Extract variance components
  var_between <- as.numeric(VarCorr(model)[[cluster_var]][1])  # Between-cluster variance
  var_within <- as.numeric(attr(VarCorr(model), "sc")^2)  # Within-cluster variance
  
  # Calculate ICC
  icc <- var_between / (var_between + var_within)
  
  # Print ICC
  cat("The ICC for", outcome_var, "clustering within", cluster_var, "is:", round(icc, 2), "\n")
}