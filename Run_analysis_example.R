# run_analysis_example.R

library(dplyr)
library(tibble)
library(tidyr)
library(rstanarm)
library(INLA)
library(ggplot2)

source("baci_analysis_functions.R")

# --- 1. Define simulation parameters ---
analysis_method_to_test <- "Fast Approximation (INLA)"
n_sites_ctrl <- 5
n_transects <- 5
n_years <- 10
intervention_year <- 3
true_uplift_pct <- 5
shock_type <- "Localized Impact (COTS)"
shock_year <- 7
shock_magnitude_pct <- 80
survey_precision_sd <- 0.045
spatial_patchiness_sd <- 0.03
temporal_variation_sd <- 0.04

# --- 2. Run the analysis function ---
cat("Running BACI simulation using:", analysis_method_to_test, "\nThis may take a moment...\n")

results <- run_baci_analysis(
  analysis_method = analysis_method_to_test,
  n_sites_ctrl = n_sites_ctrl,
  n_transects = n_transects,
  n_years = n_years,
  intervention_year = intervention_year,
  true_uplift_pct = true_uplift_pct,
  shock_type = shock_type,
  shock_year = shock_year,
  shock_magnitude_pct = shock_magnitude_pct,
  survey_precision_sd = survey_precision_sd,
  spatial_patchiness_sd = spatial_patchiness_sd,
  temporal_variation_sd = temporal_variation_sd
)

cat("Simulation complete.\n\n")

# --- 3. Print the key results ---
cat("--- COMPOSITE RESULTS ---\n")
print(results$results_table %>% filter(Metric == "Composite Index"))
cat("-------------------------\n\n")

cat("--- DETAILED RESULTS BY METRIC ---\n")
print(results$results_table)
cat("---------------------------------\n\n")

cat("--- FIRST 10 ROWS OF RAW SIMULATED DATA ---\n")
print(head(results$raw_data, 10))
cat("-------------------------------------------\n")```