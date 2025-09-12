# run_analysis_example.R

library(dplyr)
library(tibble)
library(tidyr)
library(rstanarm)
library(ggplot2)

source("baci_analysis_functions.R")

# --- 1. Define simulation parameters ---
n_sites_ctrl <- 5
n_transects <- 5
n_years <- 10
intervention_year <- 3
true_uplift_pct <- 5
shock_type <- "No Shock"
shock_year <- 7
shock_magnitude_pct <- 50
survey_precision_sd <- 0.045 # Corresponds to "Benthic Photo Transects"

# --- 2. Run the analysis function ---
cat("Running BACI simulation... this may take a moment.\n")

results <- run_baci_analysis(
  n_sites_ctrl = n_sites_ctrl,
  n_transects = n_transects,
  n_years = n_years,
  intervention_year = intervention_year,
  true_uplift_pct = true_uplift_pct,
  shock_type = shock_type,
  shock_year = shock_year,
  shock_magnitude_pct = shock_magnitude_pct,
  survey_precision_sd = survey_precision_sd,
  spatial_patchiness_sd = 0.03,
  temporal_variation_sd = 0.04
)

cat("Simulation complete.\n\n")

# --- 3. Print the key results ---
cat("--- COMPOSITE RESULTS ---\n")
cat("Mean Annual Uplift:", scales::percent(results$composite_uplift, accuracy = 0.01), "\n")
cat("Probability of Real Uplift:", scales::percent(results$composite_prob, accuracy = 0.1), "\n")
cat("Final Credit Score:", round(results$composite_credit * 100, 1), "\n")
cat("-------------------------\n\n")

cat("--- DETAILED RESULTS BY METRIC ---\n")
print(results$results_table)
cat("---------------------------------\n")

# --- 4. Generate and display the plot ---
plot_data <- results$plot_data
# (Plotting code is the same as in the app, omitted here for brevity)