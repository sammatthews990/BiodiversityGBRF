# run_analysis_example.R
#
# This standalone R script demonstrates how to use the BACI analysis function.
# It is designed for auditing, testing, and use by R programmers without needing to run the Shiny app.

# --- 1. Load necessary libraries ---
library(dplyr)
library(tibble)
library(tidyr)
library(rstanarm)
library(ggplot2)

# --- 2. Source the analysis functions ---
# Make sure this file is in the same directory
source("baci_analysis_functions.R")

# --- 3. Define the parameters for a single simulation run ---
# These are the same inputs that a user would select in the Shiny app
n_sites_ctrl <- 5
n_transects <- 5
n_years <- 10
intervention_year <- 3
true_uplift_pct <- 5 # 5% annual uplift
shock_type <- "No Shock"
shock_year <- 7
shock_magnitude_pct <- 50
survey_precision_sd <- 0.045 # Corresponds to "Benthic Photo Transects"
spatial_patchiness_sd <- 0.03
temporal_variation_sd <- 0.04

# --- 4. Run the analysis function ---
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
  spatial_patchiness_sd = spatial_patchiness_sd,
  temporal_variation_sd = temporal_variation_sd
)

cat("Simulation complete.\n\n")

# --- 5. Print the key results to the console ---
cat("--- KEY RESULTS ---\n")
cat("Mean Annual Uplift:", scales::percent(results$mean_uplift, accuracy = 0.01), "\n")
cat("Probability of Real Uplift:", scales::percent(results$prob_real_uplift, accuracy = 0.1), "\n")
cat("Final Credit Score:", round(results$credit_score * 100, 1), "\n")
cat("--------------------\n")

# --- 6. Generate and display the plot ---
plot_data <- results$plot_data

sim_plot <- ggplot(plot_data, aes(x = Year, y = Mean, color = Site_Type, fill = Site_Type)) +
  geom_vline(xintercept = intervention_year, linetype = "dashed", color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, linetype = 0) +
  geom_line(linewidth = 1.2) +
  annotate("text", x = intervention_year, y = 1, label = "Intervention", color = "blue", hjust = -0.1) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent, name = "Coral Cover") +
  scale_color_manual(values = c("Treatment" = "darkorange", "Control" = "gray40")) +
  scale_fill_manual(values = c("Treatment" = "darkorange", "Control" = "gray40")) +
  labs(
    title = "Simulated Monitoring Program",
    subtitle = paste("Design:", n_sites_ctrl, "Control Sites,", n_transects, "Transects/Site"),
    color = "Site Type", fill = "Site Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

if (shock_type != "No Shock") {
  sim_plot <- sim_plot + 
    geom_vline(xintercept = shock_year, linetype = "dashed", color = "red", linewidth = 1) +
    annotate("text", x = shock_year, y = 0.95, label = "Shock Event", color = "red", hjust = -0.1)
}

print(sim_plot)