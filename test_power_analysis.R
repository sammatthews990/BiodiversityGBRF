# Load the functions and parameters from your script
source("baci_analysis_functions.R")
library(ggplot2) # for plotting

# --- 1. Define a specific scenario to test ---
# Let's find the precision SD for "Benthic Photo Transects"
benthic_photo_sd <- survey_methods_params %>%
  filter(Method == "Benthic Photo Transects") %>%
  pull(SD_Precision)

# --- 2. Run the power analysis function ---
cat("Running power analysis for a single scenario...\n")
analysis_results <- run_power_analysis(
  target_uplift_pct   = 2.5,                # A 2.5% annual uplift
  monitoring_years    = 5,
  monitoring_frequency= "Annual",
  survey_precision_sd = benthic_photo_sd,
  peak_spatial_sd     = 0.05,
  temporal_sd         = 0.02,
  baseline_cover_pct  = c(20, 40),          # Test for 20% and 40% baseline cover
  n_ctrl_values       = c(3, 6)             # Test for 3 and 6 control sites
)

cat("Analysis complete. Showing first few rows of results:\n")
print(head(analysis_results))

# --- 3. Visualize the results (similar to the app) ---
cat("\nGenerating a plot of the results...\n")

plot <- analysis_results %>%
  mutate(
    Control_Sites = factor(paste(N_Controls, "Control Sites")),
    Baseline_Cover = factor(paste0(Baseline_Cover * 100, "%"))
  ) %>%
  ggplot(aes(x = N_Transects, y = Power_Mean, color = Baseline_Cover, fill = Baseline_Cover)) +
  geom_ribbon(aes(ymin = Power_Lower, ymax = Power_Upper), alpha = 0.2, linetype = 0) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  facet_wrap(~Control_Sites) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "Power to Detect a 2.5% Annual Uplift",
    x = "Number of Transects per Site",
    y = "Statistical Power",
    color = "Baseline Cover",
    fill = "Baseline Cover"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

print(plot)
# To save the plot:
# ggsave("power_analysis_standalone_plot.png", plot, width = 10, height = 6)