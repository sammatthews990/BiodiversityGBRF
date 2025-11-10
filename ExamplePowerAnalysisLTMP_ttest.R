# ---
# SCRIPT FOR A SIMPLIFIED BEFORE-AFTER POWER ANALYSIS FOR SCALED METRICS
# ---
# This script uses the simple t-test power model to generate a faceted plot
# showing the power to detect a consistent change across multiple metrics
# that have been scaled to a 0-1 range.
# ---

# --- 0. SETUP: Load necessary libraries ---
# install.packages(c("dplyr", "tibble", "purrr", "ggplot2"))
library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)


# --- 1. CORE POWER FUNCTION ---
# This function is unchanged. It's a generic engine that works on any scale,
# as long as 'delta' and 'sd_within_year' are on the SAME scale.

calculate_simple_t_test_power <- function(delta, sd_within_year, n_transects) {
  se_difference <- sqrt(2 * (sd_within_year^2 / n_transects))
  df <- (n_transects - 1) * 2
  ncp <- delta / se_difference
  power <- pt(qt(0.975, df = df), df = df, ncp = ncp, lower.tail = FALSE)
  return(power)
}


# --- 2. INPUT DATA: NAIVE SD ESTIMATES ---

## --------------------------------------------- ##


# --- 3. RUN POWER ANALYSIS FOR ALL METRICS ---

cat("--- Running Power Analysis for All Scaled Metrics ---\n")

# Define the target effect size on the 0-1 scale.
# A delta of 0.05 represents an absolute 5% change of the total possible range.
target_delta_scaled <- 0.05

# Define the range of transect numbers to test
transect_options <- 2:50

# Use pmap_dfr to iterate over each row of the sds_naive_sum dataframe
# This will run the power analysis for each metric and stack the results.
power_curves_all_metrics <- pmap_dfr(sds_naive_sum, function(Metric, Mean_Naive_SD, Sector,...){
  
  # For each metric, calculate the power curve across all transect options
  power_curve <- tibble(
    N_Transects = transect_options,
    Power = map_dbl(transect_options, ~ calculate_simple_t_test_power(
      delta = target_delta_scaled,
      sd_within_year = Mean_Naive_SD,
      n_transects = .x
    ))
  )
  
  # Add the Metric name back to the results for plotting
  power_curve %>% mutate(Metric = Metric, Sector = Sector)
})

cat("--- Power analysis complete ---\n")
print(head(power_curves_all_metrics))


# --- 4. VISUALIZE THE RESULTS WITH A FACETED PLOT ---

cat("--- Generating faceted power plot ---\n")

# Now, create the plot
faceted_power_plot_by_sector <- ggplot(power_curves_all_metrics, 
                                       aes(x = N_Transects, y = Power, group = Sector, color = Sector)) +
  # Draw a thin line for each sector
  geom_line(alpha = 0.5, size = 1) + 
  
  # Optionally, add a single, bold "average" line on top
  stat_smooth(aes(group = 1), method = "gam", se = TRUE, color = "black", fill="grey50") +
  
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  facet_wrap(~ Metric, ncol = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = paste("Power to Detect a", target_delta_scaled, "Change on the 0-1 Scale"),
    subtitle = "Simplified Before-After (t-test) scenario, using naive SD estimates.",
    x = "Number of Transects per Site",
    y = "Statistical Power"
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"), legend.position = "right")

print(faceted_power_plot_by_sector)

# --- Compare to formal SD estimates ---
# Use pmap_dfr to iterate over each row of the sds_naive_sum dataframe
# This will run the power analysis for each metric and stack the results.
power_curves_all_metrics <- pmap_dfr(Sectoral_spatial_sd, function(Metric, Spatial_SD, Sector,...){
  
  # For each metric, calculate the power curve across all transect options
  power_curve <- tibble(
    N_Transects = transect_options,
    Power = map_dbl(transect_options, ~ calculate_simple_t_test_power(
      delta = target_delta_scaled,
      sd_within_year = Spatial_SD,
      n_transects = .x
    ))
  )
  
  # Add the Metric name back to the results for plotting
  power_curve %>% mutate(Metric = Metric, Sector = Sector)
})

cat("--- Power analysis complete ---\n")
print(head(power_curves_all_metrics))


# --- 4. VISUALIZE THE RESULTS WITH A FACETED PLOT ---

cat("--- Generating faceted power plot ---\n")

# Now, create the plot
faceted_power_plot_by_sector <- ggplot(power_curves_all_metrics, 
                                       aes(x = N_Transects, y = Power, group = Sector, color = Sector)) +
  # Draw a thin line for each sector
  geom_line(alpha = 0.5, size = 1) + 
  
  # Optionally, add a single, bold "average" line on top
  stat_smooth(aes(group = 1), method = "gam", se = TRUE, color = "black", fill="grey50") +
  
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  facet_wrap(~ Metric, ncol = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = paste("Power to Detect a", target_delta_scaled, "Change on the 0-1 Scale"),
    subtitle = "Simplified Before-After (t-test) scenario, using LTMP SD estimates.",
    x = "Number of Transects per Site",
    y = "Statistical Power"
  ) +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold"), legend.position = "right")

print(faceted_power_plot_by_sector)

# --- 5. INTERPRETATION SUMMARY ---
# Find the number of transects needed for 80% power for each metric
transects_for_80_power <- power_curves_all_metrics %>%
  group_by(Metric) %>%
  filter(Power >= 0.80) %>%
  slice(1) %>%
  select(Metric, N_Transects)

cat("\n--- Approximate Number of Transects Needed for 80% Power ---\n")
print(transects_for_80_power)