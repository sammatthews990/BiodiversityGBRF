# --- 1. SETUP: Load necessary libraries ---
# install.packages(c("dplyr", "tidyr", "vegan", "ggplot2", "readr"))
library(readr)
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(broom)
library(stringr)

# --- 2. DATA LOADING ---
# Load your dataset. Ensure the path to your CSV file is correct.
fish_data <- readr::read_csv("monitoringdata/IMR Reef 1999-2024_fish_data_allfish density.csv")

source("baci_analysis_functions.R")
# --- 3. METRIC CALCULATION PER TRANSECT ---

# First, identify which columns contain species abundance data.
# We assume they start after the 'TRANSECT' column.
species_cols_start_index <- which(names(fish_data) == "TRANSECT_NO") + 1
species_cols_stop_index <- which(names(fish_data) == "Grand Total")
species_data <- fish_data[, species_cols_start_index:species_cols_stop_index]

# Calculate metrics for each row (transect).
transect_metrics <- fish_data %>%
  # Add the calculated metrics as new columns
  mutate(
    # Total Abundance: Sum of all individuals across all species in the transect.
    TotalAbundance = rowSums(species_data),
    
    # Species Richness (S): Count of species with abundance > 0.
    SpeciesRichness = rowSums(species_data > 0),
    
    # Shannon Diversity Index (H): Using the vegan package.
    ShannonDiversity = diversity(species_data, index = "shannon"),
    
    # Pielou's Evenness (J): H / log(S).
    # Handle the case where Richness is 0 or 1, where evenness is undefined.
    PielouEvenness = ifelse(SpeciesRichness <= 1, NA, ShannonDiversity / log(SpeciesRichness))
  ) %>%
  # Select only the grouping columns and the new metric columns.
  select(
    YEAR, REGION, ISLAND, EXPOSURE, LT_SITE,
    TotalAbundance, SpeciesRichness, ShannonDiversity,PielouEvenness
  )

cat("--- Metrics calculated for each transect ---\n")
print(head(transect_metrics))


# --- 4. AGGREGATE METRICS BY GROUPING LEVELS ---

# We can create a helper function to avoid repeating the same summary code.
calculate_summary_stats <- function(data, group_var) {
  # The {{group_var}} syntax allows us to pass column names to dplyr functions
  data %>%
    group_by(YEAR,REGION,{{ group_var }}) %>%
    summarise(
      # across() applies the same summary functions to multiple metric columns
      across(
        c(TotalAbundance, SpeciesRichness, PielouEvenness),
        list(
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          n = ~n()
        ),
        # Creates column names like 'TotalAbundance_mean', 'TotalAbundance_sd'
        .names = "{.col}_{.fn}"
      )
    ) %>%
    ungroup() # Always good practice to ungroup after summarising
}

# Calculate summaries for each level
summary_by_site <- calculate_summary_stats(transect_metrics, LT_SITE)
summary_by_exposure <- calculate_summary_stats(transect_metrics, EXPOSURE)
summary_by_island <- calculate_summary_stats(transect_metrics, ISLAND)
summary_by_region <- calculate_summary_stats(transect_metrics, REGION)

cat("\n--- Aggregated summary by SITE ---\n")
print(summary_by_site)
cat("\n--- Aggregated summary by EXPOSURE ---\n")
print(summary_by_exposure)


# --- 5. PLOTTING THE VARIANCE ---

# For plotting, it's best to keep the transect-level data in a "long" format.
transect_metrics_long <- transect_metrics %>%
  pivot_longer(
    cols = c(TotalAbundance, SpeciesRichness, PielouEvenness),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  # Make the Metric names factors for consistent plot ordering
  mutate(Metric = factor(Metric, levels = c("TotalAbundance", "SpeciesRichness", "PielouEvenness")))


# Helper function to create a boxplot for any grouping variable
# Corrected plotting function to avoid the deprecation warning
create_variance_plot <- function(long_data, group_col_name) {
  
  plot_title <- paste("Fish Community Metrics by", group_col_name)
  
  # --- THE FIX ---
  # Before: ggplot(long_data, aes_string(x = group_col_name, y = "Value", fill = group_col_name)) +
  # After: Use the .data pronoun inside the standard aes()
  ggplot(long_data, aes(x = .data[[group_col_name]], y = Value, fill = .data[[group_col_name]])) +
    geom_boxplot() +
    # Use facet_wrap to create a separate panel for each metric
    # 'scales = "free_y"' is crucial as metrics have different ranges
    facet_wrap(~Metric, scales = "free_y") +
    labs(
      title = plot_title,
      x = group_col_name,
      y = "Metric Value"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none", # The x-axis already shows the groups
      axis.text.x = element_text(angle = 45, hjust = 1) # Improve label readability
    )
}

# Generate a plot for each grouping level
plot_by_site <- create_variance_plot(transect_metrics_long, "LT_SITE")
plot_by_exposure <- create_variance_plot(transect_metrics_long, "EXPOSURE")
plot_by_island <- create_variance_plot(transect_metrics_long |> filter(REGION=="KEPPELS"), "ISLAND")
plot_by_region <- create_variance_plot(transect_metrics_long, "REGION")

# Print the plots
cat("\n--- Generating plots... ---\n")
print(plot_by_site)


# --- 5. PLOTTING THE VARIANCE AS A TIME SERIES ---

# We need a new plotting function to visualize trends and annual variance.
create_timeseries_plot <- function(summary_data, group_col_name) {
  
  # Reshape the data to make it easy to plot all three metrics at once
  summary_long <- summary_data %>%
    pivot_longer(
      cols = ends_with("_mean") | ends_with("_sd"),
      names_to = c("Metric", ".value"),
      names_pattern = "(.+)_(mean|sd)"
    ) %>%
    mutate(Metric = factor(Metric, levels = c("TotalAbundance", "SpeciesRichness", "PielouEvenness")))
  
  plot_title <- paste("Annual Fish Community Metrics by", group_col_name)
  
  ggplot(summary_long, aes(x = YEAR, y = mean, color = .data[[group_col_name]], fill = .data[[group_col_name]])) +
    # Add a ribbon for the standard deviation. This visualizes the variance each year.
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2, linetype = 0) +
    # Add a line for the mean trend
    geom_line(linewidth = 1) +
    geom_point(size=2) +
    # Create separate panels for each metric with their own y-axis
    facet_wrap(~Metric, scales = "free_y", ncol = 1) +
    labs(
      title = plot_title,
      subtitle = "Shaded area represents ±1 Standard Deviation of transects within that year",
      x = "Year",
      y = "Mean Metric Value",
      color = group_col_name,
      fill = group_col_name
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
}

# Generate the new time-series plots
plot_timeseries_by_site <- create_timeseries_plot(summary_by_site, "LT_SITE")
plot_timeseries_by_region <- create_timeseries_plot(summary_by_region, "REGION")
plot_timeseries_by_island <- create_timeseries_plot(summary_by_island |> filter(REGION=="KEPPELS"), "ISLAND")
# print(plot_by_exposure)
# print(plot_by_island)
# print(plot_by_region)# --- 1. Calculate the Pooled Spatial SD for each metric ---

# First, calculate the mean, sd, and variance for each site in each year
annual_site_stats <- transect_metrics %>%
  group_by(REGION, ISLAND, YEAR, LT_SITE) %>%
  summarise(
    across(
      c(TotalAbundance, SpeciesRichness, PielouEvenness),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        var = ~var(.x, na.rm = TRUE) # Calculate variance directly
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

# Now, pool these variances across all years to get a single robust estimate
pooled_spatial_variance <- annual_site_stats %>%
  # Group by the spatial unit of interest (LT_SITE)
  group_by(LT_SITE) %>%
  # Calculate the mean variance for each metric across all years
  summarise(
    across(
      ends_with("_var"),
      ~mean(.x, na.rm = TRUE),
      .names = "pooled_{.col}"
    )
  ) %>%
  ungroup()

# Finally, take the square root to get the Pooled Spatial SD
# We can also take the overall mean across all sites for a single system-wide value
system_wide_spatial_sd <- pooled_spatial_variance %>%
  summarise(
    across(
      starts_with("pooled_"),
      ~sqrt(mean(.x, na.rm = TRUE)), # Sqrt of the MEAN variance
      .names = "avg_spatial_sd_{stringr::str_remove(.col, '_var$')}"
    )
  )

cat("--- System-Wide Average Spatial SD (for 'sim_sd_spatial') ---\n")
print(system_wide_spatial_sd)

regional_spatial_sd <- annual_site_stats %>%
  # First, find the average variance for each SITE over time
  group_by(REGION,ISLAND, LT_SITE) %>%
  summarise(across(ends_with("_var"), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  # Now, find the average of these site-level variances for each REGION
  group_by(REGION, ISLAND) %>%
  summarise(
    across(
      ends_with("_var"),
      # Take the mean of the variances, then sqrt to get pooled SD
      ~sqrt(mean(.x, na.rm = TRUE)),
      .names = "spatial_sd_{str_remove(.col, '_var$')}"
    ),
    .groups = "drop"
  )
cat("--- Regional Average SPATIAL SD (for 'sim_sd_spatial') ---\n")
print(regional_spatial_sd)

# --- 2. Calculate the Residual Temporal SD for each metric ---

# We use the annual_site_stats which has the mean values per site/year
# We need to detrend the data for each site
temporal_residuals <- annual_site_stats %>%
  # We need to pivot to model each metric separately
  pivot_longer(
    cols = ends_with("_mean"),
    names_to = "Metric",
    values_to = "MeanValue",
    names_pattern = "(.+)_mean"
  ) %>%
  group_by(LT_SITE, Metric) %>%
  # The 'do' function allows us to run a custom function or model on each group
  do(broom::augment(lm(MeanValue ~ YEAR, data = .))) %>%
  ungroup()

# The '.resid' column now contains the detrended "wobble" for each year
# The SD of these residuals is our temporal noise estimate
residual_temporal_sd <- temporal_residuals %>%
  group_by(Metric) %>%
  summarise(
    residual_temporal_sd = sd(.resid, na.rm = TRUE)
  )

cat("\n--- System-Wide Residual Temporal SD (for 'sim_sd_temporal') ---\n")
print(residual_temporal_sd)

# --- 2. Calculate the Residual TEMPORAL SD, Stratified by REGION ---

# We first need the mean values per site/year, again grouped by region.
annual_site_means <- transect_metrics %>%
  group_by(REGION, YEAR, LT_SITE) %>%
  summarise(
    across(
      c(TotalAbundance, SpeciesRichness, PielouEvenness),
      ~mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    .groups = "drop"
  )

# Now, detrend the data for each site (which are nested within regions)
temporal_residuals_regional <- annual_site_means %>%
  pivot_longer(
    cols = ends_with("_mean"),
    names_to = "Metric",
    values_to = "MeanValue",
    names_pattern = "(.+)_mean"
  ) %>%
  group_by(REGION, LT_SITE, Metric) %>% # Group by REGION here
  # Run the linear model on each site's time-series
  do(broom::augment(lm(MeanValue ~ YEAR, data = .))) %>%
  ungroup()

# Finally, calculate the SD of the residuals for each REGION and METRIC
regional_residual_temporal_sd <- temporal_residuals_regional %>%
  group_by(REGION, Metric) %>% # Group by REGION before the final summary
  summarise(
    residual_temporal_sd = sd(.resid, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- Regional Residual TEMPORAL SD (for 'sim_sd_temporal') ---\n")
print(regional_residual_temporal_sd)


# --- 1. PLOT 1: Compare Variance Components ---

# First, prepare the data by combining both variance tables into one tidy format.
spatial_long <- regional_spatial_sd %>%
  pivot_longer(
    cols = -c(REGION,ISLAND),
    names_to = "Metric",
    values_to = "SD",
    names_prefix = "spatial_sd_"
  ) %>%
  mutate(VarianceType = "Spatial (within-year)")

temporal_long <- regional_residual_temporal_sd %>%
  rename(SD = residual_temporal_sd) %>%
  mutate(VarianceType = "Temporal (year-to-year)")

combined_variance_data <- bind_rows(spatial_long, temporal_long) |>
  filter(!REGION %in% c("FAMILYS", "TURTLES", "CUMBERLANDS", "FRANKLANDS"))

# Create the plot
variance_comparison_plot <- ggplot(combined_variance_data, aes(x = REGION, y = SD, fill = VarianceType)) +
  geom_col(position = "dodge") +
  # Create a separate panel for each metric, with its own y-axis scale
  facet_wrap(~Metric, scales = "free_y") +
  labs(
    title = "Comparison of Spatial vs. Temporal Variability by Region",
    subtitle = "Spatial SD reflects transect patchiness; Temporal SD reflects year-to-year noise",
    x = "Region",
    y = "Standard Deviation",
    fill = "Type of Variance"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

cat("--- Generating Plot 1: Variance Comparison ---\n")
print(variance_comparison_plot)


# --- 2. PLOT 2: Compare Power Curves Across Regions and Metrics ---

# --- A. Set up standard parameters for the power analysis ---
power_params <- list(
  target_uplift_pct = 3,    # A constant 3% uplift to detect
  monitoring_years = 5,
  monitoring_frequency = "Annual",
  survey_precision_sd = 0, # IMPORTANT: Set to 0 to isolate the effect of environmental variance
  n_ctrl_values = 5,       # A constant 5 control sites
  n_transect_values = 1:15 # Plot power across 1 to 15 transects
)

# --- B. Create a master table of parameters for each scenario ---
# Join the spatial and temporal SD tables together
power_scenarios <- regional_spatial_sd %>%
  filter(!REGION %in% c("FAMILYS", "TURTLES", "CUMBERLANDS", "FRANKLANDS")) |> ungroup() |>
  pivot_longer(cols = -c(REGION,ISLAND), names_to = "Metric", values_to = "spatial_sd", names_prefix = "spatial_sd_") %>%
  left_join(
    regional_residual_temporal_sd %>% rename(temporal_sd = residual_temporal_sd),
    by = c("REGION", "Metric")
  )

# --- C. Run the power analysis for every row in the scenarios table ---
power_results <- power_scenarios %>%
  mutate(
    analysis = pmap(
      .l = list(peak_spatial_sd = spatial_sd, temporal_sd = temporal_sd),
      .f = ~ run_power_analysis(
        target_uplift_pct = power_params$target_uplift_pct,
        monitoring_years = power_params$monitoring_years,
        monitoring_frequency = power_params$monitoring_frequency,
        survey_precision_sd = power_params$survey_precision_sd,
        peak_spatial_sd = ..1,
        temporal_sd = ..2,
        baseline_cover_pct = 30, # Dummy value, not used in calculations
        n_ctrl_values = power_params$n_ctrl_values,
        n_transect_values = power_params$n_transect_values
      )
    )
  ) %>%
  unnest(analysis)


# --- D. Create the power curve plot ---
power_curve_plot <- ggplot(power_results, aes(x = N_Transects, y = Power_Mean, color = Metric, fill = Metric)) +
  geom_ribbon(aes(ymin = Power_Lower, ymax = Power_Upper), alpha = 0.2, linetype = 0) +
  geom_line(linewidth = 1.1) +
  # Add the 80% power threshold line for reference
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  # Create a separate panel for each region
  facet_wrap(~REGION) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Power to Detect a 3% Annual Uplift",
    subtitle = paste("Assuming a 5-year study with 5 control sites. Survey error is excluded to isolate environmental variance."),
    x = "Number of Transects per Site",
    y = "Statistical Power"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

cat("\n--- Generating Plot 2: Power Curve Comparison ---\n")
print(power_curve_plot)

