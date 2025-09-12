# baci_analysis_functions.R
#
# This script contains the core statistical engine for the BACI Power Simulator.
# It uses a Bayesian hierarchical model (stan_lmer) to estimate uplift and uncertainty.

library(dplyr)
library(tibble)
library(tidyr)
library(rstanarm)

run_baci_analysis <- function(
    n_sites_ctrl, n_transects, n_years, intervention_year, true_uplift_pct,
    shock_type, shock_year, shock_magnitude_pct, survey_precision_sd,
    spatial_patchiness_sd, temporal_variation_sd
) {
  
  # --- 1. Setup Simulation Parameters ---
  uplift_rate <- true_uplift_pct / 100
  shock_loss <- shock_magnitude_pct / 100
  n_sites_total <- 1 + n_sites_ctrl
  site_ids <- paste("Site", 1:n_sites_total)
  site_types <- c("Treatment", rep("Control", n_sites_ctrl))
  
  # --- 2. Generate "True" Underlying Coral Cover ---
  true_cover_df <- tibble(Year = 0:n_years) %>%
    crossing(Site_ID = site_ids) %>%
    left_join(tibble(Site_ID = site_ids, Site_Type = site_types), by = "Site_ID") %>%
    group_by(Site_ID) %>%
    mutate(start_cover = rnorm(1, mean = 0.3, sd = spatial_patchiness_sd)) %>%
    ungroup() %>%
    mutate(temporal_noise = rnorm(n(), 0, sd = temporal_variation_sd)) %>%
    group_by(Site_ID) %>%
    mutate(
      uplift_effect = if_else(Site_Type == "Treatment" & Year >= intervention_year, uplift_rate, 0),
      True_Cover = start_cover + (0.01 * Year) + cumsum(temporal_noise + uplift_effect)
    ) %>%
    ungroup() %>%
    mutate(True_Cover = pmin(0.95, pmax(0.01, True_Cover)))
  
  # --- 3. Apply Exogenous Shock Event ---
  if (shock_type != "No Shock") {
    true_cover_df <- true_cover_df %>%
      mutate(
        shock_multiplier = case_when(
          Year < shock_year ~ 1,
          Year >= shock_year & shock_type == "Cyclonic Impact (All sites)" ~ 1 - shock_loss,
          Year >= shock_year & shock_type == "Bleaching Event (Variable impact)" ~ 1 - (shock_loss * runif(n(), 0.7, 1.3)),
          Year >= shock_year & shock_type == "Localized Impact (COTS)" & Site_ID %in% sample(site_ids, size = ceiling(n_sites_total/2)) ~ 1 - shock_loss,
          TRUE ~ 1
        ),
        True_Cover = True_Cover * shock_multiplier
      )
  }
  
  # --- 4. Simulate the Observation Process (Monitoring) ---
  observed_data <- true_cover_df %>%
    # We only observe data from Year 1 onwards
    filter(Year > 0) %>%
    crossing(Transect_ID = 1:n_transects) %>%
    mutate(
      Observed_Cover = rnorm(n(), mean = True_Cover, sd = survey_precision_sd)
    )
  
  # --- 5. Perform the Bayesian BACI Analysis ---
  # We only use data from after the intervention starts for this model
  analysis_data <- observed_data %>%
    filter(Year >= intervention_year) %>%
    mutate(Time = Year - intervention_year) # Time since intervention
  
  # Fit the Bayesian model
  # This model tests if the slope over Time is different for the Treatment site
  baci_model <- stan_lmer(
    Observed_Cover ~ Time * Site_Type + (Time | Site_ID),
    data = analysis_data,
    chains = 2, iter = 1000, refresh = 0, # Keep simulation fast for Shiny
    cores = getOption("mc.cores", 2)
  )
  
  # Extract the posterior distribution of the model parameters
  posterior_draws <- as.data.frame(baci_model)
  
  # The key parameter is the interaction term, which represents the uplift
  uplift_parameter_name <- "Time:Site_TypeTreatment"
  
  # Check if the parameter exists (it might not if the model fails)
  if(uplift_parameter_name %in% names(posterior_draws)) {
    uplift_draws <- posterior_draws[[uplift_parameter_name]]
    
    # The probability of real uplift is the proportion of the posterior > 0
    prob_real_uplift <- mean(uplift_draws > 0)
    
    # The mean uplift is the median of the posterior distribution
    mean_uplift <- median(uplift_draws)
    
  } else {
    prob_real_uplift <- 0
    mean_uplift <- 0
  }
  
  # --- 6. Calculate Credit Score and Prepare Outputs ---
  credit_score <- mean_uplift * prob_real_uplift
  
  plot_summary <- observed_data %>%
    group_by(Year, Site_Type) %>%
    summarise(
      Mean = mean(Observed_Cover, na.rm = TRUE),
      Lower_CI = quantile(Observed_Cover, 0.025, na.rm = TRUE),
      Upper_CI = quantile(Observed_Cover, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(
    plot_data = plot_summary,
    mean_uplift = mean_uplift,
    prob_real_uplift = prob_real_uplift,
    credit_score = credit_score
  ))
}