# baci_analysis_functions.R

library(dplyr)
library(tibble)
library(tidyr)
library(rstanarm)
library(INLA)

# survey_methods_params <- tribble(
#   ~Method,                   ~SD_Precision, ~Cost_per_Transect,
#   "Benthic Photo Transects", 0.04,         50,
#   "RHIS (Rapid Survey)",     0.120,         25,
#   "Detailed Orthomosaic",    0.030,         200,
#   "ReefScan (AI Towed)",     0.06,         35
# )

# 1. REPLACE survey_methods_params with TWO new tables
benthic_survey_params <- tribble(
  ~Method,                   ~SD_Precision, ~Cost_per_Transect,
  # "Benthic Photo Transects", 0.045,         50,
  # "Detailed Orthomosaic",    0.020,         200,
  # "ReefScan (AI Towed)",     0.035,         35
  "Benthic Photo Transects", 0.01,         50,
  "Detailed Orthomosaic",    0.01,         200,
  "ReefScan (AI Towed)",     0.01,         35
)

fish_survey_params <- tribble(
  ~Method,                          ~SD_Precision, ~Cost_per_Transect,
  # "Underwater Visual Census (UVC)", 0.20,          40,
  # "Diver Operated Video (DOV)",     0.18,          60,
  # "Baited Remote Video (BRUV)",     0.25,          75
  "Underwater Visual Census (UVC)", 0.01,          40,
  "Diver Operated Video (DOV)",     0.01,          60,
  "Baited Remote Video (BRUV)",     0.01,          75
)


# 2. UPDATE METRIC_DEFINITIONS to include a Metric_Type column
if (!exists("METRIC_DEFINITIONS")) {
  METRIC_DEFINITIONS <- tribble(
    ~Metric,                ~Metric_Type, ~Mean_Baseline, ~Spatial_SD, ~Temporal_SD,
    "Coral Cover",          "Benthic",    0.40,           0.078,        0.01,
    "Structural Complexity","Benthic",    0.40,           0.085,        0.04,
    "Algal Cover",          "Benthic",    0.40,           0.083,        0.01,
    "Fish Biomass",         "Fish",       0.40,           0.061,        0.02,
    "Fish Diversity",       "Fish",       0.40,           0.123,        0.03,
    "Coral Diversity",      "Benthic",    0.40,           0.085,        0.01 
  )
}

# ... (existing functions and METRIC_DEFINITIONS) ...

#' Run a Statistical Power Analysis for a BACI Design
#'
#' This function calculates the statistical power to detect a specified annual
#' trend (uplift) given a set of survey design parameters and variability assumptions.
#' It iterates through multiple scenarios for the number of control sites, transects,
#' and baseline conditions.
#'
#' @param target_uplift_pct The annual percentage uplift you want to be able to detect (e.g., 3 for 3%).
#' @param monitoring_years The total number of years for the monitoring program.
#' @param monitoring_frequency A string, either "Annual" or "Biennial".
#' @param survey_precision_sd The standard deviation associated with the measurement error of the chosen survey method.
#' @param peak_spatial_sd The peak standard deviation for spatial patchiness, anchored at 50% cover.
#' @param temporal_sd The residual year-to-year temporal standard deviation.
#' @param baseline_cover_pct A numeric vector of baseline coral cover percentages to test (e.g., c(10, 30, 50)).
#' @param n_ctrl_values A numeric vector of the number of control sites to test (e.g., c(3, 5, 8)).
#' @param n_transect_values A numeric vector of the number of transects per site to test (e.g., 1:20).
#' @param n_sims The number of Monte Carlo simulations to run for estimating confidence intervals on the power.
#' @return A tibble containing the calculated power (Power_Mean) and confidence intervals (Power_Lower, Power_Upper) for each combination of input parameters.

# ENHANCED version of run_power_analysis
run_power_analysis <- function(
    target_uplift_pct,
    monitoring_years,
    monitoring_frequency,
    survey_precision_sd,
    peak_spatial_sd,
    temporal_sd,
    n_ctrl_sites, # Changed from a vector to a single value
    n_transect_values = 1:20,
    n_sims = 1000 # Reduced for speed in interactive use
) {
  annual_trend <- target_uplift_pct / 100
  time_points <- if (monitoring_frequency == "Annual") seq(0, monitoring_years, by = 1) else seq(0, monitoring_years, by = 2)
  sum_sq_t <- sum((time_points - mean(time_points))^2)
  
  # This function now runs for each transect value
  run_one_scenario <- function(n_tran) {
    var_within_site <- peak_spatial_sd^2 + survey_precision_sd^2
    var_site_year <- (var_within_site / n_tran) + temporal_sd^2
    se_slope <- sqrt((var_site_year / sum_sq_t) * (1 + 1 / n_ctrl_sites))
    
    # 1. Calculate Power for the target uplift
    ncp <- annual_trend / se_slope
    power_estimate <- pt(qt(0.975, df = n_ctrl_sites), df = n_ctrl_sites, ncp = ncp, lower.tail = FALSE)
    ci <- tryCatch(binom.test(round(power_estimate * n_sims), n_sims)$conf.int, error = function(e) c(0,0))
    
    # 2. Calculate MDES for 80% power
    mdes <- calculate_mdes(power = 0.80, df = n_ctrl_sites, se_slope = se_slope)
    
    tibble(
      N_Transects = n_tran,
      Power_Mean = power_estimate,
      Power_Lower = ci[1],
      Power_Upper = ci[2],
      MDES = mdes
    )
  }
  
  # Use map_dfr to iterate over all transect values and row-bind the results
  results <- purrr::map_dfr(n_transect_values, run_one_scenario)
  
  return(results)
}
#' Calculate the Minimum Detectable Effect Size (MDES)
#'
#' @param power The desired statistical power (e.g., 0.80).
#' @param df Degrees of freedom, typically the number of control sites.
#' @param se_slope The standard error of the slope (interaction term).
#' @return The smallest annual trend (uplift) that can be detected with the specified power.
calculate_mdes <- function(power, df, se_slope) {
  # Find the non-centrality parameter (ncp) required to achieve the target power
  # We need to find the root of the function `power_function(ncp) - target_power = 0`
  objective_function <- function(ncp) {
    pt(qt(0.975, df = df), df = df, ncp = ncp, lower.tail = FALSE) - power
  }
  
  # Use uniroot to find the ncp. Search in a reasonable interval.
  required_ncp <- tryCatch(
    uniroot(objective_function, interval = c(1e-9, 50))$root,
    error = function(e) NA # Return NA if it can't be solved
  )
  
  # MDES is the required NCP multiplied by the standard error
  mdes <- required_ncp * se_slope
  return(mdes)
}


calculate_dynamic_sd <- function(p, anchor_p, anchor_sd) {
  k <- anchor_sd / sqrt(max(anchor_p * (1 - anchor_p), 1e-9))
  dynamic_sd <- k * sqrt(p * (1 - p))
  return(dynamic_sd)
}



run_baci_analysis <- function(
    analysis_method, n_sites_ctrl, n_transects, n_years, intervention_year, 
    true_uplift_pct, shock_type, shock_year, shock_magnitude_pct, 
    survey_precision_sd, spatial_patchiness_sd, temporal_variation_sd
) {
  
  uplift_rate <- true_uplift_pct / 100
  shock_loss <- shock_magnitude_pct / 100
  n_sites_total <- 1 + n_sites_ctrl
  site_ids <- paste("Site", 1:n_sites_total)
  site_types <- c("Treatment", rep("Control", n_sites_ctrl))
  
  # --- 1. Simulate "True" and "Observed" data for ALL 6 metrics ---
  observed_data <- METRIC_DEFINITIONS %>%
    crossing(Year = 0:n_years, Site_ID = site_ids, Transect_ID = 1:n_transects) %>%
    left_join(tibble(Site_ID = site_ids, Site_Type = site_types), by = "Site_ID") %>%
    group_by(Metric, Site_ID) %>%
    mutate(
      start_cover = rnorm(1, mean = Mean_Baseline, sd = spatial_patchiness_sd),
      temporal_noise = rnorm(n(), 0, sd = temporal_variation_sd),
      uplift_effect = if_else(Site_Type == "Treatment" & Year >= intervention_year, uplift_rate, 0),
      True_Value = start_cover + (0.01 * Year) + cumsum(temporal_noise + uplift_effect),
      True_Value = pmin(0.95, pmax(0.01, True_Value))
    ) %>%
    ungroup()
  
  if (shock_type != "No Shock") {
    observed_data <- observed_data %>%
      mutate(
        shock_multiplier = case_when(
          Year < shock_year ~ 1,
          Year >= shock_year & shock_type == "Cyclonic Impact" ~ 1 - shock_loss,
          Year >= shock_year & shock_type == "Bleaching Event" ~ 1 - (shock_loss * runif(n(), 0.7, 1.3)),
          Year >= shock_year & shock_type == "Localized Impact" & Site_ID %in% sample(site_ids, size = ceiling(n_sites_total/2)) ~ 1 - shock_loss,
          TRUE ~ 1
        ),
        True_Value = True_Value * shock_multiplier
      )
  }
  
  observed_data <- observed_data %>%
    mutate(Observed_Value = rnorm(n(), mean = True_Value, sd = survey_precision_sd))
  
  # --- 2. Calculate the Reef Condition Index (RCI) ---
  rci_data <- observed_data %>%
    group_by(Metric, Site_ID) %>%
    mutate(reference_value = mean(Observed_Value[Year == 0], na.rm = TRUE)) %>%
    ungroup() %>%
    filter(reference_value > 0.01) %>%
    mutate(Normalized_Value = Observed_Value / reference_value) %>%
    group_by(Year, Site_ID, Site_Type, Transect_ID) %>%
    summarise(Observed_Value = mean(Normalized_Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Metric = "Composite Index")
  
  analysis_input_data <- bind_rows(
    observed_data %>% select(Metric, Year, Site_ID, Site_Type, Observed_Value),
    rci_data %>% select(Metric, Year, Site_ID, Site_Type, Observed_Value)
  )
  
  # --- 3. Analyze each metric AND the RCI individually ---
  results_by_metric <- analysis_input_data %>%
    filter(Year > 0) %>%
    group_by(Metric) %>%
    do({
      metric_data <- .
      
      # Use robust error handling in case a model fails on a messy dataset
      analysis_output <- tryCatch({
        
        analysis_data <- metric_data %>%
          mutate(
            Time = Year,
            Is_Treatment = if_else(Site_Type == "Treatment", 1, 0)
          )
        
        model_formula <- Observed_Value ~ Time * Is_Treatment + (Time | Site_ID)
        uplift_param <- "Time:Is_Treatment"
        
        if (analysis_method == "Full Bayesian (Stan)") {
          model <- stan_lmer(
            model_formula, data = analysis_data, chains = 2, iter = 1000, 
            refresh = 0, cores = getOption("mc.cores", 2), na.action = na.omit
          )
          posterior <- as.data.frame(model)
          
        } else { # Fast Approximation (INLA)
          analysis_data$Site_ID_Factor <- as.factor(analysis_data$Site_ID)
          model <- INLA::inla(
            Observed_Value ~ Time * Is_Treatment + f(Site_ID_Factor, Time, model="iid"),
            data = analysis_data, family = "gaussian",
            control.compute = list(config = TRUE), control.predictor = list(compute = TRUE)
          )
          marginal <- model$marginals.fixed[[uplift_param]]
          
          posterior <- data.frame()
          if(!is.null(marginal)){
            draws <- INLA::inla.rmarginal(1000, marginal)
            posterior <- data.frame(draws)
            names(posterior) <- uplift_param
          }
        }
        
        if(uplift_param %in% names(posterior)) {
          draws <- posterior[[uplift_param]]
          prob_uplift <- mean(draws > 0)
          mean_uplift <- median(draws)
          lower_ci <- quantile(draws, 0.025)
          upper_ci <- quantile(draws, 0.975)
        } else {
          prob_uplift <- 0; mean_uplift <- 0; lower_ci <- 0; upper_ci <- 0
        }
        
        tibble(
          Mean_Uplift = mean_uplift, Uplift_CI_Lower = lower_ci,
          Uplift_CI_Upper = upper_ci, Prob_Real_Uplift = prob_uplift,
          Credit_Score = mean_uplift * prob_uplift
        )
        
      }, error = function(e) {
        # If any error occurs, return an un-powered result instead of crashing
        tibble(
          Mean_Uplift = NA, Uplift_CI_Lower = NA, Uplift_CI_Upper = NA, 
          Prob_Real_Uplift = NA, Credit_Score = NA
        )
      })
      
      analysis_output
    }) %>%
    ungroup()
  
  # --- 4. Prepare Final Outputs ---
  composite_results <- results_by_metric %>%
    filter(Metric == "Composite Index")
  
  plot_summary <- analysis_input_data %>%
    group_by(Metric, Year, Site_Type) %>%
    summarise(
      Mean = mean(Observed_Value, na.rm = TRUE),
      Lower_CI = quantile(Observed_Value, 0.025, na.rm = TRUE),
      Upper_CI = quantile(Observed_Value, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(
    plot_data = plot_summary,
    results_table = results_by_metric,
    composite_uplift = composite_results$Mean_Uplift,
    composite_prob = composite_results$Prob_Real_Uplift,
    composite_credit = composite_results$Credit_Score,
    raw_data = observed_data %>% select(Metric, Year, Site_ID, Site_Type, Transect_ID, True_Value, Observed_Value)
  ))
}


