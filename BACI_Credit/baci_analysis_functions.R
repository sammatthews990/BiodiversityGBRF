# baci_analysis_functions.R

library(dplyr)
library(tibble)
library(tidyr)
library(rstanarm)
library(INLA)

METRIC_DEFINITIONS <- tribble(
  ~Metric,                ~Mean_Baseline, ~Temporal_SD,
  "Coral Cover",          0.30,           0.04,
  "Structural Complexity",0.40,           0.05,
  "Algal Cover",          0.20,           0.06,
  "Fish Biomass",         0.50,           0.08,
  "Fish Diversity",       0.60,           0.07,
  "Invertebrate Density", 0.35,           0.09
)

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
          Year >= shock_year & shock_type == "Cyclonic Impact (All sites)" ~ 1 - shock_loss,
          Year >= shock_year & shock_type == "Bleaching Event (Variable impact)" ~ 1 - (shock_loss * runif(n(), 0.7, 1.3)),
          Year >= shock_year & shock_type == "Localized Impact (COTS)" & Site_ID %in% sample(site_ids, size = ceiling(n_sites_total/2)) ~ 1 - shock_loss,
          TRUE ~ 1
        ),
        True_Value = True_Value * shock_multiplier
      )
  }
  
  observed_data <- observed_data %>%
    mutate(Observed_Value = rnorm(n(), mean = True_Value, sd = survey_precision_sd))
  
  # --- THE FIX: RCI calculation now correctly uses Year 0 as the reference ---
  rci_data <- observed_data %>%
    group_by(Metric, Site_ID) %>%
    # Calculate the reference value ONLY from Year 0
    mutate(reference_value = mean(Observed_Value[Year == 0], na.rm = TRUE)) %>%
    ungroup() %>%
    filter(reference_value > 0) %>% # Avoid division by zero
    mutate(Normalized_Value = Observed_Value / reference_value) %>%
    # Average the normalized values to get the RCI
    group_by(Year, Site_ID, Site_Type, Transect_ID) %>%
    summarise(Observed_Value = mean(Normalized_Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(Metric = "Composite Index")
  
  analysis_input_data <- bind_rows(
    observed_data %>% select(Metric, Year, Site_ID, Site_Type, Observed_Value),
    rci_data %>% select(Metric, Year, Site_ID, Site_Type, Observed_Value)
  )
  
  results_by_metric <- analysis_input_data %>%
    filter(Year > 0) %>% # Analysis of trends starts from Year 1
    group_by(Metric) %>%
    do({
      metric_data <- .
      
      analysis_data <- metric_data %>%
        mutate(
          Time = Year,
          # Create a specific variable for the treatment effect post-intervention
          Uplift_Period = if_else(Site_Type == "Treatment" & Year >= intervention_year, 1, 0)
        )
      
      # Use a robust model that tests if the slope over Time is steeper during the uplift period
      # This is the formal statistical test for the BACI interaction
      model_formula <- Observed_Value ~ Time * Site_Type + Time:Uplift_Period + (Time | Site_ID)
      
      # Fit model using the selected engine
      if (analysis_method == "Full Bayesian (Stan)") {
        model <- stan_lmer(
          model_formula,
          data = analysis_data, chains = 2, iter = 1000, refresh = 0, cores = getOption("mc.cores", 2), na.action = na.omit
        )
        posterior <- as.data.frame(model)
        uplift_param <- "Time:Uplift_Period" # This is the parameter of interest
        
      } else { # Fast Approximation (INLA)
        # INLA requires a slightly different formula structure
        analysis_data$Site_ID_Factor <- as.factor(analysis_data$Site_ID)
        model <- INLA::inla(
          Observed_Value ~ Time * Site_Type + Time:Uplift_Period + f(Site_ID_Factor, Time, model = "iid"),
          data = analysis_data, family = "gaussian",
          control.compute = list(config = TRUE),
          control.predictor = list(compute = TRUE)
        )
        posterior <- tryCatch(
          inla.posterior.sample(1000, model),
          error = function(e) NULL
        )
        uplift_param <- "Time:Uplift_Period"
      }
      
      # Extract results
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
    }) %>%
    ungroup()
  
  composite_results <- results_by_metric %>% filter(Metric == "Composite Index")
  
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
}```