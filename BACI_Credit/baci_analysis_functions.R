
# baci_analysis_functions.R

run_baci_analysis <- function(
    n_sites_ctrl, n_transects, n_years, intervention_year, true_uplift_pct,
    shock_type, shock_year, shock_magnitude_pct, survey_precision_sd,
    spatial_patchiness_sd, temporal_variation_sd
) {
  
  uplift_rate <- true_uplift_pct / 100
  shock_loss <- shock_magnitude_pct / 100
  n_sites_total <- 1 + n_sites_ctrl
  site_ids <- paste("Site", 1:n_sites_total)
  site_types <- c("Treatment", rep("Control", n_sites_ctrl))
  
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
  
  observed_data <- true_cover_df %>%
    crossing(Transect_ID = 1:n_transects) %>%
    mutate(Observed_Cover = rnorm(n(), mean = True_Cover, sd = survey_precision_sd))
  
  baci_data <- observed_data %>%
    mutate(Period = if_else(Year < intervention_year, "Before", "After")) %>%
    group_by(Site_ID, Site_Type, Period) %>%
    summarise(Mean_Cover = mean(Observed_Cover, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Period, values_from = Mean_Cover) %>%
    mutate(Change = After - Before)
  
  treatment_change <- baci_data %>% filter(Site_Type == "Treatment") %>% pull(Change)
  control_changes <- baci_data %>% filter(Site_Type == "Control") %>% pull(Change)
  
  # THE FIX: More robust checking before the t-test
  if (length(treatment_change) == 1 && !is.na(treatment_change) && sum(!is.na(control_changes)) >= 2) {
    ttest_result <- t.test(control_changes, mu = treatment_change, alternative = "less", na.action = "na.omit")
    prob_real_uplift <- 1 - ttest_result$p.value
    mean_uplift <- treatment_change - mean(control_changes, na.rm = TRUE)
  } else {
    prob_real_uplift <- 0.5 
    mean_uplift <- if(length(treatment_change) == 1) treatment_change - mean(control_changes, na.rm = TRUE) else 0
    mean_uplift <- ifelse(is.na(mean_uplift), 0, mean_uplift)
  }
  
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
    credit_score = credit_score,
    debug_table = baci_data # Return the intermediate data for inspection
  ))
}