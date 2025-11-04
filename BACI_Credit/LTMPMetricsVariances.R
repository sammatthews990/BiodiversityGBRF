# --- 1. SETUP AND DATA PREPARATION ---

# Load necessary libraries
# install.packages(c("lme4", "dplyr", "tidyr", "readr", "broom"))
library(lme4)     # For fitting Linear Mixed-Effects Models (LMMs)
library(dplyr)    # For data manipulation
library(tidyr)    # For data cleaning
library(readr)    # For loading data
library(broom)    # For tidying model outputs

# For demonstration, create a sample dataframe based on your image.
# In your real analysis, you would replace this with:
full_data <- readr::read_csv("monitoringdata/metricsLTMP_trimmed.csv")


# Helper function for a scaled logit transformation to handle exact 0s and 1s
logit_transform <- function(p) {
  # Smithson & Verkuilen (2006) adjustment for 0s and 1s
  n <- length(p)
  p_adj <- (p * (n - 1) + 0.5) / n
  return(log(p_adj / (1 - p_adj)))
}

clean_data <- full_data %>%
  # 1. Rename columns
  rename(
    Reef = AIMS_REEF_NAME,
    Year = REPORT_YEAR,
    Site = SITE_NO,
    Coral_Cover_raw = coral, # Keep raw for conversion to proportion
    Structural_Complexity = Complexity,
    Fish_Biomass_raw = `f.biomass`, # Keep raw for log transform
    Fish_Diversity_raw = simpD,
    Algal_Cover_raw = CCAratio# Keep raw for logit transform
  ) %>%
  # 3. APPLY TRANSFORMATIONS
  mutate(
    # Log transform for biomass. Add 1 to handle potential zeros.
    Fish_Biomass = log(Fish_Biomass_raw + 1),
    # Convert percentages to proportions and then logit transform
    Coral_Cover = logit_transform(Coral_Cover_raw / 100),
    Algal_Cover = logit_transform(Algal_Cover_raw / 100),
    Fish_Diversity = logit_transform(Fish_Diversity_raw)
    # Note: Structural_Complexity is left on its original scale
  ) %>%
  # 4. Select only the final columns needed for analysis
  select(
    Sector = A_SECTOR,
    Reef,
    Site,
    Year,
    # The five transformed metrics
    Coral_Cover,
    Structural_Complexity,
    Algal_Cover,
    Fish_Biomass,
    Fish_Diversity
  ) %>%
  # 5. Pivot to long format for modeling
  pivot_longer(
    cols = -c(Sector, Reef, Site, Year),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))



cat("--- Data successfully cleaned and prepared ---\n")
print(head(clean_data))


# --- 2. CALCULATE SPATIAL SD (WITHIN-SITE VARIANCE) ---

# The goal is to find the standard deviation of measurements (transects)
# after accounting for the larger-scale variation from reefs and sites.
# This is the "residual" variance in a hierarchical model.

Sectoral_spatial_sd <- clean_data %>%
  group_by(Sector, Metric) %>%
  summarise(
    # We will try to fit the full model, but catch any warnings.
    model_fit = list(
      tryCatch(
        # 1. Attempt the full, preferred hierarchical model
        lmer(Value ~ 1 + (1 | Reef / Site), data = pick(everything())),
        
        # 2. If ANY warning is produced, this code will run instead
        warning = function(w) {
          # Print a message to the console so we know simplification happened
          message(paste("Warning in", cur_group()$Sector, cur_group()$Metric, ":", w$message))
          message("--> Model is singular or failed to converge. Fitting simpler model (1 | Site).")
          
          # 3. Fit the simpler, more robust model
          lmer(Value ~ 1 + (1 | Site), data = pick(everything()))
        }
      )
    ),
    .groups = "drop"
  ) %>%
  # The rest of the logic for extracting variance remains the same.
  # The simpler model will still have a "Residual" variance component.
  mutate(
    variances = map(model_fit, ~ as.data.frame(VarCorr(.x))),
    Spatial_SD = map_dbl(variances, ~ .x$sdcor[.x$grp == "Residual"])
  ) %>%
  select(Sector, Metric, Spatial_SD)

cat("\n--- Spatial SD (Within-Site) Calculated ---\n")
print(Sectoral_spatial_sd)

# --- 2B. CALCULATE GENERIC (OVERALL) SPATIAL SD ---
# This is the new part. We fit one model per metric, without grouping by Region first.
generic_spatial_sd <- clean_data %>%
  group_by(Metric) %>%
  summarise(
    # The new model includes Region as the top-level random effect.
    model_fit = list(tryCatch(
      lmer(Value ~ 1 + (1 | Sector / Reef / Site), data = pick(everything())),
      warning = function(w) {
        message(paste("Warning in Generic", cur_group()$Metric, "... Fitting simpler model."))
        lmer(Value ~ 1 + (1 | Reef / Site), data = pick(everything()))
      }
    )), .groups = "drop"
  ) %>%
  mutate(
    variances = map(model_fit, ~ as.data.frame(VarCorr(.x))),
    Spatial_SD = map_dbl(variances, ~ .x$sdcor[.x$grp == "Residual"])
  ) %>%
  select(Metric, Spatial_SD)

Sectoral_spatial_sd <- Sectoral_spatial_sd |>
  mutate(Region = case_when(
    Sector %in% c("CG", "PC", "CL") ~ "Northern",
    Sector %in% c("CA", "IN", "TO", "CU", "WH") ~ "Central",
    Sector %in% c("CB", "SW", "PO") ~ "Southern"
  ))

ggplot(Sectoral_spatial_sd, aes(x = Metric, y = Spatial_SD, fill = Sector)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Spatial Standard Deviation by Metric and Region",
       x = "Metric",
       y = "Spatial SD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Region)

ggplot(Sectoral_spatial_sd, aes(x = Sector, y = Spatial_SD, fill = Sector)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Spatial Standard Deviation by Metric and Region",
       x = "Metric",
       y = "Spatial SD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Metric)
  # facet_wrap(Region~Metric, scales = "free_x")

# --- 3. CALCULATE TEMPORAL SD (Both Regional and Generic) ---
# We can do this efficiently in one go.
annual_site_means <- clean_data %>%
  group_by(Sector, Reef, Site, Year, Metric) %>%
  summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = "drop")

temporal_residuals <- annual_site_means %>%
  group_by(Sector, Reef, Site, Metric) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(Mean_Value ~ Year, data = .x)),
    residuals = map(model, ~ broom::augment(.x))
  ) %>%
  unnest(residuals)

# Get Sectoral estimates
Sectoral_temporal_sd <- temporal_residuals %>%
  group_by(Sector, Metric) %>%
  summarise(Temporal_SD = sd(.resid, na.rm = TRUE), .groups = "drop")

# Get generic (overall) estimates by grouping only by Metric
generic_temporal_sd <- temporal_residuals %>%
  group_by(Metric) %>%
  summarise(Temporal_SD = sd(.resid, na.rm = TRUE), .groups = "drop")


# --- 4. COMBINE AND SAVE RESULTS ---
Sectoral_variance <- Sectoral_spatial_sd %>% left_join(Sectoral_temporal_sd)
generic_variance <- generic_spatial_sd %>% left_join(generic_temporal_sd)

# Save these two dataframes to be loaded by the Shiny app
saveRDS(Sectoral_variance, "regional_variance_estimates.rds")
saveRDS(generic_variance, "generic_variance_estimates.rds")

cat("\n--- SectorAL VARIANCE TABLE ---\n"); print(Sectoral_variance)
cat("\n--- GENERIC VARIANCE TABLE ---\n"); print(generic_variance)
