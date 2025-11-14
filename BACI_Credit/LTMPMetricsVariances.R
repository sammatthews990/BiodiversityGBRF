# --- 1. SETUP AND DATA PREPARATION ---

# Load necessary libraries
# install.packages(c("lme4", "dplyr", "tidyr", "readr", "broom"))
library(lme4)     # For fitting Linear Mixed-Effects Models (LMMs)
library(dplyr)    # For data manipulation
library(tidyr)    # For data cleaning
library(readr)    # For loading data
library(broom)    # For tidying model outputs
library(ggplot2)
library(purrr)


# For demonstration, create a sample dataframe based on your image.
# In your real analysis, you would replace this with:
full_data <- readr::read_csv("BACI_Credit/monitoringdata/metricsLTMP.csv") |>
  dplyr::select(1:16, f.richness, f.biomass, FsimpD, simpD) |>
  dplyr::mutate(CCARatio = cca/(algae+turf+cca))


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
    Coral_Diversity_Raw = simpD,
    Fish_Biomass_raw = `f.biomass`, # Keep raw for log transform
    Fish_Diversity_raw = FsimpD,
    Algal_Cover_raw = CCAratio# Keep raw for logit transform
  ) %>%
  # 3. APPLY TRANSFORMATIONS
  mutate(
    # Log transform for biomass. Add 1 to handle potential zeros.
    Fish_Biomass_log = log(Fish_Biomass_raw + 1),
    Fish_Biomass_sc = (Fish_Biomass_log - min(Fish_Biomass_log)) / 
      (max(Fish_Biomass_log)-min(Fish_Biomass_log)),
    Coral_Cover_sc = (Coral_Cover_raw - min(Coral_Cover_raw)) / 
      (max(Coral_Cover_raw)-min(Coral_Cover_raw)),
    Algal_Cover_sc = (Algal_Cover_raw - min(Algal_Cover_raw)) / 
      (max(Algal_Cover_raw)-min(Algal_Cover_raw)),
    Fish_Diversity_sc = (Fish_Diversity_raw - min(Fish_Diversity_raw)) / 
      (max(Fish_Diversity_raw)-min(Fish_Diversity_raw)),
    Coral_Diversity_sc = (Coral_Diversity_Raw - min(Coral_Diversity_Raw)) / 
      (max(Coral_Diversity_Raw)-min(Coral_Diversity_Raw)),
    Structural_Complexity_sc = (Structural_Complexity - min(Structural_Complexity, na.rm = T)) / 
      (max(Structural_Complexity, na.rm = T)-min(Structural_Complexity, na.rm = T))
  ) %>%
  rowwise() %>%
  mutate(RCI = mean(c(Coral_Cover_sc, Coral_Diversity_sc,
                           Structural_Complexity_sc,
                           Algal_Cover_sc,
                           Fish_Biomass_sc,
                           Fish_Diversity_sc), na.rm = T)) %>%
  # 4. Select only the final columns needed for analysis
  select(
    Sector = A_SECTOR,
    Reef,
    Site,
    Year,
    # The five transformed metrics
    Coral_Cover_sc,
    Coral_Diversity_sc,
    Structural_Complexity_sc,
    Algal_Cover_sc,
    Fish_Biomass_sc,
    Fish_Diversity_sc,
    RCI#,
    # Coral_Cover_raw, Algal_Cover_raw, Fish_Diversity_raw,
    # Fish_Biomass_raw, Coral_Diversity_Raw
  ) %>%
  # 5. Pivot to long format for modeling
  pivot_longer(
    cols = -c(Sector, Reef, Site, Year),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))

# Naive SD calculations for comparison
sds_naive <- clean_data %>%
  group_by(Sector, Metric, Year, Reef) %>%
  summarise(Mean = mean(Value, na.rm = T),
            Naive_SD = sd(Value, na.rm = TRUE), .groups = "drop")
sds_naive_sum <- sds_naive %>%
  # group_by(Metric) %>%
  group_by(Sector, Metric) %>%
  summarise(Mean_Naive_SD = round(mean(Naive_SD, na.rm = T),3), .groups = "drop") 
ggplot(sds_naive, aes(x = Sector, y = Naive_SD)) +
  geom_boxplot(position = "dodge") +
  theme_minimal() +
  labs(title = "Naive Standard Deviation by Metric and Sector",
       x = "Metric",
       y = "Naive SD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Metric, scales = "free_y")

ggplot(sds_naive, aes(x=Mean, y=Naive_SD)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~Metric, scales="free")
cat("--- Data successfully cleaned and prepared ---\n")
print(head(clean_data))



# --- 2. CALCULATE SPATIAL SD (WITHIN-SITE VARIANCE) ---

# The goal is to find the standard deviation of measurements (transects)
# after accounting for the larger-scale variation from reefs and sites.
# This is the "residual" variance in a hierarchical model.

Sectoral_spatial_sd <- clean_data %>%
  # STEP 1: Add YEAR to the grouping to isolate within-year variance
  group_by(Sector, Metric, Year) %>%
  summarise(
    # STEP 2: Fit the model for each year's data subset.
    model_fit = list(
      tryCatch(
        lmer(Value ~ 1 + (1 | Reef / Site), data = pick(everything())),
        warning = function(w) {
          message(paste("Warning in", cur_group()$Sector, cur_group()$Metric, cur_group()$Year, "... Fitting simpler model."))
          lmer(Value ~ 1 + (1 | Site), data = pick(everything()))
        }
      )
    ),
    .groups = "drop"
  ) %>%
  # This gives us a yearly estimate of Spatial_SD
  mutate(
    variances = map(model_fit, ~ as.data.frame(VarCorr(.x))),
    Spatial_SD_yearly = map_dbl(variances, ~ .x$sdcor[.x$grp == "Residual"])
  ) %>%
  
  # STEP 3: Pool the yearly estimates to get a final, robust Spatial_SD
  group_by(Sector, Metric) %>%
  summarise(
    # First, calculate the mean of the variances (sd^2)
    Mean_of_Variances = mean(Spatial_SD_yearly^2, na.rm = TRUE),
    # Then, take the square root of the mean variance
    Spatial_SD = round(sqrt(Mean_of_Variances),4),
    .groups = "drop"
  ) %>%
  select(Sector, Metric, Spatial_SD) # Keep only the final columns


cat("\n--- Spatial SD (Within-Site) Calculated ---\n")
print(Sectoral_spatial_sd)

Sectoral_spatial_sd_includingyears <- clean_data %>%
  # STEP 1: Add YEAR to the grouping to isolate within-year variance
  group_by(Sector, Metric) %>%
  summarise(
    # STEP 2: Fit the model for each year's data subset.
    model_fit = list(
      tryCatch(
        lmer(Value ~ 1 + (1 | Reef / Site), data = pick(everything())),
        warning = function(w) {
          message(paste("Warning in", cur_group()$Sector, cur_group()$Metric, cur_group()$Year, "... Fitting simpler model."))
          lmer(Value ~ 1 + (1 | Site), data = pick(everything()))
        }
      )
    ),
    .groups = "drop"
  ) %>%
  # This gives us a yearly estimate of Spatial_SD
  mutate(
    variances = map(model_fit, ~ as.data.frame(VarCorr(.x))),
    Spatial_SD = round(map_dbl(variances, ~ .x$sdcor[.x$grp == "Residual"]),3)
  ) %>%
  select(Sector, Metric, Spatial_SD) # Keep only the final columns


cat("\n--- Spatial SD (Within-Site) Calculated ---\n")
print(Sectoral_spatial_sd)

# --- 1. SETUP AND DATA PREPARATION ---

# Load necessary libraries
# install.packages(c("lme4", "dplyr", "tidyr", "readr", "broom"))
library(lme4)     # For fitting Linear Mixed-Effects Models (LMMs)
library(dplyr)    # For data manipulation
library(tidyr)    # For data cleaning
library(readr)    # For loading data
library(broom)    # For tidying model outputs
library(ggplot2)
library(purrr)


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
    Coral_Diversity_Raw = simpD,
    Fish_Biomass_raw = `f.biomass`, # Keep raw for log transform
    Fish_Diversity_raw = FsimpD,
    Algal_Cover_raw = CCAratio# Keep raw for logit transform
  ) %>%
  # 3. APPLY TRANSFORMATIONS
  mutate(
    # Log transform for biomass. Add 1 to handle potential zeros.
    Fish_Biomass_log = log(Fish_Biomass_raw + 1),
    Fish_Biomass_sc = (Fish_Biomass_log - min(Fish_Biomass_log)) / 
      (max(Fish_Biomass_log)-min(Fish_Biomass_log)),
    Coral_Cover_sc = (Coral_Cover_raw - min(Coral_Cover_raw)) / 
      (max(Coral_Cover_raw)-min(Coral_Cover_raw)),
    Algal_Cover_sc = (Algal_Cover_raw - min(Algal_Cover_raw)) / 
      (max(Algal_Cover_raw)-min(Algal_Cover_raw)),
    Fish_Diversity_sc = (Fish_Diversity_raw - min(Fish_Diversity_raw)) / 
      (max(Fish_Diversity_raw)-min(Fish_Diversity_raw)),
    Coral_Diversity_sc = (Coral_Diversity_Raw - min(Coral_Diversity_Raw)) / 
      (max(Coral_Diversity_Raw)-min(Coral_Diversity_Raw)),
    Structural_Complexity_sc = (Structural_Complexity - min(Structural_Complexity, na.rm = T)) / 
      (max(Structural_Complexity, na.rm = T)-min(Structural_Complexity, na.rm = T))
  ) %>%
  rowwise() %>%
  mutate(RCI = mean(c(Coral_Cover_sc, Coral_Diversity_sc,
                      Structural_Complexity_sc,
                      Algal_Cover_sc,
                      Fish_Biomass_sc,
                      Fish_Diversity_sc), na.rm = T)) %>%
  # 4. Select only the final columns needed for analysis
  select(
    Sector = A_SECTOR,
    Reef,
    Site,
    Year,
    # The five transformed metrics
    Coral_Cover_sc,
    Coral_Diversity_sc,
    Structural_Complexity_sc,
    Algal_Cover_sc,
    Fish_Biomass_sc,
    Fish_Diversity_sc,
    RCI,
    Coral_Cover_raw, Algal_Cover_raw, Fish_Diversity_raw,
    Fish_Biomass_raw, Coral_Diversity_Raw
  ) %>%
  # 5. Pivot to long format for modeling
  pivot_longer(
    cols = -c(Sector, Reef, Site, Year),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))

annual_changes <- clean_data %>%
  group_by(Sector, Reef, Site, Metric, Year) %>%
  summarise(Mean = mean(Value, na.rm = TRUE)) %>%
  group_by(Sector, Reef, Site, Metric) %>%
  arrange(Year) %>%
  mutate(Yearly_Change = Mean - lag(Mean)) %>%
  ungroup()

ggplot(annual_changes %>% filter(Metric %in% c("Coral_Cover_sc",
                                                "Coral_Diversity_sc",
                                               "Structural_Complexity_sc",
                                              "Algal_Cover_sc",
                                               "Fish_Biomass_sc",
                                               "Fish_Diversity_sc","RCI")), 
       aes(x=Metric, y=abs(Yearly_Change))) +
  geom_boxplot() 

# Naive SD calculations for comparison
sds_naive <- clean_data %>%
  group_by(Sector, Metric, Year, Reef) %>%
  summarise(Mean = mean(Value, na.rm = T),
            Naive_SD = sd(Value, na.rm = TRUE), .groups = "drop")
sds_naive_sum <- sds_naive %>%
  # group_by(Metric) %>%
  group_by(Sector, Metric) %>%
  summarise(Mean_Naive_SD = round(mean(Naive_SD, na.rm = T),3), .groups = "drop") 
ggplot(sds_naive, aes(x = Sector, y = Naive_SD)) +
  geom_boxplot(position = "dodge") +
  theme_minimal() +
  labs(title = "Naive Standard Deviation by Metric and Sector",
       x = "Metric",
       y = "Naive SD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Metric, scales = "free_y")

ggplot(sds_naive, aes(x=Mean, y=Naive_SD)) +
  geom_point() +
  geom_smooth(method = "lm")+
  facet_wrap(~Metric, scales="free")
cat("--- Data successfully cleaned and prepared ---\n")
print(head(clean_data))



# --- 2. CALCULATE SPATIAL SD (WITHIN-SITE VARIANCE) ---

# The goal is to find the standard deviation of measurements (transects)
# after accounting for the larger-scale variation from reefs and sites.
# This is the "residual" variance in a hierarchical model.

Sectoral_spatial_sd <- clean_data %>%
  # STEP 1: Add YEAR to the grouping to isolate within-year variance
  group_by(Sector, Metric, Year) %>%
  summarise(
    # STEP 2: Fit the model for each year's data subset.
    model_fit = list(
      tryCatch(
        lmer(Value ~ 1 + (1 | Reef / Site), data = pick(everything())),
        warning = function(w) {
          message(paste("Warning in", cur_group()$Sector, cur_group()$Metric, cur_group()$Year, "... Fitting simpler model."))
          lmer(Value ~ 1 + (1 | Site), data = pick(everything()))
        }
      )
    ),
    .groups = "drop"
  ) %>%
  # This gives us a yearly estimate of Spatial_SD
  mutate(
    variances = map(model_fit, ~ as.data.frame(VarCorr(.x))),
    Spatial_SD_yearly = map_dbl(variances, ~ .x$sdcor[.x$grp == "Residual"])
  ) %>%
  
  # STEP 3: Pool the yearly estimates to get a final, robust Spatial_SD
  group_by(Sector, Metric) %>%
  summarise(
    # First, calculate the mean of the variances (sd^2)
    Mean_of_Variances = mean(Spatial_SD_yearly^2, na.rm = TRUE),
    # Then, take the square root of the mean variance
    Spatial_SD = round(sqrt(Mean_of_Variances),4),
    .groups = "drop"
  ) %>%
  select(Sector, Metric, Spatial_SD) # Keep only the final columns


cat("\n--- Spatial SD (Within-Site) Calculated ---\n")
print(Sectoral_spatial_sd)

Sectoral_spatial_sd_includingyears <- clean_data %>%
  # STEP 1: Add YEAR to the grouping to isolate within-year variance
  group_by(Sector, Metric) %>%
  summarise(
    # STEP 2: Fit the model for each year's data subset.
    model_fit = list(
      tryCatch(
        lmer(Value ~ 1 + (1 | Reef / Site), data = pick(everything())),
        warning = function(w) {
          message(paste("Warning in", cur_group()$Sector, cur_group()$Metric, cur_group()$Year, "... Fitting simpler model."))
          lmer(Value ~ 1 + (1 | Site), data = pick(everything()))
        }
      )
    ),
    .groups = "drop"
  ) %>%
  # This gives us a yearly estimate of Spatial_SD
  mutate(
    variances = map(model_fit, ~ as.data.frame(VarCorr(.x))),
    Spatial_SD = round(map_dbl(variances, ~ .x$sdcor[.x$grp == "Residual"]),3)
  ) %>%
  select(Sector, Metric, Spatial_SD) # Keep only the final columns


cat("\n--- Spatial SD (Within-Site) Calculated ---\n")
print(Sectoral_spatial_sd)

Sectoral_spatial_sd %>% group_by(Metric) %>%
  summarise(Mean = mean(Spatial_SD))
Sectoral_spatial_sd_includingyears %>% group_by(Metric) %>%
  summarise(Mean = mean(Spatial_SD))

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

ggplot(Sectoral_spatial_sd, aes(x = Sector, y = Spatial_SD, fill = Sector)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Spatial Standard Deviation by Metric and Region",
       x = "Metric",
       y = "Spatial SD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Metric)

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

ggplot(Sectoral_spatial_sd, aes(x = Sector, y = Spatial_SD, fill = Sector)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Spatial Standard Deviation by Metric and Region",
       x = "Metric",
       y = "Spatial SD") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  facet_wrap(~Metric)

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
