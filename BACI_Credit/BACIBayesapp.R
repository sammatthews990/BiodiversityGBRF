# app.R
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(bslib)
library(scales)
library(bsicons)
library(rstanarm)
library(DT)
library(INLA)
library(leaflet)
library(readr)
library(purrr)

source("baci_analysis_functions.R")

# --- Configuration for BACI Simulator & Power Analysis ----
survey_methods_params <- tribble(
  ~Method,                   ~SD_Precision, ~Cost_per_Transect,
  "Benthic Photo Transects", 0.045,         50,
  "RHIS (Rapid Survey)",     0.120,         25,
  "Detailed Orthomosaic",    0.020,         200,
  "ReefScan (AI Towed)",     0.035,         35
)

cat("--- METRIC_DEFINITIONS Loaded by App ---\n") # Add this line
print(head(METRIC_DEFINITIONS))                    # Add this line
cat("----------------------------------------\n") # Add this line

# --- Data Loading and PRE-PROCESSING for Model Explorer ----
# This section now includes the crucial UPLIFT CALCULATION.
modelled_data_raw <- readr::read_csv("simdata_ADRIA.csv")

# Create a separate "counterfactual" dataset
counterfactual_data <- modelled_data_raw %>%
  filter(`Deployment Volume` == 0) %>%
  select(Year, Site, Reef, `Geomorphic zone`, `Coral Cover`, `Coral Cover sd`, Diversity, `Diversity sd`, `Shelter Volume`, `Shelter Volume sd`, RCI, `RCI sd`) %>%
  rename(
    CF_Coral_Cover = `Coral Cover`, CF_Coral_Cover_sd = `Coral Cover sd`,
    CF_Diversity = Diversity, CF_Diversity_sd = `Diversity sd`,
    CF_Shelter_Volume = `Shelter Volume`, CF_Shelter_Volume_sd = `Shelter Volume sd`,
    CF_RCI = RCI, CF_RCI_sd = `RCI sd`
  )

# Join the counterfactual back to the main data and calculate uplift
modelled_data_with_uplift <- modelled_data_raw %>%
  left_join(counterfactual_data, by = c("Year", "Site", "Reef", "Geomorphic zone")) %>%
  mutate(
    # Calculate uplift for each metric
    Uplift_Coral_Cover = `Coral Cover` - CF_Coral_Cover,
    Uplift_Diversity = Diversity - CF_Diversity,
    Uplift_Shelter_Volume = `Shelter Volume` - CF_Shelter_Volume,
    Uplift_RCI = RCI - CF_RCI,
    # Calculate the SD of the uplift using variance propagation
    Uplift_Coral_Cover_sd = sqrt(`Coral Cover sd`^2 + CF_Coral_Cover_sd^2),
    Uplift_Diversity_sd = sqrt(`Diversity sd`^2 + CF_Diversity_sd^2),
    Uplift_Shelter_Volume_sd = sqrt(`Shelter Volume sd`^2 + CF_Shelter_Volume_sd^2),
    Uplift_RCI_sd = sqrt(`RCI sd`^2 + CF_RCI_sd^2)
  ) %>%
  # Rename columns for easier use in the app
  rename(
    Year = Year, Reef_Name = Reef, GeomorphicZone = `Geomorphic zone`,
    Intervention = Intervention, Deployment_Volume = `Deployment Volume`,
    Coral_Cover = `Coral Cover`, Coral_Cover_sd = `Coral Cover sd`,
    Diversity = Diversity, Diversity_sd = `Diversity sd`,
    Shelter_Volume = `Shelter Volume`, Shelter_Volume_sd = `Shelter Volume sd`,
    RCI = RCI, RCI_sd = `RCI sd`,
    Deployment_Site_Flag = `deployment site flag`,
    site_lat = `site lat`, site_long = `site long`
  ) %>%
  mutate(across(c(Reef_Name, GeomorphicZone, Intervention, Deployment_Site_Flag, Deployment_Volume), as.factor))

# UI ----
ui <- page_navbar(
  title = "Biodiversity Credit Dashboard",
  theme = bs_theme(version = 5, preset = "shiny"),
  # --- CSS: responsive value-box typography + icon sizing ---

  
  # --- THE FIX: Replace the old <head> with this new, more robust CSS ---  
  header = tags$head(
    tags$style(HTML("
    /* (your existing rules)… */
    .tab-pane { container-type: inline-size; }
    .value-box-grid { container-type: inline-size; }
    .value-box-grid .bslib-layout-gap { gap: 0.5rem !important; }
    .value-box-grid .bslib-value-box { margin: 0.25rem !important; }

    @container (max-width: 600px) {
      .bslib-value-box {
        aspect-ratio: 4 / 3;
        display: flex;
        flex-direction: column;
        justify-content: center;
      }
      .bslib-value-box .value-box-title {
        font-size: clamp(0.7rem, 6cqi, 1rem);
        white-space: normal;
      }
      .bslib-value-box .value-box-value {
        font-size: clamp(1.0rem, 12cqi, 2.2rem);
      }
      .bslib-value-box .showcase-icon {
        font-size: clamp(1.2rem, 10cqi, 2.6rem) !important;
        top: 0.5rem !important; right: 0.5rem !important;
      }
    }

    /* -------- Compact variant for short cards -------- */
    /* Desktop & wide containers */
    .value-box-compact .value-box-title {
      font-size: clamp(0.65rem, 0.95vw, 0.95rem);
      line-height: 1.15; white-space: normal;
    }
    .value-box-compact .value-box-value {
      font-size: clamp(0.9rem, 1.4vw, 1.25rem);
      line-height: 1.15;
    }
    .value-box-compact .showcase-icon {
      font-size: clamp(1.1rem, 1.8vw, 2.0rem) !important;
    }
    .value-box-compact .card-body {
      padding: 0.6rem 0.8rem;   /* tighter to help fit 120px height */
    }

    /* Narrow containers: shrink a bit more */
    @container (max-width: 600px) {
      .value-box-compact .value-box-title {
        font-size: clamp(0.6rem, 5cqi, 0.9rem);
      }
      .value-box-compact .value-box-value {
        font-size: clamp(0.85rem, 8cqi, 1.15rem);
      }
      .value-box-compact .showcase-icon {
        font-size: clamp(1.0rem, 7cqi, 1.6rem) !important;
      }
    }
  "))
  ),

  # -- TAB 1: Model Scenario Explorer -----

  tabPanel("Model Scenario Explorer",
           page_sidebar(
             sidebar = sidebar(
               width = "350px",
               open = "desktop",
               card(card_header("Map Selection"), leafletOutput("reefMap", height = 250)),
               # card(
               #   card_header("Filtering Controls"),
               #   selectInput("reef_selector", "Reef Name", choices = levels(modelled_data_with_uplift$Reef_Name), multiple = TRUE, selected = levels(modelled_data_with_uplift$Reef_Name)),
               #   checkboxGroupInput("geomorph_selector", "Geomorphic Zone", choices = levels(modelled_data_with_uplift$GeomorphicZone), selected = levels(modelled_data_with_uplift$GeomorphicZone)),
               #   checkboxGroupInput("deploy_selector", "Deployment Site", choices = levels(modelled_data_with_uplift$Deployment_Site_Flag), selected = levels(modelled_data_with_uplift$Deployment_Site_Flag)),
               #   checkboxGroupInput("interv_selector", "DHW Tolerance", choices = levels(modelled_data_with_uplift$Intervention), selected = levels(modelled_data_with_uplift$Intervention)),
               #   checkboxGroupInput("volume_selector", "Deployment Volume", choices = levels(modelled_data_with_uplift$Deployment_Volume), selected = levels(modelled_data_with_uplift$Deployment_Volume)),
               #   sliderInput("year_selector", "Year Range", min = min(modelled_data_with_uplift$Year), max = max(modelled_data_with_uplift$Year), value = c(min(modelled_data_with_uplift$Year), max(modelled_data_with_uplift$Year)), sep = "")
               # )
               accordion(
                 open = "Spatial Filters",
                 accordion_panel("Spatial Filters", value = "Spatial Filters",
                                 selectInput("reef_selector", "Reef Name", 
                                             choices = levels(modelled_data_with_uplift$Reef_Name), 
                                             multiple = TRUE, selected = levels(modelled_data_with_uplift$Reef_Name)),
                                 checkboxGroupInput("geomorph_selector", "Geomorphic Zone", 
                                             choices = levels(modelled_data_with_uplift$GeomorphicZone), 
                                             selected = levels(modelled_data_with_uplift$GeomorphicZone))
                 ),
                 accordion_panel("Intervention Filters",
                                 checkboxGroupInput("deploy_selector", "Deployment Site", inline = TRUE,
                                             choices = levels(modelled_data_with_uplift$Deployment_Site_Flag), 
                                            selected = levels(modelled_data_with_uplift$Deployment_Site_Flag)),
                                 checkboxGroupInput("interv_selector", "DHW Scenario",inline = TRUE,
                                             choices = unique(modelled_data_with_uplift$Intervention), 
                                            selected = unique(modelled_data_with_uplift$Intervention)),
                                 checkboxGroupInput("volume_selector", "Deployment Volume", inline = TRUE,
                                             choices = levels(modelled_data_with_uplift$Deployment_Volume), 
                                              selected = levels(modelled_data_with_uplift$Deployment_Volume))
                 ),
                 accordion_panel("Time Filter",
                                 sliderInput("year_selector", "Year Range", 
                                             min = min(modelled_data_with_uplift$Year), 
                                             max = max(modelled_data_with_uplift$Year), 
                                             value = c(min(modelled_data_with_uplift$Year), max(modelled_data_with_uplift$Year)), 
                                             sep = "")
                 )
               )
             ),
             
             layout_columns(
               col_widths = c(7, 5),
               card(
                 card_header(
                   class = "d-flex justify-content-between align-items-center",
                   "Metric Trends",
                   div(class = "d-flex",
                       selectInput("explorer_metric", NULL, 
                                   choices = c("Coral Cover" = "Coral_Cover", "Diversity" = "Diversity", "Shelter Volume" = "Shelter_Volume", "RCI" = "RCI"), 
                                   width = "150px"),
                       # --- THE FIX: Restored the missing choices ---
                       selectInput("explorer_color_by", NULL, 
                                   choices = c("Geomorphic Zone" = "GeomorphicZone", "Reef" = "Reef_Name", "Intervention" = "Intervention", "Deployment Volume" = "Deployment_Volume", "Deployment Flag" = "Deployment_Site_Flag"), 
                                   selected = "GeomorphicZone", width = "160px"),
                       radioButtons("plot_display_toggle", NULL, choices = c("Raw Values", "Uplift"), selected = "Raw Values", inline = TRUE)
                   )
                 ),
                 plotOutput("timeSeriesPlot", height = "400px")
               ),
               div(
                 layout_columns(
                   col_widths = 6,
                   value_box(title = "Uplift at Final Year (Deployment)", value = textOutput("final_uplift_card_deploy"), showcase = bs_icon("graph-up-arrow", size = "100%"), height = "120px"),
                   value_box(title = "Avg. Annual Uplift (Deployment)", value = textOutput("annual_uplift_card_deploy"), showcase = bs_icon("calendar-event", size = "100%"), height = "120px")
                 ),
                 layout_columns(
                   col_widths = 6,
                   value_box(title = "Uplift at Final Year (Spillover)", value = textOutput("final_uplift_card_spill"), showcase = bs_icon("graph-up", size = "100%"), max_height = "120px"),
                   value_box(title = "Avg. Annual Uplift (Spillover)", value = textOutput("annual_uplift_card_spill"), showcase = bs_icon("calendar3-range", size = "100%"), max_height = "120px")
                 ),
                 card(
                   full_screen = TRUE,
                   card_header("Detailed Modelled Data"),
                   DTOutput("dataTableExplorer")
                 )
               )
             )
           )
  ),
  # --- TAB 2: Power Analysis ----
  # --- TAB 2: Power Analysis ----
  tabPanel("Power Analysis",
           page_sidebar(
             sidebar = sidebar(
               width = "350px",
               open = "desktop",
               accordion(
                 open = "Design",
                 accordion_panel("Survey Design Parameters", icon = bs_icon("sliders"), value = "Design",
                                 numericInput("power_uplift_pct", "Annual Uplift to Detect (%)", value = 3, min = 0.5, max = 10, step = 0.5),
                                 sliderInput("power_nyears", "Monitoring Duration (Years)", min = 3, max = 10, value = 5, step = 1),
                                 radioButtons("power_frequency", "Monitoring Frequency", choices = c("Annual", "Biennial"), selected = "Annual", inline = TRUE),
                                 sliderInput("power_nctrl", "Number of Control Sites (for cards/highlight)", min = 1, max = 10, value = 5, step = 1),
                                 sliderInput("power_ntran", "Number of Transects (for cards/highlight)", min = 1, max = 20, value = 10, step = 1),
                                 selectInput("power_nctrl_selector", "Number of Control Sites to Plot", choices = 1:10, multiple = TRUE, selected = c(3, 5, 8))
                 ),
                 accordion_panel("Variability Assumptions", icon = bs_icon("graph-up-arrow"),
                                 selectInput("power_metric", "Metric to Analyze", 
                                             choices = c("Composite Index (RCI)", METRIC_DEFINITIONS$Metric)),
                                 
                                 # NEW: Slider to control the heterogeneity scenarios
                                 sliderInput("power_sd_spatial_var_pct", "Spatial Heterogeneity Scenarios (% Variation)",
                                             min = 0, max = 100, value = 50, step = 5,
                                             post = "%"),
                                 
                                 # These are now just for display of the central value
                                 numericInput("power_sd_spatial", "Central SD (among Transects)", 
                                              value = 0, min = 0, max = 0.5, step = 0.01),
                                 numericInput("power_sd_temporal", "Central SD (year-to-year)", 
                                              value = 0, min = 0, max = 0.5, step = 0.01)
                 ),
                 accordion_panel("Survey Method & Cost", icon = bs_icon("gear"),
                                 # REPLACE the single selectInput with these two
                                 selectInput("power_method_benthic", "Benthic Survey Method", 
                                             choices = benthic_survey_params$Method, 
                                             selected = "Benthic Photo Transects"),
                                 selectInput("power_method_fish", "Fish Survey Method", 
                                             choices = fish_survey_params$Method, 
                                             selected = "Underwater Visual Census (UVC)"),
                                 
                                 numericInput("cost_per_site_visit", "Cost per Site Visit (Logistics)", value = 500, min=0),
                                 actionButton("run_power_analysis", "Run Power Analysis", class = "btn-primary w-100", icon = icon("play"))
                 )
               )
             ),
             div(class = "value-box-grid",
                 layout_columns(
                   col_widths = c(3, 3, 3, 3),
                   value_box(
                     title = "Min. Detectable Uplift (at 80% Power)", max_height = "120px",
                     value = textOutput("power_mdes_txt"),
                     showcase = bs_icon("search-heart", size = "100%"),
                     class = "value-box-compact"
                   ),
                   value_box(
                     title = "Power for Target Uplift",
                     value = textOutput("power_avg_power_txt"),max_height = "120px",
                     showcase = bs_icon("check-circle-fill", size = "100%"),
                     class = "value-box-compact"
                   ),
                   value_box(
                     title = "Target Uplift",
                     value = textOutput("power_uplift_txt"), max_height = "120px",
                     showcase = bs_icon("bullseye", size = "100%"),
                     class = "value-box-compact",
                     theme_color = "primary"
                   ),
                   value_box(
                     title = "Estimated Total Cost",
                     value = textOutput("power_total_cost_txt"), max_height = "120px",
                     showcase = bs_icon("cash-coin", size = "100%"),
                     class = "value-box-compact",
                     theme_color = "primary"
                   )
                 )
             ),
             layout_columns(
               col_widths = c(12),
               card(
                 card_header("Power Curves by Metric"),
                 plotOutput("powerCurvePlot", height = "600px")
               )
             )
           )
  ),
  
  # --- TAB 3: BACI Credit Simulator ----
  tabPanel("BACI Credit Simulator",
           page_sidebar(
             sidebar = sidebar(
               width = "350px", 
               open = "desktop",
               tags$h4("Simulation Controls"),
               selectInput("sim_method", "Survey Method", choices = survey_methods_params$Method),
               sliderInput("sim_nctrl", "Number of Control Sites", min = 1, max = 10, value = 5, step = 1),
               sliderInput("sim_ntran", "Number of Transects per Site", min = 1, max = 10, value = 5, step = 1),
               tags$hr(),
               sliderInput("sim_nyears", "Monitoring Duration (Years)", min = 5, max = 20, value = 10, step = 1),
               sliderInput("sim_intervention_year", "Intervention Start Year", min = 1, max = 20, value = 3, step = 1),
               numericInput("sim_uplift_pct", "True Annual Uplift (%)", value = 5, min = 0, max = 20, step = 1),
               tags$hr(),
               selectInput("sim_shock_type", "Exogenous Shock Scenario", choices = c("No Shock", "Cyclonic Impact (All sites)", "Bleaching Event (Variable impact)", "Localized Impact (COTS)")),
               sliderInput("sim_shock_year", "Shock Event Year", min = 1, max = 20, value = 7, step = 1),
               sliderInput("sim_shock_magnitude", "Shock Magnitude (% Loss)", min = 0, max = 100, value = 50, step = 5),
               tags$hr(),
               numericInput("sim_sd_spatial", "Spatial Patchiness (SD)", value = 0.03, min = 0.01, max = 0.2, step = 0.01),
               numericInput("sim_sd_temporal", "Residual Temporal SD", value = 0.04, min = 0.01, max = 0.2, step = 0.01),
               tags$hr(),
               radioButtons("analysis_method", "Analysis Method", choices = c("Full Bayesian (Stan)", "Fast Approximation (INLA)"), selected = "Fast Approximation (INLA)"),
               actionButton("run_sim", "Run Analysis", class = "btn-primary w-100", icon = icon("play"))
             ),
             layout_columns(
               col_widths = c(4, 4, 4),
               value_box(title = "Mean Annual Uplift (Composite)", value = textOutput("uplift_card"), showcase = bs_icon("graph-up-arrow")),
               value_box(title = "Probability of Uplift (Composite)", value = textOutput("prob_card"), showcase = bs_icon("patch-check-fill")),
               value_box(title = "Final Credit Score (Composite)", value = textOutput("credit_card"), showcase = bs_icon("award-fill"), theme_color = "success")
             ),
             layout_columns(
               col_widths = c(7, 5),
               card(
                 card_header(class = "d-flex justify-content-between align-items-center", "Simulated Metric Trends", selectInput("metric_selector", NULL, choices = c("Composite Index", METRIC_DEFINITIONS$Metric), selected = "Composite Index", width = "250px")),
                 plotOutput("simulationPlot", height = "400px")
               ),
               card(card_header("Detailed Results by Metric"), DTOutput("resultsTable"))
             )
           )
  )
)

server <- function(input, output, session) {
  # --- SERVER LOGIC FOR TAB 1 ----
  reef_locations <- modelled_data_with_uplift %>% 
    group_by(Reef_Name) %>% 
    summarise(lat = first(site_lat), lng = first(site_long), .groups = "drop")
  
  output$reefMap <- renderLeaflet({
    leaflet(reef_locations) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addCircleMarkers(lng = ~lng, lat = ~lat, layerId = ~Reef_Name, label = ~Reef_Name, radius = 8, stroke = FALSE, fillOpacity = 0.8)
  })
  
  observeEvent(input$reefMap_marker_click, {
    clicked_reef <- input$reefMap_marker_click$id
    current_selection <- input$reef_selector
    if (clicked_reef %in% current_selection) { new_selection <- current_selection[current_selection != clicked_reef] } else { new_selection <- c(current_selection, clicked_reef) }
    updateSelectInput(session, "reef_selector", selected = new_selection)
  })
  
  filtered_model_data <- reactive({
    req(input$reef_selector, input$geomorph_selector, input$year_selector, input$deploy_selector, input$interv_selector, input$volume_selector)
    modelled_data_with_uplift %>%
      filter(
        Reef_Name %in% input$reef_selector,
        GeomorphicZone %in% input$geomorph_selector,
        Deployment_Site_Flag %in% input$deploy_selector,
        Intervention %in% input$interv_selector,
        Deployment_Volume %in% input$volume_selector,
        Year >= input$year_selector[1] & Year <= input$year_selector[2]
      )
  })
  
  output$timeSeriesPlot <- renderPlot({
    df <- filtered_model_data()
    validate(need(nrow(df) > 0, "No data available for the current filter settings."))
    
    # Determine which columns to use based on the toggle
    if (input$plot_display_toggle == "Uplift") {
      metric_col_name <- paste0("Uplift_", input$explorer_metric)
      sd_col_name <- paste0("Uplift_", input$explorer_metric, "_sd")
      y_lab <- paste("Uplift in", gsub("_", " ", input$explorer_metric))
    } else {
      metric_col_name <- input$explorer_metric
      sd_col_name <- paste0(input$explorer_metric, "_sd")
      y_lab <- gsub("_", " ", input$explorer_metric)
    }
    
    group_col_name <- input$explorer_color_by
    
    validate(need(sd_col_name %in% names(df), "SD column for selected metric not found."))
    
    df <- df %>% mutate(Grouping_Var = as.factor(.data[[group_col_name]]))
    
    plot_data <- df %>%
      group_by(Year, Grouping_Var) %>%
      summarise(
        Mean_Value = mean(.data[[metric_col_name]], na.rm = TRUE),
        Agg_SD = sqrt(mean(.data[[sd_col_name]]^2, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      filter(!is.na(Agg_SD)) %>%
      mutate(
        Lower_CI = Mean_Value - 1.96 * Agg_SD,
        Upper_CI = Mean_Value + 1.96 * Agg_SD
      )
    
    ggplot(plot_data, aes(x = Year, y = Mean_Value, color = Grouping_Var, fill = Grouping_Var)) +
      geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, linetype = 0) +
      geom_line(linewidth = 1.2) +
      {if(input$plot_display_toggle == "Uplift") geom_hline(yintercept = 0, linetype = "dashed")} +
      labs(y = y_lab, x = "Year", color = gsub("_", " ", group_col_name), fill = gsub("_", " ", group_col_name)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  # --- NEW: Server logic for the summary cards ---
  card_data <- reactive({
    df <- filtered_model_data()
    validate(need(nrow(df) > 0, "No data to calculate summary."))
    
    intervention_df <- df %>% filter(!Deployment_Volume == 0)
    validate(need(nrow(intervention_df) > 0, "No intervention sites in current filter to calculate uplift."))
    
    final_year <- max(intervention_df$Year)
    intervention_start <- min(intervention_df$Year[intervention_df$Deployment_Site_Flag == 1], na.rm = TRUE)
    
    uplift_col_name <- paste0("Uplift_", input$explorer_metric)
    
    # Calculate summary stats for BOTH deployment and spillover sites
    summary <- intervention_df %>%
      filter(Year == final_year, !is.na(.data[[uplift_col_name]])) %>%
      group_by(Deployment_Site_Flag) %>%
      summarise(final_uplift = mean(.data[[uplift_col_name]], na.rm = TRUE), .groups = "drop")
    
    annual_uplift_calc <- function(final_uplift) {
      if (!is.infinite(intervention_start)) {
        final_uplift / (final_year - intervention_start + 1)
      } else { NA }
    }
    
    # Extract values for each group
    deploy_data <- summary %>% filter(Deployment_Site_Flag == 1)
    spill_data <- summary %>% filter(Deployment_Site_Flag == 0)
    
    list(
      deploy_final = if(nrow(deploy_data) > 0) deploy_data$final_uplift else NA,
      deploy_annual = if(nrow(deploy_data) > 0) annual_uplift_calc(deploy_data$final_uplift) else NA,
      spill_final = if(nrow(spill_data) > 0) spill_data$final_uplift else NA,
      spill_annual = if(nrow(spill_data) > 0) annual_uplift_calc(spill_data$final_uplift) else NA
    )
  })
  
  # --- Render all four cards ---
  render_card_text <- function(value, suffix = "") {
    renderText({
      data <- card_data()
      req(data)
      val <- data[[value]]
      if (is.na(val)) "N/A" else paste0(scales::percent(val, accuracy = 0.1), suffix)
    })
  }
  
  output$final_uplift_card_deploy <- render_card_text("deploy_final")
  output$annual_uplift_card_deploy <- render_card_text("deploy_annual", " / year")
  output$final_uplift_card_spill <- render_card_text("spill_final")
  output$annual_uplift_card_spill <- render_card_text("spill_annual", " / year")
  
  output$dataTableExplorer <- renderDT({
    DT::datatable(
      filtered_model_data(),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  
  # --- SERVER LOGIC FOR TAB 2: Power Analysis ----
  
  # Reactive to get the correct CENTRAL SDs for the chosen metric
  selected_metric_params <- reactive({
    # ... (this reactive remains the same as before) ...
    metric_name <- req(input$power_metric)
    if (metric_name == "Composite Index (RCI)") {
      avg_vars <- METRIC_DEFINITIONS %>% summarise(Spatial_SD = sqrt(mean(Spatial_SD^2)), Temporal_SD = sqrt(mean(Temporal_SD^2)))
      return(list(spatial = avg_vars$Spatial_SD, temporal = avg_vars$Temporal_SD, type = "Composite"))
    } else {
      params <- METRIC_DEFINITIONS %>% filter(Metric == metric_name)
      return(list(spatial = params$Spatial_SD, temporal = params$Temporal_SD, type = params$Metric_Type))
    }
  })
  
  # ... (selected_survey_precision and observe reactives remain the same) ...
  selected_survey_precision <- reactive({
    params <- selected_metric_params()
    benthic_prec <- benthic_survey_params$SD_Precision[benthic_survey_params$Method == req(input$power_method_benthic)]
    fish_prec <- fish_survey_params$SD_Precision[fish_survey_params$Method == req(input$power_method_fish)]
    if (params$type == "Benthic") return(benthic_prec)
    else if (params$type == "Fish") return(fish_prec)
    else return(sqrt(mean(c(benthic_prec^2, fish_prec^2))))
  })
  
  observe({
    params <- selected_metric_params()
    updateNumericInput(session, "power_sd_spatial", value = round(params$spatial, 3))
    updateNumericInput(session, "power_sd_temporal", value = round(params$temporal, 3))
  })
  
  # This eventReactive is now significantly updated to run scenarios
  power_analysis_results <- eventReactive(input$run_power_analysis, {
    showNotification("Running power analysis for all scenarios...", type = "message", duration = 5)
    
    params <- selected_metric_params()
    sd_precision <- selected_survey_precision()
    variation_pct <- input$power_sd_spatial_var_pct / 100
    
    # 1. Define the three spatial heterogeneity scenarios
    scenarios <- tibble(
      Scenario = factor(c("Low Heterogeneity", "Medium (Observed)", "High Heterogeneity"), 
                        levels = c("Low Heterogeneity", "Medium (Observed)", "High Heterogeneity")),
      spatial_sd_val = c(
        params$spatial * (1 - variation_pct),
        params$spatial,
        params$spatial * (1 + variation_pct)
      )
    )
    
    # 2. Use purrr::pmap to run the power analysis for each scenario
    results <- scenarios %>%
      mutate(
        analysis = pmap(list(spatial_sd_val), ~ run_power_analysis(
          target_uplift_pct   = req(input$power_uplift_pct),
          monitoring_years    = req(input$power_nyears),
          monitoring_frequency= req(input$power_frequency),
          survey_precision_sd = sd_precision,
          peak_spatial_sd     = ..1, # This passes the spatial_sd_val for the current row
          temporal_sd         = params$temporal,
          baseline_cover_pct  = 30,
          n_ctrl_values       = as.numeric(req(input$power_nctrl_selector)),
          n_transect_values   = 1:20
        ))
      ) %>%
      select(-spatial_sd_val) %>% # No longer needed
      unnest(analysis) # Expand the results
    
    return(results)
  })
  
  # The cards should now report on the CENTRAL ("Medium") scenario only
  power_card_calcs <- reactive({
    # ... (This logic is mostly the same but now we only calculate for the medium scenario)
    req(input$power_nctrl, input$power_ntran, selected_metric_params())
    params <- selected_metric_params()
    sd_spatial <- params$spatial # Use the central value
    sd_temporal <- params$temporal
    sd_precision <- selected_survey_precision()
    nyears <- input$power_nyears
    time_points <- if (input$power_frequency == "Annual") seq(0, nyears, by = 1) else seq(0, nyears, by = 2)
    sum_sq_t <- sum((time_points - mean(time_points))^2)
    var_within_site <- sd_spatial^2 + sd_precision^2
    var_site_year <- (var_within_site / input$power_ntran) + sd_temporal^2
    se_slope <- sqrt((var_site_year / sum_sq_t) * (1 + 1 / input$power_nctrl))
    mdes <- calculate_mdes(power = 0.80, df = input$power_nctrl, se_slope = se_slope)
    
    # Cost calculation
    benthic_cost <- benthic_survey_params$Cost_per_Transect[benthic_survey_params$Method == req(input$power_method_benthic)]
    fish_cost <- fish_survey_params$Cost_per_Transect[fish_survey_params$Method == req(input$power_method_fish)]
    total_transect_cost <- benthic_cost + fish_cost
    n_visits <- nyears * (if (input$power_frequency == "Annual") 1 else 0.5)
    total_cost <- n_visits * ((1 + input$power_nctrl) * input$cost_per_site_visit + (1 + input$power_nctrl) * input$power_ntran * total_transect_cost)
    
    list(mdes = mdes, total_cost = total_cost)
  })
  
  # ... (The renderText outputs for the cards remain the same) ...
  output$power_mdes_txt <- renderText({ res <- power_card_calcs(); if(is.na(res$mdes)) "N/A" else paste0(scales::percent(res$mdes, 0.1), " / year") })
  output$power_total_cost_txt <- renderText({ paste0("$", prettyNum(power_card_calcs()$total_cost, big.mark = ",")) })
  output$power_uplift_txt <- renderText({ paste0(input$power_uplift_pct, "% per year") })
  
  # The power text must also filter for the central scenario
  output$power_avg_power_txt <- renderText({
    res <- tryCatch(power_analysis_results(), error = function(e) NULL)
    validate(need(!is.null(res), "Click 'Run Power Analysis'"))
    
    pd <- res %>% 
      filter(
        Scenario == "Medium (Observed)", # Filter for the central case
        N_Controls == input$power_nctrl, 
        N_Transects == input$power_ntran
      )
    validate(need(nrow(pd) > 0, "Adjust design and re-run"))
    
    paste0(scales::percent(pd$Power_Mean, 0.1), " (", scales::percent(pd$Power_Lower, 0.1), " – ", scales::percent(pd$Power_Upper, 0.1), ")")
  })
  
  
  # The plot function is now updated to show the different scenarios
  output$powerCurvePlot <- renderPlot({
    df <- power_analysis_results()
    validate(need(nrow(df) > 0, "Click 'Run' to generate results."))
    
    plot_data <- df %>%
      mutate(Control_Sites = factor(paste(N_Controls, "Control Sites")))
    
    point_data <- plot_data %>%
      filter(N_Controls == input$power_nctrl, N_Transects == input$power_ntran)
    
    ggplot(plot_data, aes(x = N_Transects, y = Power_Mean, color = Scenario, fill = Scenario)) +
      geom_ribbon(aes(ymin = Power_Lower, ymax = Power_Upper), alpha = 0.2, linetype = 0) +
      geom_line(linewidth = 1.1) +
      geom_vline(xintercept = input$power_ntran, linetype = "dotted", color = "gray50") +
      # Highlight the points for the selected design on all three curves
      geom_point(data = point_data, size = 3) +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
      facet_wrap(~Control_Sites) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      # Use a nice color scale for the scenarios
      scale_color_viridis_d(name = "Heterogeneity Scenario") +
      scale_fill_viridis_d(name = "Heterogeneity Scenario") +
      labs(
        title = paste("Power to Detect a", input$power_uplift_pct, "% Annual Uplift in", input$power_metric),
        subtitle = paste("Scenarios show power under low, medium (observed), and high spatial variability."),
        x = "Number of Transects per Site",
        y = "Statistical Power"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
# --- SERVER LOGIC FOR BACI Simulation Tab ----
  observeEvent(input$sim_nyears, {
    nyears <- input$sim_nyears
    updateSliderInput(session, "sim_intervention_year", max = nyears, value = min(input$sim_intervention_year, nyears))
    updateSliderInput(session, "sim_shock_year", max = nyears, value = min(input$sim_shock_year, nyears))
  })
  analysis_results <- eventReactive(input$run_sim, {
    msg <- if(input$analysis_method == "Full Bayesian (Stan)") "Running full Bayesian simulation... this will be slow." else "Running fast approximation..."
    showNotification(msg, type = "message", duration = 10)
    method_params <- survey_methods_params %>% filter(Method == input$sim_method)
    sd_precision <- method_params$SD_Precision
    run_baci_analysis(analysis_method = input$analysis_method, n_sites_ctrl = input$sim_nctrl, n_transects = input$sim_ntran, n_years = input$sim_nyears, intervention_year = input$sim_intervention_year, true_uplift_pct = input$sim_uplift_pct, shock_type = input$sim_shock_type, shock_year = input$sim_shock_year, shock_magnitude_pct = input$sim_shock_magnitude, survey_precision_sd = sd_precision, spatial_patchiness_sd = input$sim_sd_spatial, temporal_variation_sd = input$sim_sd_temporal)
  })
  
  output$uplift_card <- renderText({ req(analysis_results()); paste0(round(analysis_results()$composite_uplift * 100, 2), "%") })
  output$prob_card <- renderText({ req(analysis_results()); scales::percent(analysis_results()$composite_prob, accuracy = 0.1) })
  output$credit_card <- renderText({ req(analysis_results()); round(analysis_results()$composite_credit * 100, 1) })
  
  output$simulationPlot <- renderPlot({
    req(input$metric_selector); plot_data <- analysis_results()$plot_data %>% filter(Metric == input$metric_selector)
    y_label <- if (input$metric_selector == "Composite Index") "Reef Condition Index (Normalized)" else input$metric_selector
    y_limits <- if (input$metric_selector == "Composite Index") c(0.5, 1.5) else c(0, 1)
    y_formatter <- if (input$metric_selector == "Composite Index") scales::number_format(accuracy = 0.1) else scales::percent
    p <- ggplot(plot_data, aes(x = Year, y = Mean, color = Site_Type, fill = Site_Type)) +
      geom_vline(xintercept = input$sim_intervention_year, linetype = "dashed", color = "blue", linewidth = 1) +
      geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, linetype = 0) +
      geom_line(linewidth = 1.2) +
      annotate("text", x = input$sim_intervention_year, y = y_limits[2], label = "Intervention", color = "blue", hjust = -0.1, vjust = 1) +
      coord_cartesian(ylim = y_limits, expand = FALSE) + scale_y_continuous(labels = y_formatter, name = y_label) +
      scale_color_manual(values = c("Treatment" = "darkorange", "Control" = "gray40")) + scale_fill_manual(values = c("Treatment" = "darkorange", "Control" = "gray40")) +
      labs(title = paste("Simulated Trend for:", input$metric_selector), subtitle = paste("Design:", input$sim_nctrl, "Control Sites,", input$sim_ntran, "Transects/Site,", "using", input$sim_method), color = "Site Type", fill = "Site Type") +
      theme_minimal(base_size = 14) + theme(legend.position = "bottom")
    if (input$sim_shock_type != "No Shock") { p <- p + geom_vline(xintercept = input$sim_shock_year, linetype = "dashed", color = "red", linewidth = 1) + annotate("text", x = input$sim_shock_year, y = y_limits[2] * 0.95, label = "Shock Event", color = "red", hjust = -0.1, vjust = 1) }
    p
  })
  
  output$resultsTable <- renderDT({
    results_data <- analysis_results()$results_table
    results_data <- results_data %>% mutate(Uplift_CI = paste0(round(Uplift_CI_Lower * 100, 1), "% to ", round(Uplift_CI_Upper * 100, 1), "%")) %>% select(Metric, Mean_Uplift, Uplift_CI, Prob_Real_Uplift, Credit_Score)
    DT::datatable(results_data, rownames = FALSE, colnames = c("Metric", "Mean Annual Uplift", "95% CI of Uplift", "Probability of Uplift", "Credit Score"), options = list(dom = 't', pageLength = 10, scrollX = TRUE)) %>% formatPercentage(c("Mean_Uplift", "Prob_Real_Uplift"), digits = 1) %>% formatRound("Credit_Score", digits = 2)
  })
  
  output$simulatedDataTable <- renderDT({
    req(analysis_results()); raw_data <- analysis_results()$raw_data
    DT::datatable(raw_data, filter = 'top', rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) %>% formatPercentage(c("True_Value", "Observed_Value"), digits = 1)
  })
  
  

  output$dataTableExplorer <- renderDT({
    DT::datatable(
      filtered_model_data(),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)