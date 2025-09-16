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
METRIC_DEFINITIONS <- tribble(
  ~Metric,                ~Mean_Baseline, ~Temporal_SD,
  "Coral Cover",          0.30,           0.04,
  "Structural Complexity",0.40,           0.05,
  "Fish Biomass",         0.50,           0.08
)

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
                                 numericInput("power_sd_spatial_peak", "Peak Spatial Patchiness (SD at 50% Cover)", value = 0.03, min = 0.01, max = 0.2, step = 0.01),
                                 numericInput("power_sd_temporal", "Residual Temporal SD", value = 0.04, min = 0.01, max = 0.2, step = 0.01),
                                 checkboxGroupInput("power_baselines_pct", "Baseline Coral Cover (%)", choices = c(10, 30, 50, 70), selected = c(10, 30, 50))
                 ),
                 accordion_panel("Survey Method & Cost", icon = bs_icon("gear"),
                                 selectInput("power_method_selector", "Survey Method", choices = survey_methods_params$Method, selected = "Benthic Photo Transects"),
                                 numericInput("cost_per_site_visit", "Cost per Site Visit", value = 500, min=0),
                                 actionButton("run_power_analysis", "Run Power Analysis", class = "btn-primary w-100", icon = icon("play"))
                 )
               )
             ),
             # layout_columns(
             #   col_widths = c(8, 4),
             #   card(
             #     card_header("Power Curves by Baseline Condition"),
             #     plotOutput("powerCurvePlot", height = "600px")
             #   ),
             #   card(
             #     uiOutput("power_summary_cards")
             #   )
             # )
             # ROW 1: four cards (same pattern as Tab 3)
             # ROW 1: four cards across (same pattern as Tab 3)
             div(class = "value-box-grid",   # container for your CSS; OUTSIDE the grid
                 layout_columns(
                   col_widths = c(3, 3, 3, 3),
                   value_box(
                     title = "Selected Uplift",
                     value = textOutput("power_uplift_txt"), max_height = "120px",
                     showcase = bs_icon("bullseye", size = "100%"),
                     class = "value-box-compact",
                     theme_color = "primary"
                   ),
                   value_box(
                     title = "Chosen Survey Method",
                     value = textOutput("power_method_txt"), max_height = "120px",
                     showcase = bs_icon("camera", size = "100%"),
                     class = "value-box-compact",
                     theme_color = "primary"
                   ),
                   # if you want static color, keep theme_color here; if dynamic, see note below
                   value_box(
                     title = "Average Power to Detect",
                     value = textOutput("power_avg_power_txt"), max_height = "120px",
                     showcase = bs_icon("check-circle-fill", size = "100%"),
                     class = "value-box-compact"
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
             
             # ROW 2: plot
             layout_columns(
               col_widths = c(12),
               card(
                 card_header("Power Curves by Baseline Condition"),
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
  power_analysis_results <- eventReactive(input$run_power_analysis, {
    
    showNotification("Running Monte Carlo simulation (10,000 iterations)... this may take a moment.", type = "message", duration = 10)
    
    n_sims <- 10000
    annual_trend <- input$power_uplift_pct / 100
    nyears <- input$power_nyears
    method_params <- survey_methods_params %>% filter(Method == input$power_method_selector)
    sd_precision <- method_params$SD_Precision
    peak_sd_spatial <- req(input$power_sd_spatial_peak)
    sd_temporal <- req(input$power_sd_temporal)
    baselines <- as.numeric(req(input$power_baselines_pct)) / 100
    
    n_ctrl_seq <- as.numeric(req(input$power_nctrl_selector))
    transect_seq <- 1:20
    
    time_points <- if(input$power_frequency == "Annual") seq(0, nyears, by=1) else seq(0, nyears, by=2)
    sum_sq_t <- sum((time_points - mean(time_points))^2)
    
    scenarios <- tidyr::crossing(
      N_Controls = n_ctrl_seq,
      N_Transects = transect_seq,
      Baseline_Cover = baselines
    )
    
    run_one_scenario <- function(n_ctrl, n_tran, baseline) {
      dynamic_sd_spatial <- calculate_dynamic_sd(p = baseline, anchor_p = 0.5, anchor_sd = peak_sd_spatial)
      total_transect_sd <- sqrt(dynamic_sd_spatial^2 + sd_precision^2)
      var_site_year <- (total_transect_sd^2 / n_tran) + sd_temporal^2
      se_slope <- sqrt( (var_site_year / sum_sq_t) * (1 + 1/n_ctrl) )
      
      ncp <- annual_trend / se_slope
      p_values <- pt(qt(0.975, df = n_ctrl), df = n_ctrl, ncp = ncp, lower.tail = FALSE)
      
      detected <- rbinom(n_sims, 1, p_values)
      
      ci <- binom.test(sum(detected), n_sims)$conf.int
      tibble(Power_Mean = mean(detected), Power_Lower = ci[1], Power_Upper = ci[2])
    }
    
    results <- scenarios %>%
      mutate(
        results = purrr::pmap(list(N_Controls, N_Transects, Baseline_Cover), run_one_scenario)
      ) %>%
      unnest(results)
    
    return(results)
  })
  
  output$powerCurvePlot <- renderPlot({
    df <- power_analysis_results()
    validate(need(nrow(df) > 0, "Click 'Run Power Analysis' to generate results."))
    
    plot_data <- df %>%
      filter(N_Controls %in% as.numeric(input$power_nctrl_selector)) %>%
      mutate(
        Control_Sites = factor(paste(N_Controls, "Control Sites")),
        Baseline_Cover = factor(paste0(Baseline_Cover * 100, "%"))
      )
    
    # --- THE FIX: Create a new summary dataset for the overall trend ---
    summary_data <- plot_data %>%
      group_by(Control_Sites, N_Transects) %>%
      summarise(
        Power_Mean = mean(Power_Mean),
        Power_Lower = min(Power_Lower),
        Power_Upper = max(Power_Upper),
        .groups = "drop"
      )
    
    point_data <- plot_data %>%
      filter(N_Controls == input$power_nctrl, N_Transects == input$power_ntran)
    
    ggplot(plot_data, aes(x = N_Transects, y = Power_Mean, color = Baseline_Cover, fill = Baseline_Cover)) +
      geom_ribbon(aes(ymin = Power_Lower, ymax = Power_Upper), alpha = 0.2, linetype = 0) +
      geom_line(linewidth = 1.2) +
      
      # --- THE FIX: Add new layers for the overall summary line and ribbon ---
      geom_ribbon(data = summary_data, aes(x = N_Transects, y = Power_Mean, ymin = Power_Lower, ymax = Power_Upper), fill = "black", alpha = 0.2, inherit.aes = FALSE) +
      geom_line(data = summary_data, aes(x = N_Transects,y = Power_Mean), color = "black", linewidth = 1.2, inherit.aes = FALSE) +
      
      geom_vline(xintercept = input$power_ntran, linetype = "dotted", color = "gray50") +
      geom_point(data = point_data, size = 3) +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = "gray20") +
      facet_wrap(~Control_Sites) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      scale_color_viridis_d(name = "Baseline Cover") +
      scale_fill_viridis_d(name = "Baseline Cover") +
      labs(
        title = paste("Power to Detect a", input$power_uplift_pct, "% Annual Uplift"),
        subtitle = paste("Using", input$power_method_selector, "(Overall trend in black)"),
        x = "Number of Transects per Site",
        y = "Statistical Power (with 95% CI from 10000 simulations)"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  # output$power_summary_cards <- renderUI({
  #   
  #   res <- tryCatch(power_analysis_results(), error = function(e) NULL)
  #   validate(need(res, "Click 'Run Power Analysis' to see results."))
  #   
  #   # --- THE FIX: Summarize across all selected baselines for the card ---
  #   power_data <- res %>%
  #     filter(
  #       N_Controls == input$power_nctrl, 
  #       N_Transects == input$power_ntran
  #     ) %>%
  #     summarise(
  #       Power_Mean = mean(Power_Mean),
  #       Power_Lower = min(Power_Lower),
  #       Power_Upper = max(Power_Upper)
  #     )
  #   
  #   validate(need(nrow(power_data) > 0, "No results for this specific design. Adjust sliders."))
  #   
  #   power_text <- paste0(
  #     scales::percent(power_data$Power_Mean, 0.1),
  #     " (", scales::percent(power_data$Power_Lower, 0.1), " - ", scales::percent(power_data$Power_Upper, 0.1), ")"
  #   )
  #   theme <- if (power_data$Power_Mean >= 0.8) "success" else "danger"
  #   
  #   method_params <- survey_methods_params %>% filter(Method == input$power_method_selector)
  #   n_visits <- input$power_nyears * (if(input$power_frequency == "Annual") 1 else 0.5)
  #   total_cost <- n_visits * ( (1 + input$power_nctrl) * input$cost_per_site_visit + (1 + input$power_nctrl) * input$power_ntran * method_params$Cost_per_Transect )
  #   
  #   tagList(
  #     value_box(title = "Selected Uplift", value = paste0(input$power_uplift_pct, "% per year"), showcase = bs_icon("bullseye", size = "200%"), theme_color = "primary"),
  #     value_box(title = "Chosen Survey Method", value = input$power_method_selector, showcase = bs_icon("camera", size = "200%"), theme_color = "primary"),
  #     value_box(
  #       title = "Average Power to Detect",
  #       value = power_text,
  #       showcase = bs_icon("check-circle-fill", size = "100%"),
  #       theme_color = theme
  #     ),
  #     value_box(title = "Estimated Total Cost", value = paste0("$", prettyNum(total_cost, big.mark = ",")), showcase = bs_icon("cash-coin", size = "200%"), theme_color = "primary")
  #   )
  # })
  # Tab 2: card texts
  output$power_uplift_txt <- renderText({
    paste0(input$power_uplift_pct, "% per year")
  })
  
  output$power_method_txt <- renderText({
    input$power_method_selector
  })
  
  output$power_avg_power_txt <- renderText({
    res <- tryCatch(power_analysis_results(), error = function(e) NULL)
    validate(need(!is.null(res), "Click 'Run Power Analysis'"))
    
    pd <- res %>%
      dplyr::filter(N_Controls == input$power_nctrl, N_Transects == input$power_ntran) %>%
      dplyr::summarise(
        Power_Mean  = mean(Power_Mean),
        Power_Lower = min(Power_Lower),
        Power_Upper = max(Power_Upper),
        .groups = "drop"
      )
    validate(need(nrow(pd) > 0, "Adjust design and re-run"))
    
    paste0(
      scales::percent(pd$Power_Mean, 0.1),
      " (", scales::percent(pd$Power_Lower, 0.1),
      " – ", scales::percent(pd$Power_Upper, 0.1), ")"
    )
  })
  
  output$power_total_cost_txt <- renderText({
    method_params <- survey_methods_params %>%
      dplyr::filter(Method == input$power_method_selector)
    
    n_visits <- input$power_nyears * (if (input$power_frequency == "Annual") 1 else 0.5)
    total_cost <- n_visits * ( (1 + input$power_nctrl) * input$cost_per_site_visit +
                                 (1 + input$power_nctrl) * input$power_ntran * method_params$Cost_per_Transect )
    paste0("$", prettyNum(total_cost, big.mark = ","))
  })

  
  # --- SERVER LOGIC FOR BACI Simulation Tab ---
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