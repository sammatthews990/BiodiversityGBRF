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

source("baci_analysis_functions.R")

# --- Configuration for BACI Simulator & Power Analysis ---
survey_methods_params <- tribble(
  ~Method,                   ~SD_Precision,
  "Benthic Photo Transects", 0.045,
  "RHIS (Rapid Survey)",     0.120,
  "Detailed Orthomosaic",    0.020,
  "ReefScan (AI Towed)",     0.035
)
METRIC_DEFINITIONS <- tribble(
  ~Metric,                ~Mean_Baseline, ~Temporal_SD,
  "Coral Cover",          0.30,           0.04,
  "Structural Complexity",0.40,           0.05,
  "Fish Biomass",         0.50,           0.08
)

# --- Data Loading for Model Explorer ---
modelled_data <- readr::read_csv("simdata_ADRIA.csv") %>%
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

ui <- page_navbar(
  title = "Coral Reef Analysis Dashboard",
  theme = bs_theme(version = 5, preset = "shiny"),
  
  # --- TAB 1: Model Scenario Explorer ---
  tabPanel("Model Scenario Explorer",
           page_sidebar(
             sidebar = sidebar(
               width = "350px",
               card(card_header("Map Selection"), leafletOutput("reefMap", height = 250)),
               card(
                 card_header("Filtering Controls"),
                 selectInput("reef_selector", "Reef Name", choices = levels(modelled_data$Reef_Name), multiple = TRUE, selected = levels(modelled_data$Reef_Name)[1]),
                 checkboxGroupInput("geomorph_selector", "Geomorphic Zone", choices = levels(modelled_data$GeomorphicZone), selected = levels(modelled_data$GeomorphicZone)),
                 sliderInput("year_selector", "Year Range", min = min(modelled_data$Year), max = max(modelled_data$Year), value = c(min(modelled_data$Year), max(modelled_data$Year)), sep = ""),
                 selectInput("explorer_metric", "Metric to Plot", choices = c("Coral Cover" = "Coral_Cover", "Diversity" = "Diversity", "Shelter Volume" = "Shelter_Volume", "RCI" = "RCI"), selected = "Coral_Cover"),
                 selectInput("explorer_color_by", "Group/Color By", choices = c("Geomorphic Zone" = "GeomorphicZone", "Reef" = "Reef_Name", "Intervention" = "Intervention", "Deployment Volume" = "Deployment_Volume", "Deployment Flag" = "Deployment_Site_Flag"), selected = "GeomorphicZone")
               )
             ),
             layout_columns(
               col_widths = c(7, 5),
               card(card_header("Metric Trends"), plotOutput("timeSeriesPlot", height = "400px")),
               card(full_screen = TRUE, card_header("Detailed Modelled Data"), DTOutput("dataTableExplorer"))
             )
           )
  ),
  
  # --- TAB 2: Power Analysis ---
  tabPanel("Power Analysis",
           page_sidebar(
             sidebar = sidebar(
               width = "350px",
               accordion(
                 open = "Design",
                 accordion_panel("Survey Design Parameters", icon = bs_icon("sliders"), value = "Design",
                                 numericInput("power_uplift_pct", "Annual Uplift to Detect (%)", value = 3, min = 0.5, max = 10, step = 0.5),
                                 sliderInput("power_nyears", "Monitoring Duration (Years)", min = 3, max = 10, value = 5, step = 1),
                                 radioButtons("power_frequency", "Monitoring Frequency", choices = c("Annual", "Biennial"), selected = "Annual", inline = TRUE),
                                 selectInput("power_nctrl_selector", "Number of Control Sites to Plot", choices = 1:10, multiple = TRUE, selected = c(3, 5, 8))
                 ),
                 accordion_panel("Variability Assumptions", icon = bs_icon("graph-up-arrow"),
                                 numericInput("power_sd_spatial", "Spatial Patchiness (SD)", value = 0.03, min = 0.01, max = 0.2, step = 0.01),
                                 numericInput("power_sd_temporal", "Residual Temporal SD", value = 0.04, min = 0.01, max = 0.2, step = 0.01)
                 ),
                 accordion_panel("Survey Method & Analysis", icon = bs_icon("gear"),
                                 selectInput("power_method_selector", "Survey Method", choices = survey_methods_params$Method, selected = "Benthic Photo Transects"),
                                 radioButtons("power_analysis_method", "Analysis Method", choices = c("Traditional (Formula-Based)", "Simulation-Based (INLA)"), selected = "Traditional (Formula-Based)"),
                                 actionButton("run_power_analysis", "Run Power Analysis", class = "btn-primary w-100", icon = icon("play"))
                 )
               )
             ),
             card(
               card_header("Power Curves by Number of Control Sites"),
               plotOutput("powerCurvePlot", height = "600px")
             )
           )
  ),
  
  # --- TAB 3: BACI Credit Simulator ---
  tabPanel("BACI Credit Simulator",
           page_sidebar(
             sidebar = sidebar(
               width = "350px", tags$h4("Simulation Controls"),
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
  # --- SERVER LOGIC FOR TAB 1 Explorer ---
  reef_locations <- modelled_data %>% 
    group_by(Reef_Name) %>% 
    summarise(lat = first(site_lat), lng = first(site_long), .groups = "drop")
  
  output$reefMap <- renderLeaflet({
    leaflet(reef_locations) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addCircleMarkers(lng = ~lng, lat = ~lat, layerId = ~Reef_Name, label = ~Reef_Name, radius = 8, stroke = FALSE, fillOpacity = 0.8)
  })
  
  observeEvent(input$reefMap_marker_click, {
    clicked_reef <- input$reefMap_marker_click$id
    current_selection <- input$reef_selector
    if (clicked_reef %in% current_selection) { new_selection <- current_selection[current_selection != clicked_reef] } else { new_selection <- c(current_selection, clicked_reef) }
    updateSelectInput(session, "reef_selector", selected = new_selection)
  })
  
  filtered_model_data <- reactive({
    req(input$reef_selector, input$geomorph_selector, input$year_selector)
    modelled_data %>%
      filter(
        Reef_Name %in% input$reef_selector,
        GeomorphicZone %in% input$geomorph_selector,
        Year >= input$year_selector[1] & Year <= input$year_selector[2]
      )
  })
  
  # --- CORRECTED PLOT LOGIC ---
  output$timeSeriesPlot <- renderPlot({
    df <- filtered_model_data()
    validate(need(nrow(df) > 0, "No data available for the current filter settings."))
    
    metric_col_name <- input$explorer_metric
    sd_col_name <- paste0(input$explorer_metric, "_sd")
    group_col_name <- input$explorer_color_by
    
    validate(need(sd_col_name %in% names(df), 
                  paste("SD column for '", gsub("_", " ", metric_col_name), "' not found.")))
    
    # THE FIX: Create a new, clean factor variable for grouping just before plotting.
    # This ensures that even if the column is numeric-like, ggplot treats it as a discrete group.
    df <- df %>% mutate(Grouping_Var = as.factor(.data[[group_col_name]]))
    
    plot_data <- df %>%
      group_by(Year, Grouping_Var) %>%
      summarise(
        Mean_Value = mean(.data[[metric_col_name]], na.rm = TRUE),
        Agg_SD = sqrt(mean(.data[[sd_col_name]]^2, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      filter(!is.na(Agg_SD)) %>% # Remove groups where SD could not be calculated
      mutate(
        Lower_CI = Mean_Value - 1.96 * Agg_SD,
        Upper_CI = Mean_Value + 1.96 * Agg_SD
      )
    
    ggplot(plot_data, aes(x = Year, y = Mean_Value, color = Grouping_Var, fill = Grouping_Var)) +
      geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, linetype = 0) +
      geom_line(linewidth = 1.2) +
      labs(
        y = gsub("_", " ", metric_col_name),
        x = "Year",
        color = gsub("_", " ", group_col_name),
        fill = gsub("_", " ", group_col_name)
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  # --- SERVER LOGIC FOR TAB 2: Power Analysis ---
  power_analysis_results <- eventReactive(input$run_power_analysis, {
    
    # --- Setup common parameters ---
    annual_trend <- input$power_uplift_pct / 100
    nyears <- input$power_nyears
    z_alpha <- qnorm(1 - (1 - 0.95)/2) # 95% CI
    method_params <- survey_methods_params %>% filter(Method == input$power_method_selector)
    sd_precision <- method_params$SD_Precision
    sd_spatial <- req(input$power_sd_spatial)
    sd_temporal <- req(input$power_sd_temporal)
    n_ctrl_seq <- as.numeric(req(input$power_nctrl_selector))
    transect_seq <- 1:20
    
    time_points <- if(input$power_frequency == "Annual") seq(0, nyears, by=1) else seq(0, nyears, by=2)
    sum_sq_t <- sum((time_points - mean(time_points))^2)
    
    # --- Run analysis based on selected method ---
    if (input$power_analysis_method == "Traditional (Formula-Based)") {
      showNotification("Calculating power using statistical formula...", type = "message")
      
      results <- tidyr::crossing(
        N_Controls = n_ctrl_seq,
        N_Transects = transect_seq
      ) %>%
        mutate(
          total_transect_sd = sqrt(sd_spatial^2 + sd_precision^2),
          var_site_year = (total_transect_sd^2 / N_Transects) + sd_temporal^2,
          se_slope = sqrt( (var_site_year / sum_sq_t) * (1 + 1/N_Controls) ),
          Power = pnorm( (annual_trend / se_slope) - z_alpha)
        )
      
    } else { # Simulation-Based (INLA)
      showNotification("Running INLA simulation (100 iterations)... this may take a moment.", type = "message", duration = 15)
      
      n_sims <- 100
      
      # This is a nested loop inside a reactive, which is complex but necessary here.
      # It iterates through every design scenario and runs a mini-simulation.
      scenario_list <- list()
      for (n_ctrl in n_ctrl_seq) {
        for (n_tran in transect_seq) {
          
          sim_results <- purrr::map_df(1:n_sims, ~{
            # Generate one random dataset
            sim_data <- tibble(
              Time = rep(time_points, times = 1 + n_ctrl),
              Site_ID = rep(paste0("Site", 1:(1+n_ctrl)), each = length(time_points)),
              Is_Treatment = if_else(Site_ID == "Site1", 1, 0)
            ) %>%
              mutate(
                true_value = 0.3 + (Time * 0.01) + (Time * Is_Treatment * annual_trend) + rnorm(n(), 0, sd_temporal),
                observed_value = rnorm(n(), true_value, sd = sd_precision)
              )
            
            # Fit INLA model
            model <- INLA::inla(observed_value ~ Time * Is_Treatment + f(Site_ID, model="iid"), data = sim_data)
            
            # Check for significance (p-value < 0.05 is a common proxy)
            p_value <- model$summary.fixed["Time:Is_Treatment", "0.5quant"] # This is an approximation
            detected <- p_value < 0.05 # This is a simplified check for detection
            
            tibble(detected = detected)
          })
          
          power_estimate <- mean(sim_results$detected, na.rm = TRUE)
          scenario_list[[paste(n_ctrl, n_tran)]] <- tibble(N_Controls = n_ctrl, N_Transects = n_tran, Power = power_estimate)
        }
      }
      results <- bind_rows(scenario_list)
    }
    
    return(results)
  })
  
  output$powerCurvePlot <- renderPlot({
    df <- power_analysis_results()
    validate(need(nrow(df) > 0, "Click 'Run Power Analysis' to generate results."))
    
    df <- df %>% mutate(Control_Sites = factor(paste(N_Controls, "Control Sites")))
    
    ggplot(df, aes(x = N_Transects, y = Power)) +
      geom_line(linewidth = 1.2, color = "dodgerblue") +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = "gray20") +
      facet_wrap(~Control_Sites) +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(
        title = paste("Power to Detect a", input$power_uplift_pct, "% Annual Uplift"),
        subtitle = paste("Using", input$power_method_selector, "with", input$power_analysis_method, "method"),
        x = "Number of Transects per Site",
        y = "Statistical Power"
      ) +
      theme_minimal(base_size = 14)
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