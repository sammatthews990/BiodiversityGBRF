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

# --- Configuration for BACI Simulator & Power Analysis ---
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
  title = "Biodiversity Credit Dashboard",
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
             layout_columns(
               col_widths = c(8, 4),
               card(
                 card_header("Power Curves by Baseline Condition"),
                 plotOutput("powerCurvePlot", height = "600px")
               ),
               card(
                 uiOutput("power_summary_cards")
               )
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
  
  output$power_summary_cards <- renderUI({
    
    res <- tryCatch(power_analysis_results(), error = function(e) NULL)
    validate(need(res, "Click 'Run Power Analysis' to see results."))
    
    # --- THE FIX: Summarize across all selected baselines for the card ---
    power_data <- res %>%
      filter(
        N_Controls == input$power_nctrl, 
        N_Transects == input$power_ntran
      ) %>%
      summarise(
        Power_Mean = mean(Power_Mean),
        Power_Lower = min(Power_Lower),
        Power_Upper = max(Power_Upper)
      )
    
    validate(need(nrow(power_data) > 0, "No results for this specific design. Adjust sliders."))
    
    power_text <- paste0(
      scales::percent(power_data$Power_Mean, 0.1),
      " (", scales::percent(power_data$Power_Lower, 0.1), " - ", scales::percent(power_data$Power_Upper, 0.1), ")"
    )
    theme <- if (power_data$Power_Mean >= 0.8) "success" else "danger"
    
    method_params <- survey_methods_params %>% filter(Method == input$power_method_selector)
    n_visits <- input$power_nyears * (if(input$power_frequency == "Annual") 1 else 0.5)
    total_cost <- n_visits * ( (1 + input$power_nctrl) * input$cost_per_site_visit + (1 + input$power_nctrl) * input$power_ntran * method_params$Cost_per_Transect )
    
    tagList(
      value_box(title = "Selected Uplift", value = paste0(input$power_uplift_pct, "% per year"), showcase = bs_icon("bullseye"), theme_color = "primary"),
      value_box(title = "Chosen Survey Method", value = input$power_method_selector, showcase = bs_icon("camera"), theme_color = "primary"),
      value_box(
        title = "Average Power to Detect",
        value = power_text,
        showcase = bs_icon("check-circle-fill"),
        theme_color = theme
      ),
      value_box(title = "Estimated Total Cost", value = paste0("$", prettyNum(total_cost, big.mark = ",")), showcase = bs_icon("cash-coin"), theme_color = "primary")
    )
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