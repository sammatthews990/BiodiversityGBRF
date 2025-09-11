# app.R
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(DT)
library(bslib)
library(scales)
library(bsicons)

# ---- Helper Function for Dynamic SD ----
calculate_dynamic_sd <- function(p, anchor_p, anchor_sd) {
  k <- anchor_sd / sqrt(max(anchor_p * (1 - anchor_p), 1e-9))
  dynamic_sd <- k * sqrt(p * (1 - p))
  return(dynamic_sd)
}

# ---- Pre-defined Survey Method Parameters ----
survey_methods_params <- tribble(
  ~Method,                   ~SD_Precision, ~Cost_per_Transect,
  "Benthic Photo Transects", 0.045,         50,
  "RHIS (Rapid Survey)",     0.120,         25,
  "Detailed Orthomosaic",    0.020,         200,
  "ReefScan (AI Towed)",     0.040,         35
)

theme <- bs_theme(
  version = 5,
  base_font    = font_google("Inter", wght = c(400, 600, 700)),
  heading_font = font_google("Inter", wght = c(700)),
  primary = "#007bff",
  "table-cell-padding-y" = "0.2rem"
)

# Define slider maximums
max_transects <- 20
max_sites <- 20

# --- UI Definition ---
ui <- page_navbar(
  title = "BACI Survey Design Simulator",
  theme = theme,
  
  # --- TAB 1: POWER ANALYSIS ---
  tabPanel("Power Analysis",
           page_sidebar(
             sidebar = sidebar(
               width = "350px",
               accordion(
                 open = "Design",
                 accordion_panel(
                   "Survey Design Parameters", icon = bs_icon("sliders"), value = "Design",
                   numericInput("power_uplift_pct", "Annual Uplift to Detect (%)", value = 3, min = 0.5, max = 10, step = 0.5),
                   sliderInput("power_nyears", "Monitoring Duration (Years)", min = 3, max = 10, value = 5, step = 1),
                   radioButtons("monitoring_frequency", "Monitoring Frequency",
                                choices = c("Every 6 Months", "Annual", "Biennial", "Start & End Only"),
                                selected = "Annual", inline = TRUE),
                   sliderInput("power_nctrl", "Number of Control Sites", min = 1, max = max_sites, value = 5, step = 1),
                   sliderInput("power_ntran", "Number of Transects per Site", min = 1, max = max_transects, value = 5, step = 1)
                 ),
                 accordion_panel(
                   "Variability Assumptions", icon = bs_icon("graph-up-arrow"),
                   numericInput("power_sd_spatial", "Peak Spatial Patchiness (SD at 50% Cover)", value = 0.03, min = 0.01, max = 0.2, step = 0.01),
                   numericInput("power_sd_temporal", "Residual Temporal SD", value = 0.04, min = 0.01, max = 0.2, step = 0.01),
                   sliderInput("power_temporal_uncertainty", "Uncertainty in Temporal SD (%)", min = 0, max = 100, value = 25, step = 5),
                   checkboxGroupInput("power_baselines_pct", "Baseline Coral Cover (%)",
                                      choices = c(10, 20, 30, 40, 50),
                                      selected = c(10, 30, 50))
                 ),
                 accordion_panel(
                   "Survey Method & Cost", icon = bs_icon("gear"),
                   checkboxGroupInput("survey_methods", "Select Survey Methods to Compare",
                                      choices = survey_methods_params$Method,
                                      selected = c("Benthic Photo Transects", "Detailed Orthomosaic")),
                   tags$hr(),
                   tags$h5("Survey Cost Calculator"),
                   numericInput("cost_per_site_visit", "Cost per Site Visit (Travel, etc.)", value = 500, min=0)
                 )
               )
             ),
             layout_columns(
               col_widths = c(12, 12), row_heights = c(6, 4),
               card(full_screen = TRUE, card_header("Power Curves with 95% Confidence Interval"), plotOutput("powerCurvePlot")),
               card(card_header("Design Scenarios & Cost-Effectiveness"), DTOutput("scenarioTable"))
             )
           )
  ),
  
  # --- TAB 2: SIMULATION VISUALIZER ---
  tabPanel("Simulation Visualizer",
           page_sidebar(
             sidebar = sidebar(
               width = "350px",
               tags$h4("Simulation Controls"),
               selectInput("sim_method", "Survey Method", choices = survey_methods_params$Method, selected = "Benthic Photo Transects"),
               sliderInput("sim_nctrl", "Number of Control Sites", min = 1, max = 10, value = 5, step = 1),
               sliderInput("sim_ntran", "Number of Transects per Site", min = 1, max = 10, value = 5, step = 1),
               tags$hr(),
               sliderInput("sim_nyears", "Simulation Duration (Years)", min = 5, max = 20, value = 10, step = 1),
               sliderInput("sim_intervention_year", "Intervention Start Year", min = 1, max = 20, value = 3, step = 1),
               numericInput("sim_uplift_pct", "Annual Uplift Post-Intervention (%)", value = 5, min = 0, max = 20, step = 1),
               tags$hr(),
               selectInput("sim_shock_type", "Exogenous Shock Scenario",
                           choices = c("No Shock", "Cyclonic Impact (All sites)", "Bleaching Event (Variable impact)", "Localized Impact (COTS)")),
               sliderInput("sim_shock_year", "Shock Event Year", min = 1, max = 20, value = 7, step = 1),
               sliderInput("sim_shock_magnitude", "Shock Magnitude (% Coral Loss)", min = 0, max = 100, value = 50, step = 5),
               actionButton("run_sim", "Run Simulation", class = "btn-primary w-100", icon = icon("play"))
             ),
             card(
               card_header("Simulated Coral Cover Trends"),
               plotOutput("simulationPlot", height = "600px")
             )
           )
  )
)


server <- function(input, output, session){
  
  # --- TAB 1 SERVER LOGIC ---
  
  # Main reactive for calculating power for ALL outputs on Tab 1
  power_analysis_data <- reactive({
    validate(
      need(input$power_baselines_pct, "Please select at least one 'Baseline Coral Cover'."),
      need(input$survey_methods, "Please select at least one 'Survey Method'.")
    )
    
    n_sims <- 500
    annual_trend <- input$power_uplift_pct / 100
    baselines <- as.numeric(input$power_baselines_pct) / 100
    z_alpha <- qnorm(1 - (1 - 0.95)/2)
    nyears <- input$power_nyears
    selected_methods <- req(input$survey_methods)
    
    mean_sd_temporal <- req(input$power_sd_temporal)
    sd_of_sd_temporal <- mean_sd_temporal * (input$power_temporal_uncertainty / 100)
    peak_sd_spatial <- req(input$power_sd_spatial)
    
    plot_scenarios <- tidyr::crossing(N_Controls = input$power_nctrl, N_Transects = 1:max_transects)
    table_n_ctrl_scenarios <- unique(sort(c(max(1, input$power_nctrl - 2), input$power_nctrl, input$power_nctrl + 2)))
    table_n_tran_scenarios <- unique(sort(c(max(1, input$power_ntran - 2), input$power_ntran, input$power_ntran + 2)))
    table_scenarios <- tidyr::crossing(N_Controls = table_n_ctrl_scenarios, N_Transects = table_n_tran_scenarios)
    all_scenarios <- dplyr::bind_rows(plot_scenarios, table_scenarios) %>% dplyr::distinct()
    
    time_points <- switch(input$monitoring_frequency,
                          "Every 6 Months" = seq(0, nyears, by = 0.5),
                          "Annual" = seq(0, nyears, by = 1),
                          "Biennial" = unique(c(seq(0, nyears, by = 2), nyears)),
                          "Start & End Only" = c(0, nyears))
    
    sum_sq_t <- sum((time_points - mean(time_points))^2)
    
    sim_data <- tidyr::crossing(sim_id = 1:n_sims, all_scenarios, Baseline_Cover = baselines, Method = selected_methods) %>%
      left_join(survey_methods_params, by = "Method") %>%
      mutate(
        sim_sd_temporal = pmax(0.001, rnorm(n(), mean = mean_sd_temporal, sd = sd_of_sd_temporal)),
        dynamic_sd_spatial = calculate_dynamic_sd(p = Baseline_Cover, anchor_p = 0.5, anchor_sd = peak_sd_spatial),
        total_transect_sd = sqrt(dynamic_sd_spatial^2 + SD_Precision^2),
        var_site_year = (total_transect_sd^2 / N_Transects) + sim_sd_temporal^2,
        se_slope = sqrt( (var_site_year / sum_sq_t) * (1 + 1/N_Controls) ),
        Power = pnorm( (annual_trend / se_slope) - z_alpha)
      )
    
    summary_data <- sim_data %>%
      group_by(Method, N_Controls, N_Transects, Baseline_Cover, Cost_per_Transect) %>%
      summarise(Power_Median = median(Power), Power_Lower_CI = quantile(Power, 0.025), Power_Upper_CI = quantile(Power, 0.975), .groups = "drop")
    
    return(summary_data)
  })
  
  # Power Curve Plot for Tab 1
  output$powerCurvePlot <- renderPlot({
    plot_data <- power_analysis_data() %>%
      filter(N_Controls == input$power_nctrl) %>%
      mutate(
        Baseline_Label = factor(paste0(round(Baseline_Cover * 100), "%"), levels = paste0(sort(unique(round(Baseline_Cover*100))), "%")),
        Method = factor(Method, levels = survey_methods_params$Method)
      )
    
    validate(need(nrow(plot_data) > 0, "No data to plot. Please check selections."))
    
    ggplot(plot_data, aes(x = N_Transects, y = Power_Median, color = Baseline_Label, fill = Baseline_Label)) +
      geom_ribbon(aes(ymin = Power_Lower_CI, ymax = Power_Upper_CI), alpha = 0.2, linetype = 0) +
      geom_line(linewidth = 1.2) +
      geom_vline(xintercept = input$power_ntran, linetype = "dotted", color = "gray50") +
      geom_point(data = . %>% filter(N_Transects == input$power_ntran), size = 3) +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = "gray20") +
      facet_wrap(~Method, ncol = 2) +
      coord_cartesian(xlim = c(0, max_transects), ylim = c(0, 1), expand = FALSE) +
      scale_x_continuous(name = "Number of Transects per Site", breaks = scales::pretty_breaks(n=5)) +
      scale_y_continuous(labels = scales::percent, name = "Statistical Power") +
      scale_color_viridis_d(direction = -1, name = "Baseline Coral Cover") +
      scale_fill_viridis_d(direction = -1, name = "Baseline Coral Cover") +
      labs(
        title = paste0("Power to Detect a ", input$power_uplift_pct, "% Annual Uplift over ", input$power_nyears, " Years"),
        subtitle = paste("Assuming ", input$monitoring_frequency, " monitoring with 1 Treatment Site and ", input$power_nctrl, " Control Sites", sep="")
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", panel.spacing = unit(1.5, "lines"), strip.text = element_text(face = "bold", size = 12))
  })
  
  # Scenario Table for Tab 1
  output$scenarioTable <- renderDT({
    full_data <- power_analysis_data()
    
    cost_site <- req(input$cost_per_site_visit)
    nyears <- req(input$power_nyears)
    baselines <- as.numeric(input$power_baselines_pct) / 100
    
    table_n_ctrl_scenarios <- unique(sort(c(max(1, input$power_nctrl - 2), input$power_nctrl, input$power_nctrl + 2)))
    table_n_tran_scenarios <- unique(sort(c(max(1, input$power_ntran - 2), input$power_ntran, input$power_ntran + 2)))
    
    table_data <- full_data %>% filter(N_Controls %in% table_n_ctrl_scenarios, N_Transects %in% table_n_tran_scenarios)
    
    time_points <- switch(input$monitoring_frequency,
                          "Every 6 Months" = seq(0, nyears, by = 0.5),
                          "Annual" = seq(0, nyears, by = 1),
                          "Biennial" = unique(c(seq(0, nyears, by = 2), nyears)),
                          "Start & End Only" = c(0, nyears))
    
    n_surveys <- length(time_points)
    
    df_wide <- table_data %>%
      mutate(
        Cost = n_surveys * ((1 + N_Controls) * cost_site + (1 + N_Controls) * N_Transects * Cost_per_Transect),
        Power_Label = paste0(scales::percent(Power_Median, accuracy = 0.1), " <small>(", scales::percent(Power_Lower_CI, accuracy = 0.1), " - ", scales::percent(Power_Upper_CI, accuracy = 0.1), ")</small>")
      ) %>%
      select(Method, N_Controls, N_Transects, Baseline_Cover, Power_Median, Cost, Power_Label) %>%
      pivot_wider(names_from = Baseline_Cover, values_from = c(Power_Median, Power_Label), names_glue = "{.value}_{round(Baseline_Cover*100)}")
    
    baseline_cols_pct <- paste0(round(baselines * 100))
    power_cols <- paste0("Power_Median_", baseline_cols_pct)
    label_cols <- paste0("Power_Label_", baseline_cols_pct)
    header_names <- c("Method", "Control Sites", "Transects/Site", "Total Cost", paste0(baseline_cols_pct, "% Cover"))
    
    dt <- DT::datatable(df_wide, escape = FALSE, rownames = FALSE, colnames = header_names,
                        options = list(dom = 't', ordering = FALSE, columnDefs = list(list(visible=FALSE, targets=power_cols), list(targets = 3, render = JS("function(data, type, row, meta) { return '$' + data.toLocaleString(); }")))))
    
    for(i in seq_along(baselines)){
      dt <- dt %>% formatStyle(columns = label_cols[i], valueColumns = power_cols[i], backgroundColor = styleInterval(c(0.799), c('rgba(255, 99, 132, 0.2)', 'rgba(75, 192, 192, 0.2)')))
    }
    
    dt
  })
  
  
  # --- TAB 2 SERVER LOGIC ---
  
  # Reactive to store the simulation results
  simulation_results <- eventReactive(input$run_sim, {
    
    # --- 1. Set up simulation parameters ---
    n_sites <- 1 + input$sim_nctrl
    site_ids <- paste("Site", 1:n_sites)
    site_types <- c("Treatment", rep("Control", input$sim_nctrl))
    
    # Get method-specific precision
    method_params <- survey_methods_params %>% filter(Method == input$sim_method)
    sd_precision <- method_params$SD_Precision
    
    # Get other variability params
    sd_spatial <- 0.03 # Assuming a fixed spatial patchiness for simplicity
    sd_temporal <- 0.04 # Assuming a fixed temporal variation
    
    # --- 2. Generate "true" coral cover over time ---
    true_cover_df <- tibble(Year = 0:input$sim_nyears) %>%
      crossing(Site_ID = site_ids, Site_Type = site_types) %>%
      # Assign a random starting cover for each site
      group_by(Site_ID) %>%
      mutate(start_cover = rnorm(1, mean = 0.3, sd = 0.1)) %>%
      ungroup() %>%
      # Simulate annual growth and intervention effect
      mutate(
        annual_growth = rnorm(n(), 0.01, 0.005), # Small background growth with some noise
        uplift_effect = if_else(Site_Type == "Treatment" & Year >= input$sim_intervention_year, input$sim_uplift_pct / 100, 0),
        cumulative_growth = cumsum(annual_growth + uplift_effect),
        True_Cover = pmin(0.95, pmax(0.01, start_cover + cumulative_growth))
      )
    
    # --- 3. Apply shock event ---
    shock_loss <- input$sim_shock_magnitude / 100
    
    if (input$sim_shock_type != "No Shock") {
      shock_df <- true_cover_df %>%
        filter(Year >= input$sim_shock_year) %>%
        group_by(Site_ID) %>%
        mutate(
          shock_multiplier = case_when(
            input$sim_shock_type == "Cyclonic Impact (All sites)" ~ 1 - shock_loss,
            input$sim_shock_type == "Bleaching Event (Variable impact)" ~ 1 - (shock_loss * runif(1, 0.7, 1.3)),
            input$sim_shock_type == "Localized Impact (COTS)" & Site_ID %in% sample(site_ids, size = n_sites/2) ~ 1 - shock_loss,
            TRUE ~ 1
          ),
          True_Cover = True_Cover[Year == input$sim_shock_year] * shock_multiplier
        ) %>%
        ungroup()
      
      true_cover_df <- true_cover_df %>%
        filter(Year < input$sim_shock_year) %>%
        bind_rows(shock_df)
    }
    
    # --- 4. Simulate the observation process ---
    observed_data <- true_cover_df %>%
      crossing(Transect_ID = 1:input$sim_ntran) %>%
      mutate(
        total_sd = sqrt(sd_spatial^2 + sd_precision^2),
        Observed_Cover = rnorm(n(), mean = True_Cover, sd = total_sd)
      )
    
    # --- 5. Summarize for plotting ---
    summary_for_plot <- observed_data %>%
      group_by(Year, Site_Type) %>%
      summarise(
        Mean_Cover = mean(Observed_Cover),
        Lower_CI = quantile(Observed_Cover, 0.025),
        Upper_CI = quantile(Observed_Cover, 0.975),
        .groups = "drop"
      )
    
    return(summary_for_plot)
  })
  
  # --- Render the simulation plot ---
  output$simulationPlot <- renderPlot({
    plot_data <- simulation_results()
    
    ggplot(plot_data, aes(x = Year, y = Mean_Cover, color = Site_Type, fill = Site_Type)) +
      geom_vline(xintercept = input$sim_intervention_year, linetype = "dashed", color = "blue", linewidth = 1) +
      geom_vline(xintercept = if(input$sim_shock_type != "No Shock") input$sim_shock_year else NA, linetype = "dashed", color = "red", linewidth = 1) +
      geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, linetype = 0) +
      geom_line(linewidth = 1.2) +
      annotate("text", x = input$sim_intervention_year, y = 1, label = "Intervention", color = "blue", hjust = -0.1) +
      {
        if(input$sim_shock_type != "No Shock") {
          annotate("text", x = input$sim_shock_year, y = 0.95, label = "Shock Event", color = "red", hjust = -0.1)
        }
      } +
      scale_y_continuous(limits = c(0, 1), labels = scales::percent, name = "Coral Cover") +
      scale_color_manual(values = c("Treatment" = "darkorange", "Control" = "gray40")) +
      scale_fill_manual(values = c("Treatment" = "darkorange", "Control" = "gray40")) +
      labs(
        title = "Simulated Monitoring Program Under a Disturbance Scenario",
        subtitle = paste("Design:", input$sim_nctrl, "Control Sites,", input$sim_ntran, "Transects/Site,", "using", input$sim_method),
        color = "Site Type", fill = "Site Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
}

shinyApp(ui, server)