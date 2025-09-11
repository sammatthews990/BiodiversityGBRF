# app.R
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(bslib)
library(scales)
library(bsicons)

# Source the external file containing our statistical engine
source("baci_analysis_functions.R")

survey_methods_params <- tribble(
  ~Method,                   ~SD_Precision, ~Cost_per_Transect,
  "Benthic Photo Transects", 0.045,         50,
  "RHIS (Rapid Survey)",     0.120,         25,
  "Detailed Orthomosaic",    0.020,         200,
  "ReefScan (AI Towed)",     0.035,         35
)

# --- THEME DEFINITION ---
theme <- bs_theme(
  version = 5,
  base_font    = font_google("Inter", wght = c(400, 600, 700)),
  heading_font = font_google("Inter", wght = c(700)),
  primary = "#007bff",
  "table-cell-padding-y" = "0.2rem"
)

# --- UI Definition ---
ui <- page_navbar(
  title = "BACI Credit Simulator",
  theme = theme,
  
  tabPanel("BACI Analysis & Crediting",
           page_sidebar(
             sidebar = sidebar(
               width = "350px",
               tags$h4("Simulation Controls"),
               selectInput("sim_method", "Survey Method", choices = survey_methods_params$Method, selected = "Benthic Photo Transects"),
               sliderInput("sim_nctrl", "Number of Control Sites", min = 1, max = 10, value = 5, step = 1),
               sliderInput("sim_ntran", "Number of Transects per Site", min = 1, max = 10, value = 5, step = 1),
               tags$hr(),
               # CORRECTED: Monitoring duration now starts at 1 year
               sliderInput("sim_nyears", "Monitoring Duration (Years)", min = 1, max = 20, value = 10, step = 1),
               sliderInput("sim_intervention_year", "Intervention Start Year", min = 1, max = 20, value = 3, step = 1),
               numericInput("sim_uplift_pct", "True Annual Uplift Post-Intervention (%)", value = 5, min = 0, max = 20, step = 1),
               tags$hr(),
               selectInput("sim_shock_type", "Exogenous Shock Scenario",
                           choices = c("No Shock", "Cyclonic Impact (All sites)", "Bleaching Event (Variable impact)", "Localized Impact (COTS)")),
               sliderInput("sim_shock_year", "Shock Event Year", min = 1, max = 20, value = 7, step = 1),
               sliderInput("sim_shock_magnitude", "Shock Magnitude (% Coral Loss)", min = 0, max = 100, value = 50, step = 5),
               actionButton("run_sim", "Run Analysis", class = "btn-primary w-100", icon = icon("play"))
             ),
             
             layout_columns(
               col_widths = c(4, 4, 4),
               value_box(title = "Mean Uplift", value = textOutput("uplift_card"), showcase = bs_icon("graph-up-arrow")),
               value_box(title = "Probability of Real Uplift", value = textOutput("prob_card"), showcase = bs_icon("patch-check-fill")),
               value_box(title = "Final Credit Score", value = textOutput("credit_card"), showcase = bs_icon("award-fill"), theme_color = "success")
             ),
             
             layout_columns(
               col_widths = c(7, 5),
               card(
                 card_header("Simulated Coral Cover Trends"),
                 plotOutput("simulationPlot", height = "500px")
               ),
               card(
                 card_header("BACI Analysis Data"),
                 DTOutput("debugTable")
               )
             )
           )
  )
)

# --- Server Definition ---
server <- function(input, output, session) {
  
  # Observer to dynamically update sliders
  observeEvent(input$sim_nyears, {
    nyears <- input$sim_nyears
    updateSliderInput(session, "sim_intervention_year", max = nyears, value = min(input$sim_intervention_year, nyears))
    updateSliderInput(session, "sim_shock_year", max = nyears, value = min(input$sim_shock_year, nyears))
  })
  
  analysis_results <- eventReactive(input$run_sim, {
    method_params <- survey_methods_params %>% filter(Method == input$sim_method)
    sd_precision <- method_params$SD_Precision
    
    run_baci_analysis(
      n_sites_ctrl = input$sim_nctrl,
      n_transects = input$sim_ntran,
      n_years = input$sim_nyears,
      intervention_year = input$sim_intervention_year,
      true_uplift_pct = input$sim_uplift_pct,
      shock_type = input$sim_shock_type,
      shock_year = input$sim_shock_year,
      shock_magnitude_pct = input$sim_shock_magnitude,
      survey_precision_sd = sd_precision,
      spatial_patchiness_sd = 0.03,
      temporal_variation_sd = 0.04
    )
  })
  
  # --- Render Outputs ---
  output$uplift_card <- renderText({
    req(analysis_results())
    paste0(round(analysis_results()$mean_uplift * 100, 1), "%")
  })
  
  output$prob_card <- renderText({
    req(analysis_results())
    scales::percent(analysis_results()$prob_real_uplift, accuracy = 0.1)
  })
  
  output$credit_card <- renderText({
    req(analysis_results())
    round(analysis_results()$credit_score * 100, 1)
  })
  
  output$simulationPlot <- renderPlot({
    res <- analysis_results()
    plot_data <- res$plot_data
    
    # Base plot
    p <- ggplot(plot_data, aes(x = Year, y = Mean, color = Site_Type, fill = Site_Type)) +
      geom_vline(xintercept = input$sim_intervention_year, linetype = "dashed", color = "blue", linewidth = 1) +
      geom_ribbon(aes(ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, linetype = 0) +
      geom_line(linewidth = 1.2) +
      annotate("text", x = input$sim_intervention_year, y = 1, label = "Intervention", color = "blue", hjust = -0.1) +
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
    
    # CORRECTED: Conditionally add the shock vline and annotation
    if (input$sim_shock_type != "No Shock") {
      p <- p + 
        geom_vline(xintercept = input$sim_shock_year, linetype = "dashed", color = "red", linewidth = 1) +
        annotate("text", x = input$sim_shock_year, y = 0.95, label = "Shock Event", color = "red", hjust = -0.1)
    }
    
    p
  })
  
  output$debugTable <- renderDT({
    debug_data <- analysis_results()$debug_table
    
    DT::datatable(
      debug_data,
      rownames = FALSE,
      options = list(dom = 't', pageLength = 15, scrollX = TRUE),
      caption = "Mean cover and calculated change for each site."
    ) %>%
      formatPercentage(c("Before", "After", "Change"), digits = 1)
  })
}

shinyApp(ui, server)