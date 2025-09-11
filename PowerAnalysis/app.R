# app.R
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(DT)
library(bslib)

# ---- Default tables (same values you provided) ----
metrics_default <- tribble(
  ~ShortName, ~BaseMean, ~SD_Transects, ~SD_ReefsSites, ~SD_Accuracy, ~SD_Precision,
  "CorCov",   0.332,     0.056,         0.136,          0,            0,
  "CorDiv",   0.639,     0.045,         0.122,          0,            0,
  "HabVol",   0.665,     0.070,         0.102,          0,            0,
  "AlgInd",   0.251,     0.049,         0.161,          0,            0,
  "FishBiom", 0.700,     0.030,         0.039,          0,            0,
  "FishDiv",  0.610,     0.075,         0.086,          0,            0
)

sites <- tibble(
  SiteID      = c("PR1","CT1","CT2","CT3","CT4","CT5"),
  IsTreatment = c(TRUE,  FALSE,FALSE,FALSE,FALSE,FALSE),
  CTNum       = c(NA,    1,     2,     3,     4,     5)
)

metric_sets <- list(
  mC3 = c("CorCov","CorDiv","HabVol"),
  mC4 = c("CorCov","CorDiv","HabVol","AlgInd"),
  mC5 = c("CorCov","CorDiv","HabVol","AlgInd","FishBiom"),
  mC6 = c("CorCov","CorDiv","HabVol","AlgInd","FishBiom","FishDiv")
)

z_from_conf <- function(c) qnorm(1 - (1 - c)/2)

theme <- bs_theme(
  version = 5,
  base_font    = font_google("Inter", wght = c(400,600,700), local = FALSE),
  heading_font = font_google("Inter", wght = c(600,700),     local = FALSE)
)


# app.R
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(DT)
library(bslib)

# ---- Default tables (same values you provided) ----
metrics_default <- tribble(
  ~ShortName, ~BaseMean, ~SD_Transects, ~SD_ReefsSites, ~SD_Accuracy, ~SD_Precision,
  "CorCov",   0.332,     0.056,         0.136,          0,            0,
  "CorDiv",   0.639,     0.045,         0.122,          0,            0,
  "HabVol",   0.665,     0.070,         0.102,          0,            0,
  "AlgInd",   0.251,     0.049,         0.161,          0,            0,
  "FishBiom", 0.700,     0.030,         0.039,          0,            0,
  "FishDiv",  0.610,     0.075,         0.086,          0,            0
)

sites <- tibble(
  SiteID      = c("PR1","CT1","CT2","CT3","CT4","CT5"),
  IsTreatment = c(TRUE,  FALSE,FALSE,FALSE,FALSE,FALSE),
  CTNum       = c(NA,    1,     2,     3,     4,     5)
)

metric_sets <- list(
  mC3 = c("CorCov","CorDiv","HabVol"),
  mC4 = c("CorCov","CorDiv","HabVol","AlgInd"),
  mC5 = c("CorCov","CorDiv","HabVol","AlgInd","FishBiom"),
  mC6 = c("CorCov","CorDiv","HabVol","AlgInd","FishBiom","FishDiv")
)

z_from_conf <- function(c) qnorm(1 - (1 - c)/2)

theme <- bs_theme(
  version = 5,
  base_font    = font_google("Inter", wght = c(400,600,700), local = FALSE),
  heading_font = font_google("Inter", wght = c(600,700),     local = FALSE)
)


ui <- navbarPage(
  id = "nav",
  title = "GBR Simulator",
  theme = theme,
  header = tags$head(
    tags$style(HTML("
      :root{ --app-font: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto,
              'Helvetica Neue', Arial, 'Noto Sans', 'Liberation Sans', sans-serif; }
      body, .shiny-input-container, .table, .dataTables_wrapper { 
        font-family: var(--app-font) !important; font-weight: 400;
      }
      h1, h2, h3, h4, h5, h6, .h1, .h2, .h3, .h4, .h5, .h6, 
      b, strong, label, .card-title, .modal-title, 
      .dataTables_wrapper .dataTables_length label, 
      .dataTables_wrapper .dataTables_filter label {
        font-weight: 700 !important;
      }
    "))
  ),
  
  # --- NEW: User input tab (must be confirmed) ---
  tabPanel(
    "User input",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        tags$h4("Global assumptions"),
        numericInput("uplift", "Expected (true) uplift (0–1)", value = 0.08, min = 0, max = 1, step = 0.01),
        
        radioButtons("z_mode", "How to set SE multiplier (z):",
                     choices = c("Enter z directly" = "z",
                                 "Derive from confidence level" = "ci"),
                     selected = "ci"),
        conditionalPanel(
          "input.z_mode == 'z'",
          numericInput("zmult", "SE multiplier (z)", value = 1.96, min = 0, step = 0.01)
        ),
        conditionalPanel(
          "input.z_mode == 'ci'",
          selectInput("ci", "Confidence level for bands",
                      choices = c(0.80,0.90,0.95,0.975,0.99), selected = "0.95")
        ),
        
        numericInput("nctrl", "Number of control sites",
                     value = 3, min = 1, max = nrow(sites) - 1, step = 1),
        sliderInput("ntran", "Transects per site", min = 1, max = 6, value = 3, step = 1),
        sliderInput("nsims", "Simulations", min = 50, max = 500, value = 200, step = 50),
        
        tags$hr(),
        actionButton("confirm_inputs", "✅ Confirm & proceed", class = "btn-primary")
      ),
      mainPanel(
        tags$h4("Metric parameters (edit the green columns)"),
        DTOutput("metricsEdit"),
        tags$div(style="margin-top:8px;",
                 tags$small("Calculated columns (red) update automatically; base means and SDs are editable.")
        )
      )
    )
  ),
  
  tabPanel(
    "Results",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          "sets_results", "Composites to plot",
          choices  = names(metric_sets),
          selected = names(metric_sets)
        ),
        radioButtons(
          "avg_mode", "How to compute the blue bars:",
          choices  = c("Average over simulations (stable)" = "avg",
                       "Single simulated run (Excel-like)" = "one"),
          selected = "one"
        ),
        sliderInput("sim_pick", "Simulation to visualize (when using single run)",
                    min = 1, max = 50, value = 1, step = 1),
        numericInput("seed","Seed", value = 1234, min = 1, step = 1),
        helpText("Bars: Treatment (PR1) vs average of first N control sites.",
                 "Label shows uplift (PR1 − Controls). Green = detected (Δ > Z·SE).")
      ),
      mainPanel(
        plotOutput("upliftBars", height = 300),
        plotOutput("upliftSummary", height = 300),# NEW
        DT::DTOutput("upliftStats")
      )
    )
  ),
  
  # --- Data explorer (unchanged from your last version) ---
  tabPanel(
    "Data",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "view", "Choose data view:",
          c("Raw metric draws",
            "Composite replicates",
            "Means by site & metric",
            "Metric parameters"),
          selected = "Raw metric draws"
        ),
        downloadButton("downloadData", "Download CSV")
      ),
      mainPanel(DTOutput("dataTable"))
    )
  ),
  
  # --- Summary plot tab (from the previous step) ---
  tabPanel(
    "Summary plot",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        checkboxGroupInput(
          "sets_show", "Composites to display",
          choices  = names(metric_sets),
          selected = names(metric_sets)
        ),
        checkboxGroupInput(
          "sites_show", "Sites to plot",
          choices  = sites$SiteID,
          selected = sites$SiteID
        ),
        radioButtons(
          "se_mode", "SE bars use:",
          choices  = c("Transects only (match Excel)" = "transects",
                       "Sims × Transects"             = "sims_transects"),
          selected = "transects"
        )
      ),
      mainPanel(
        plotOutput("sitePlot", height = 380),
        DTOutput("siteStatsTable")
      )
    )
  )
)

server <- function(input, output, session){
  
  # --- Gate the app until confirmed ---
  confirmed <- reactiveVal(FALSE)
  
  observeEvent(input$confirm_inputs, {
    confirmed(TRUE)
    showNotification("Inputs confirmed. You can now use the other tabs.", type = "message")
    updateNavbarPage(session, "nav", selected = "Results")
  })
  
  observeEvent(input$nsims, {
    updateSliderInput(session, "sim_pick", max = input$nsims)
  })
  
  observeEvent(input$nav, {
    if (!confirmed() && input$nav != "User input") {
      showNotification("Please confirm 'User input' first.", type = "warning")
      updateNavbarPage(session, "nav", selected = "User input")
    }
  }, ignoreInit = TRUE)
  
  # --- Editable metrics table ---
  metrics_rv <- reactiveVal(metrics_default)
  
  metrics_display <- reactive({
    m <- metrics_rv()
    m %>%
      mutate(
        `Total variation at the transect level (SD of mean)` =
          sqrt(SD_Transects^2 + SD_Accuracy^2 + SD_Precision^2),
        `Total variation at transects, sites and reefs (SD of mean)` =
          sqrt(SD_ReefsSites^2 + SD_Accuracy^2 + SD_Precision^2)
      )
  })
  
  output$metricsEdit <- renderDT({
    df <- metrics_display()
    editable_cols <- c("BaseMean","SD_Transects","SD_ReefsSites","SD_Accuracy","SD_Precision")
    DT::datatable(
      df,
      rownames = FALSE,
      editable = list(target = "cell",
                      disable = list(columns = which(!(names(df) %in% editable_cols)))),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        columns = c("BaseMean","SD_Transects","SD_ReefsSites","SD_Accuracy","SD_Precision"),
        backgroundColor = "#e8f7e8"  # green-ish
      ) %>%
      formatStyle(
        columns = c("Total variation at the transect level (SD of mean)",
                    "Total variation at transects, sites and reefs (SD of mean)"),
        backgroundColor = "#fde2e2"  # red-ish
      )
  })
  
  observeEvent(input$metricsEdit_cell_edit, {
    info <- input$metricsEdit_cell_edit
    df <- metrics_display()
    colname <- names(df)[info$col]
    # Only update underlying if it is one of the editable sources
    if (colname %in% c("BaseMean","SD_Transects","SD_ReefsSites","SD_Accuracy","SD_Precision")) {
      base <- metrics_rv()
      row_id <- info$row
      val <- suppressWarnings(as.numeric(info$value))
      if (!is.na(val)) {
        base[row_id, colname] <- val
        metrics_rv(base)
      }
    }
    # re-render
    replaceData(dataTableProxy("metricsEdit"), metrics_display(), resetPaging = FALSE)
  })
  
  # Ensure #controls never exceeds available
  observe({
    updateNumericInput(session, "nctrl", max = nrow(sites) - 1)
  })
  
  # --- Use the user inputs everywhere else ---
  Z <- reactive({
    if (input$z_mode == "z") {
      as.numeric(input$zmult)
    } else {
      z_from_conf(as.numeric(input$ci))
    }
  })
  
  metrics_current <- reactive({
    metrics_rv() %>%
      mutate(SD_Combined_Transects = sqrt(SD_Transects^2 + SD_Accuracy^2 + SD_Precision^2))
  })
  
  # ---------- SIMULATION PIPELINE (same as before, but uses inputs above) ----------
  sim_data <- reactive({
    req(confirmed())
    set.seed(if (is.null(input$seed)) 1234 else input$seed)
    
    # current metric parameters (with SD_Combined_Transects)
    m <- metrics_current()   # ShortName, BaseMean, SD_ReefsSites, SD_Combined_Transects, ...
    
    # 1) SITE-LEVEL OFFSETS: one draw per (Sim, Site, Metric)
    site_offsets <-
      expand.grid(
        Sim    = seq_len(input$nsims),
        SiteID = sites$SiteID,
        Metric = m$ShortName,
        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
      ) |>
      dplyr::as_tibble() |>
      dplyr::left_join(m,    by = c("Metric" = "ShortName")) |>
      dplyr::left_join(sites, by = "SiteID") |>
      dplyr::mutate(
        # PR1 mean = expected uplift; CT sites mean = 0
        Offset = stats::rnorm(
          n(), 
          mean = ifelse(IsTreatment, input$uplift, 0),
          sd   = SD_ReefsSites
        )
      ) |>
      dplyr::select(Sim, SiteID, Metric, BaseMean, SD_Combined_Transects, Offset, IsTreatment)
    
    # 2) TRANSECT-LEVEL DRAWS around the site means
    expand.grid(
      Sim      = seq_len(input$nsims),
      Transect = seq_len(input$ntran),
      SiteID   = sites$SiteID,
      Metric   = m$ShortName,
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
    ) |>
      dplyr::as_tibble() |>
      dplyr::left_join(site_offsets, by = c("Sim","SiteID","Metric")) |>
      dplyr::mutate(
        Mean  = BaseMean + Offset,                      # site-shifted mean
        SD    = SD_Combined_Transects,                  # transect-level SD
        Value = pmin(1, pmax(0, stats::rnorm(n(), Mean, SD)))  # clamp to [0,1]
      ) |>
      dplyr::select(Sim, Transect, SiteID, Metric, BaseMean, Mean, SD, Value, IsTreatment)
  })
  
  # ---- Stats across ALL composite sets (for clustered bars) ----
  # Helper: site IDs of the first N controls (CT1, CT2, ...)
  control_ids <- reactive({
    sites %>% dplyr::filter(!IsTreatment) %>% dplyr::arrange(CTNum) %>%
      dplyr::slice_head(n = input$nctrl) %>% dplyr::pull(SiteID)
  })
  
  # Composite replicates for all sets at once (Sim × Transect × Site × Set)
  comp_by_site_all <- reactive({
    req(confirmed())
    sd <- sim_data()
    # If user wants a single run, keep only that Sim
    if (identical(input$avg_mode, "one")) {
      sd <- dplyr::filter(sd, Sim == input$sim_pick)
    }
    dplyr::bind_rows(lapply(names(metric_sets), function(snm) {
      mset <- metric_sets[[snm]]
      sd %>%
        dplyr::filter(Metric %in% mset) %>%
        dplyr::group_by(SiteID, Sim, Transect) %>%
        dplyr::summarise(Composite = mean(Value), .groups = "drop") %>%
        dplyr::mutate(Set = snm)
    }))
  })
  
  
  # Uplift summary per set: Treatment mean, ControlAvg, Delta, SE, Z*SE, Detected?
  uplift_summary_all <- reactive({
    df <- comp_by_site_all()
    if (!is.null(input$sets_results) && length(input$sets_results) > 0) {
      df <- dplyr::filter(df, Set %in% input$sets_results)
    }
    
    # Means per site & set (over Sim×Transect if avg; over Transect only if single run)
    means_site <- df %>%
      dplyr::group_by(Set, SiteID) %>%
      dplyr::summarise(Mean = mean(Composite), .groups = "drop")
    
    # Within-site variance per site & set
    within_site <- df %>%
      dplyr::group_by(Set, SiteID) %>%
      dplyr::summarise(VarWithin = stats::var(Composite), .groups = "drop")
    
    # Controls to include
    ctrl_ids <- sites %>%
      dplyr::filter(!IsTreatment) %>% dplyr::arrange(CTNum) %>%
      dplyr::slice_head(n = input$nctrl) %>% dplyr::pull(SiteID)
    
    # Among-site variance across control means (per set)
    among_by_set <- means_site %>%
      dplyr::filter(SiteID %in% ctrl_ids) %>%
      dplyr::group_by(Set) %>%
      dplyr::summarise(VarAmong = if (dplyr::n() > 1) stats::var(Mean) else 0, .groups = "drop")
    
    treat <- means_site %>% dplyr::filter(SiteID == "PR1") %>% dplyr::transmute(Set, Treat = Mean)
    control <- means_site %>% dplyr::filter(SiteID %in% ctrl_ids) %>%
      dplyr::group_by(Set) %>% dplyr::summarise(ControlAvg = mean(Mean), .groups = "drop")
    
    within_avg <- within_site %>% dplyr::group_by(Set) %>%
      dplyr::summarise(VarWithinAvg = mean(VarWithin), .groups = "drop")
    
    dplyr::left_join(treat, control, by = "Set") %>%
      dplyr::left_join(within_avg, by = "Set") %>%
      dplyr::left_join(among_by_set, by = "Set") %>%
      dplyr::mutate(
        Delta = Treat - ControlAvg,
        SE    = sqrt( (VarWithinAvg + VarAmong) / (input$nctrl * input$ntran) ),
        Band  = SE * Z(),
        Detected = Delta > Band
      )
  })
  
  # ---- Clustered bars + uplift label (green/red) ----
  output$upliftBars <- renderPlot({
    library(ggplot2)
    st <- uplift_summary_all()
    validate(need(nrow(st) > 0, "Select at least one composite."))
    
    bars <- tidyr::pivot_longer(st, cols = c("Treat","ControlAvg"),
                                names_to = "Group", values_to = "Mean") %>%
      dplyr::mutate(Group = dplyr::recode(Group,
                                          Treat = "Treatment (PR1)", ControlAvg = "Controls (avg)"))
    
    # position for the uplift label (just above the higher bar of the pair)
    lab_pos <- st %>%
      dplyr::mutate(y = pmax(Treat, ControlAvg) + 0.02,
                    label = sprintf("Δ = %.02f", Delta),
                    fill = ifelse(Detected, "#2ECC71", "#E74C3C"))  # green / red
    
    ggplot(bars, aes(x = Set, y = Mean, fill = Group)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_label(data = lab_pos,
                 aes(x = Set, y = y, label = label),
                 inherit.aes = FALSE, label.size = 0.25,
                 size = 3.8, label.padding = unit(0.15, "lines"),
                 fill = lab_pos$fill, color = "white") +
      scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = "Mean composite (proportion)", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })
  
  output$upliftSummary <- renderPlot({
    st <- uplift_summary_all()
    validate(need(nrow(st) > 0, "Select at least one composite."))
    
    band_label <- if (input$z_mode == "ci") paste0(as.numeric(input$ci)*100, "%")
    else paste0("z = ", round(Z(),2))
    mode_label <- if (identical(input$avg_mode, "one"))
      paste0("Single run (Sim ", input$sim_pick, ")")
    else "Average over simulations"
    
    ggplot(st, aes(x = Set)) +
      geom_col(aes(y = Delta), fill = "steelblue", alpha = 0.45, width = 0.6) +
      geom_line(aes(y = Band, group = 1), color = "orange", linewidth = 0.9) +
      geom_point(aes(y = Band), color = "orange", size = 2.2) +
      scale_y_continuous(limits = c(0, 0.1), expand = expansion(mult = c(0, 0.05))) +
      labs(
        x = NULL, y = "Uplift in reef condition (proportion)",
        subtitle = sprintf("Effect Size %.02f , Controls %d , Transects %d , Confidence Band %s — %s",
                           input$uplift, input$nctrl, input$ntran, band_label, mode_label)
      ) +
      theme_minimal(base_size = 12)
  })
  
  # Optional: a tidy table of the same stats (and whether it’s detected)
  output$upliftStats <- DT::renderDT({
    st <- uplift_summary_all() %>%
      dplyr::transmute(
        Composite = Set,
        Treatment = round(Treat, 2),
        Controls  = round(ControlAvg, 2),
        Uplift_Delta = round(Delta, 2),
        SE = signif(SE, 2),
        Band = signif(Band, 2),
        Detected = ifelse(Detected, "Yes", "No")
      )
    DT::datatable(st, rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  
  comp_by_site <- reactive({
    mset <- metric_sets[[input$set]]
    sim_data() %>%
      filter(Metric %in% mset) %>%
      group_by(SiteID, Sim, Transect) %>%
      summarise(Composite = mean(Value), .groups = "drop")
  })
  
  summary_tbl <- reactive({
    df_comp <- comp_by_site()
    within <- df_comp %>% group_by(SiteID) %>% summarise(VarWithin = var(Composite), .groups="drop")
    control_means <- df_comp %>%
      group_by(SiteID) %>% summarise(Mean = mean(Composite), .groups="drop") %>%
      filter(SiteID != "PR1") %>% arrange(SiteID) %>% slice_head(n = input$nctrl)
    var_among <- if (nrow(control_means) > 1) var(control_means$Mean) else 0
    se <- sqrt( (mean(within$VarWithin) + var_among) / (input$nctrl * input$ntran) )
    band <- se * Z()
    treat <- df_comp %>% filter(SiteID=="PR1") %>% summarise(Mean = mean(Composite)) %>% pull(Mean)
    ctrl  <- control_means %>% summarise(Mean = mean(Mean)) %>% pull(Mean)
    tibble(Composite=input$set, Treat=treat, ControlAvg=ctrl, Delta=treat-ctrl, SE=se,
           CI_low=(treat-ctrl)-band, CI_high=(treat-ctrl)+band)
  })
  
  # ---- Results tab outputs (unchanged) ----
  output$deltaPlot   <- renderPlot({ s <- summary_tbl(); 
  ggplot(s, aes(x=Composite, y=Delta)) +
    geom_col() +
    geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=0.12) +
    labs(y="Treatment − Control (mean composite)") +
    theme_minimal(base_size = 12) })
  
  output$meansTable  <- renderTable({
    comp_by_site() %>% group_by(SiteID) %>% summarise(MeanComposite=mean(Composite), .groups="drop") %>%
      pivot_wider(names_from=SiteID, values_from=MeanComposite) %>%
      mutate(Composite=input$set) %>% select(Composite, everything())
  })
  
  output$summaryTable <- renderTable({ summary_tbl() })
  
  # ---- Data tab outputs (unchanged) ----
  data_view <- reactive({
    switch(input$view,
           "Raw metric draws"        = sim_data(),
           "Composite replicates"    = comp_by_site(),
           "Means by site & metric"  = sim_data() %>% group_by(SiteID, Metric) %>% summarise(Mean = mean(Value), .groups="drop"),
           "Metric parameters"       = metrics_current()
    )
  })
  output$dataTable <- renderDT({
    datatable(data_view(), filter="top", rownames=FALSE,
              options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip", buttons=c("copy","csv","excel")),
              extensions="Buttons")
  }, server=TRUE)
  output$downloadData <- downloadHandler(
    filename=function(){ paste0(gsub(" ","_",tolower(input$view)),"_",Sys.Date(),".csv") },
    content=function(file){ write.csv(data_view(), file, row.names=FALSE) }
  )
  
  # Filter by site / set for the Summary plot tab
  comp_by_site_all_summary <- reactive({
    df <- comp_by_site_all()
    sel_sites <- if (is.null(input$sites_show) || !length(input$sites_show)) sites$SiteID else input$sites_show
    if (!is.null(input$sets_show) && length(input$sets_show)) {
      df <- dplyr::filter(df, Set %in% input$sets_show)
    }
    dplyr::filter(df, SiteID %in% sel_sites)
  })
  
  site_stats_summary <- reactive({
    df <- comp_by_site_all_summary()
    n_eff <- if (identical(input$se_mode, "transects")) input$ntran else input$nsims * input$ntran
    df %>%
      dplyr::group_by(Set, SiteID) %>%
      dplyr::summarise(
        Mean = mean(Composite),
        VarWithin = stats::var(Composite),
        SE = sqrt(VarWithin / n_eff),
        .groups = "drop"
      )
  })
  
  site_stats <- reactive({
    df <- comp_by_site_all()
    if (!is.null(input$sets_show) && length(input$sets_show)) df <- filter(df, Set %in% input$sets_show)
    n_eff <- if (identical(input$se_mode, "transects")) input$ntran else input$nsims * input$ntran
    df %>% group_by(Set, SiteID) %>%
      summarise(Mean = mean(Composite), VarWithin = var(Composite), SE = sqrt(VarWithin / n_eff), .groups="drop")
  })
  output$sitePlot <- renderPlot({
    st <- site_stats_summary()
    ggplot2::ggplot(st, ggplot2::aes(x = SiteID, y = Mean, fill = Set)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = pmax(0, Mean - SE), ymax = pmin(1, Mean + SE)),
        position = ggplot2::position_dodge(width = 0.8), width = 0.2
      ) +
      ggplot2::labs(x = "Sites (PR1 = project, CTx = controls)", y = "Mean Condition (±SE)") +
      ggplot2::theme_minimal(base_size = 12)
  })
  
  output$siteStatsTable <- DT::renderDT({
    st <- site_stats_summary()
    DT::datatable(
      tidyr::pivot_wider(st, names_from = SiteID, values_from = c(Mean, SE)),
      rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)


server <- function(input, output, session){
  
  # --- Gate the app until confirmed ---
  confirmed <- reactiveVal(FALSE)
  
  observeEvent(input$confirm_inputs, {
    confirmed(TRUE)
    showNotification("Inputs confirmed. You can now use the other tabs.", type = "message")
    updateNavbarPage(session, "nav", selected = "Results")
  })
  
  observeEvent(input$nsims, {
    updateSliderInput(session, "sim_pick", max = input$nsims)
  })
  
  observeEvent(input$nav, {
    if (!confirmed() && input$nav != "User input") {
      showNotification("Please confirm 'User input' first.", type = "warning")
      updateNavbarPage(session, "nav", selected = "User input")
    }
  }, ignoreInit = TRUE)
  
  # --- Use the user inputs everywhere else ---
  Z <- reactive({
    if (input$z_mode == "z") {
      as.numeric(input$zmult)
    } else {
      z_from_conf(as.numeric(input$ci))
    }
  })
  
  metrics_current <- reactive({
    metrics_rv() %>%
      mutate(SD_Combined_Transects = sqrt(SD_Transects^2 + SD_Accuracy^2 + SD_Precision^2))
  })
  
  # --- Editable metrics table ---
  metrics_rv <- reactiveVal(metrics_default)
  
  metrics_display <- reactive({
    m <- metrics_rv()
    m %>%
      mutate(
        `Total variation at the transect level (SD of mean)` =
          sqrt(SD_Transects^2 + SD_Accuracy^2 + SD_Precision^2),
        `Total variation at transects, sites and reefs (SD of mean)` =
          sqrt(SD_ReefsSites^2 + SD_Accuracy^2 + SD_Precision^2)
      )
  })
  
  output$metricsEdit <- renderDT({
    df <- metrics_display()
    editable_cols <- c("BaseMean","SD_Transects","SD_ReefsSites","SD_Accuracy","SD_Precision")
    DT::datatable(
      df,
      rownames = FALSE,
      editable = list(target = "cell",
                      disable = list(columns = which(!(names(df) %in% editable_cols)))),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        columns = c("BaseMean","SD_Transects","SD_ReefsSites","SD_Accuracy","SD_Precision"),
        backgroundColor = "#e8f7e8"  # green-ish
      ) %>%
      formatStyle(
        columns = c("Total variation at the transect level (SD of mean)",
                    "Total variation at transects, sites and reefs (SD of mean)"),
        backgroundColor = "#fde2e2"  # red-ish
      )
  })
  
  observeEvent(input$metricsEdit_cell_edit, {
    info <- input$metricsEdit_cell_edit
    df <- metrics_display()
    colname <- names(df)[info$col]
    # Only update underlying if it is one of the editable sources
    if (colname %in% c("BaseMean","SD_Transects","SD_ReefsSites","SD_Accuracy","SD_Precision")) {
      base <- metrics_rv()
      row_id <- info$row
      val <- suppressWarnings(as.numeric(info$value))
      if (!is.na(val)) {
        base[row_id, colname] <- val
        metrics_rv(base)
      }
    }
    # re-render
    replaceData(dataTableProxy("metricsEdit"), metrics_display(), resetPaging = FALSE)
  })
  
  # Ensure #controls never exceeds available
  observe({
    updateNumericInput(session, "nctrl", max = nrow(sites) - 1)
  })
  
  # --- Use the user inputs everywhere else ---
  Z <- reactive({
    if (input$z_mode == "z") {
      as.numeric(input$zmult)
    } else {
      z_from_conf(as.numeric(input$ci))
    }
  })
  
  metrics_current <- reactive({
    metrics_rv() %>%
      mutate(SD_Combined_Transects = sqrt(SD_Transects^2 + SD_Accuracy^2 + SD_Precision^2))
  })
  
  # ---------- SIMULATION PIPELINE (same as before, but uses inputs above) ----------
  sim_data <- reactive({
    req(confirmed())
    set.seed(if (is.null(input$seed)) 1234 else input$seed)
    
    # current metric parameters (with SD_Combined_Transects)
    m <- metrics_current()   # ShortName, BaseMean, SD_ReefsSites, SD_Combined_Transects, ...
    
    # 1) SITE-LEVEL OFFSETS: one draw per (Sim, Site, Metric)
    site_offsets <-
      expand.grid(
        Sim    = seq_len(input$nsims),
        SiteID = sites$SiteID,
        Metric = m$ShortName,
        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
      ) |>
      dplyr::as_tibble() |>
      dplyr::left_join(m,    by = c("Metric" = "ShortName")) |>
      dplyr::left_join(sites, by = "SiteID") |>
      dplyr::mutate(
        # PR1 mean = expected uplift; CT sites mean = 0
        Offset = stats::rnorm(
          n(), 
          mean = ifelse(IsTreatment, input$uplift, 0),
          sd   = SD_ReefsSites
        )
      ) |>
      dplyr::select(Sim, SiteID, Metric, BaseMean, SD_Combined_Transects, Offset, IsTreatment)
    
    # 2) TRANSECT-LEVEL DRAWS around the site means
    expand.grid(
      Sim      = seq_len(input$nsims),
      Transect = seq_len(input$ntran),
      SiteID   = sites$SiteID,
      Metric   = m$ShortName,
      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
    ) |>
      dplyr::as_tibble() |>
      dplyr::left_join(site_offsets, by = c("Sim","SiteID","Metric")) |>
      dplyr::mutate(
        Mean  = BaseMean + Offset,                      # site-shifted mean
        SD    = SD_Combined_Transects,                  # transect-level SD
        Value = pmin(1, pmax(0, stats::rnorm(n(), Mean, SD)))  # clamp to [0,1]
      ) |>
      dplyr::select(Sim, Transect, SiteID, Metric, BaseMean, Mean, SD, Value, IsTreatment)
  })
  
  # ---- Stats across ALL composite sets (for clustered bars) ----
  # Helper: site IDs of the first N controls (CT1, CT2, ...)
  control_ids <- reactive({
    sites %>% dplyr::filter(!IsTreatment) %>% dplyr::arrange(CTNum) %>%
      dplyr::slice_head(n = input$nctrl) %>% dplyr::pull(SiteID)
  })
  
  # Composite replicates for all sets at once (Sim × Transect × Site × Set)
  comp_by_site_all <- reactive({
    req(confirmed())
    sd <- sim_data()
    # If user wants a single run, keep only that Sim
    if (identical(input$avg_mode, "one")) {
      sd <- dplyr::filter(sd, Sim == input$sim_pick)
    }
    dplyr::bind_rows(lapply(names(metric_sets), function(snm) {
      mset <- metric_sets[[snm]]
      sd %>%
        dplyr::filter(Metric %in% mset) %>%
        dplyr::group_by(SiteID, Sim, Transect) %>%
        dplyr::summarise(Composite = mean(Value), .groups = "drop") %>%
        dplyr::mutate(Set = snm)
    }))
  })
  
  
  # Uplift summary per set: Treatment mean, ControlAvg, Delta, SE, Z*SE, Detected?
  uplift_summary_all <- reactive({
    df <- comp_by_site_all()
    if (!is.null(input$sets_results) && length(input$sets_results) > 0) {
      df <- dplyr::filter(df, Set %in% input$sets_results)
    }
    # Means per site & set (over Sim×Transect if avg; over Transect only if single run)
    means_site <- df %>%
      dplyr::group_by(Set, SiteID) %>%
      dplyr::summarise(Mean = mean(Composite), .groups = "drop")
    
    # Within-site variance per site & set
    within_site <- df %>%
      dplyr::group_by(Set, SiteID) %>%
      dplyr::summarise(VarWithin = stats::var(Composite), .groups = "drop")
    
    # Controls to include
    ctrl_ids <- sites %>%
      dplyr::filter(!IsTreatment) %>% dplyr::arrange(CTNum) %>%
      dplyr::slice_head(n = input$nctrl) %>% dplyr::pull(SiteID)
    
    # Among-site variance across control means (per set)
    among_by_set <- means_site %>%
      dplyr::filter(SiteID %in% ctrl_ids) %>%
      dplyr::group_by(Set) %>%
      dplyr::summarise(VarAmong = if (dplyr::n() > 1) stats::var(Mean) else 0, .groups = "drop")
    
    # These are averages across simulations. For a single run, var(Composite) is for that run only.
    # The 'VarWithinAvg' used in SE calculation should represent the typical within-site variability
    # observed across all simulations for a given set.
    # If avg_mode is "one", VarWithin and VarAmong are for that single run.
    # If avg_mode is "avg", VarWithin and VarAmong are aggregated over simulations.
    
    treat <- means_site %>% dplyr::filter(SiteID == "PR1") %>% dplyr::transmute(Set, Treat = Mean)
    control <- means_site %>% dplyr::filter(SiteID %in% ctrl_ids) %>%
      dplyr::group_by(Set) %>% dplyr::summarise(ControlAvg = mean(Mean), .groups = "drop")
    
    within_avg <- within_site %>% dplyr::group_by(Set) %>%
      dplyr::summarise(VarWithinAvg = mean(VarWithin), .groups = "drop")
    
    dplyr::left_join(treat, control, by = "Set") %>%
      dplyr::left_join(within_avg, by = "Set") %>%
      dplyr::left_join(among_by_set, by = "Set") %>%
      dplyr::mutate(
        Delta = Treat - ControlAvg,
        # SE calculation uses VarWithinAvg and VarAmong, and number of controls and transects
        # This SE is for the difference between Treatment mean and ControlAvg mean
        # When avg_mode = "one", these variances reflect a single run.
        # When avg_mode = "avg", these variances are averages over all simulations.
        SE    = sqrt( (VarWithinAvg + VarAmong) / (input$nctrl * input$ntran) ),
        Band  = SE * Z(),
        Detected = Delta > Band
      )
  })
  
  
  # ---- Clustered bars + uplift label (green/red) ----
  output$upliftBars <- renderPlot({
    library(ggplot2)
    st <- uplift_summary_all()
    validate(need(nrow(st) > 0, "Select at least one composite."))
    
    bars <- tidyr::pivot_longer(st, cols = c("Treat","ControlAvg"),
                                names_to = "Group", values_to = "Mean") %>%
      dplyr::mutate(Group = dplyr::recode(Group,
                                          Treat = "Treatment (PR1)", ControlAvg = "Controls (avg)"))
    
    # position for the uplift label (just above the higher bar of the pair)
    lab_pos <- st %>%
      dplyr::mutate(y = pmax(Treat, ControlAvg) + 0.02,
                    label = sprintf("Δ = %.02f", Delta),
                    fill = ifelse(Detected, "#2ECC71", "#E74C3C"))  # green / red
    
    ggplot(bars, aes(x = Set, y = Mean, fill = Group)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.6) +
      geom_label(data = lab_pos,
                 aes(x = Set, y = y, label = label),
                 inherit.aes = FALSE, label.size = 0.25,
                 size = 3.8, label.padding = unit(0.15, "lines"),
                 fill = lab_pos$fill, color = "white") +
      scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
      labs(x = NULL, y = "Mean composite (proportion)", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })
  
  output$upliftSummary <- renderPlot({
    st <- uplift_summary_all()
    validate(need(nrow(st) > 0, "Select at least one composite."))
    
    band_label <- if (input$z_mode == "ci") paste0(as.numeric(input$ci)*100, "%")
    else paste0("z = ", round(Z(),2))
    mode_label <- if (identical(input$avg_mode, "one"))
      paste0("Single run (Sim ", input$sim_pick, ")")
    else "Average over simulations"
    
    ggplot(st, aes(x = Set)) +
      geom_col(aes(y = Delta), fill = "steelblue", alpha = 0.45, width = 0.6) +
      geom_line(aes(y = Band, group = 1), color = "orange", linewidth = 0.9) +
      geom_point(aes(y = Band), color = "orange", size = 2.2) +
      scale_y_continuous(limits = c(0, 0.1), expand = expansion(mult = c(0, 0.05))) +
      labs(
        x = NULL, y = "Uplift in reef condition (proportion)",
        subtitle = sprintf("Effect Size %.02f , Controls %d , Transects %d , Confidence Band %s — %s",
                           input$uplift, input$nctrl, input$ntran, band_label, mode_label)
      ) +
      theme_minimal(base_size = 12)
  })
  
  # Optional: a tidy table of the same stats (and whether it’s detected)
  output$upliftStats <- DT::renderDT({
    st <- uplift_summary_all() %>%
      dplyr::transmute(
        Composite = Set,
        Treatment = round(Treat, 2),
        Controls  = round(ControlAvg, 2),
        Uplift_Delta = round(Delta, 2),
        SE = signif(SE, 2),
        Band = signif(Band, 2),
        Detected = ifelse(Detected, "Yes", "No")
      )
    DT::datatable(st, rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  
  comp_by_site <- reactive({
    mset <- metric_sets[[input$set]]
    sim_data() %>%
      filter(Metric %in% mset) %>%
      group_by(SiteID, Sim, Transect) %>%
      summarise(Composite = mean(Value), .groups = "drop")
  })
  
  summary_tbl <- reactive({
    df_comp <- comp_by_site()
    within <- df_comp %>% group_by(SiteID) %>% summarise(VarWithin = var(Composite), .groups="drop")
    control_means <- df_comp %>%
      group_by(SiteID) %>% summarise(Mean = mean(Composite), .groups="drop") %>%
      filter(SiteID != "PR1") %>% arrange(SiteID) %>% slice_head(n = input$nctrl)
    var_among <- if (nrow(control_means) > 1) var(control_means$Mean) else 0
    se <- sqrt( (mean(within$VarWithin) + var_among) / (input$nctrl * input$ntran) )
    band <- se * Z()
    treat <- df_comp %>% filter(SiteID=="PR1") %>% summarise(Mean = mean(Composite)) %>% pull(Mean)
    ctrl  <- control_means %>% summarise(Mean = mean(Mean)) %>% pull(Mean)
    tibble(Composite=input$set, Treat=treat, ControlAvg=ctrl, Delta=treat-ctrl, SE=se,
           CI_low=(treat-ctrl)-band, CI_high=(treat-ctrl)+band)
  })
  
  # ---- Clustered bars + uplift label (green/red) ----
  # ... (output$upliftBars, output$upliftSummary, output$upliftStats are unchanged) ...
  
  
  # Filter by site / set for the Summary plot tab
  comp_by_site_all_summary <- reactive({
    df <- comp_by_site_all()
    sel_sites <- if (is.null(input$sites_show) || !length(input$sites_show)) sites$SiteID else input$sites_show
    if (!is.null(input$sets_show) && length(input$sets_show)) {
      df <- dplyr::filter(df, Set %in% input$sets_show)
    }
    dplyr::filter(df, SiteID %in% sel_sites)
  })
  
  site_stats_summary <- reactive({
    df <- comp_by_site_all_summary()
    # n_eff here is related to how many 'observations' contribute to the mean
    # If we are averaging over simulations (avg_mode = "avg"), then the effective n for within-site variance
    # is input$nsims * input$ntran. If it's a single run, it's just input$ntran.
    n_eff <- if (identical(input$avg_mode, "one")) input$ntran else input$nsims * input$ntran
    
    df %>%
      dplyr::group_by(Set, SiteID) %>%
      dplyr::summarise(
        Mean = mean(Composite),
        VarWithin = stats::var(Composite),
        SE = sqrt(VarWithin / n_eff), # SE of the mean composite for a given site & set
        .groups = "drop"
      )
  })
  
  output$sitePlot <- renderPlot({
    st <- site_stats_summary()
    validate(need(nrow(st) > 0, "Select at least one composite and site for the plot."))
    
    # Adjusting ymin/ymax to ensure bars don't go below 0 or above 1
    ggplot2::ggplot(st, ggplot2::aes(x = SiteID, y = Mean, fill = Set)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = pmax(0, Mean - SE), ymax = pmin(1, Mean + SE)),
        position = ggplot2::position_dodge(width = 0.8), width = 0.2
      ) +
      ggplot2::labs(x = "Sites (PR1 = project, CTx = controls)", y = "Mean Condition (±SE)") +
      ggplot2::scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "top")
  })
  
  # ---- Results tab outputs (unchanged) ----
  output$deltaPlot   <- renderPlot({ s <- summary_tbl(); 
  ggplot(s, aes(x=Composite, y=Delta)) +
    geom_col() +
    geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=0.12) +
    labs(y="Treatment − Control (mean composite)") +
    theme_minimal(base_size = 12) })
  
  output$meansTable  <- renderTable({
    comp_by_site() %>% group_by(SiteID) %>% summarise(MeanComposite=mean(Composite), .groups="drop") %>%
      pivot_wider(names_from=SiteID, values_from=MeanComposite) %>%
      mutate(Composite=input$set) %>% select(Composite, everything())
  })
  
  output$summaryTable <- renderTable({ summary_tbl() })
  
  # ---- Data tab outputs (unchanged) ----
  data_view <- reactive({
    switch(input$view,
           "Raw metric draws"        = sim_data(),
           "Composite replicates"    = comp_by_site(),
           "Means by site & metric"  = sim_data() %>% group_by(SiteID, Metric) %>% summarise(Mean = mean(Value), .groups="drop"),
           "Metric parameters"       = metrics_current()
    )
  })
  output$dataTable <- renderDT({
    datatable(data_view(), filter="top", rownames=FALSE,
              options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip", buttons=c("copy","csv","excel")),
              extensions="Buttons")
  }, server=TRUE)
  output$downloadData <- downloadHandler(
    filename=function(){ paste0(gsub(" ","_",tolower(input$view)),"_",Sys.Date(),".csv") },
    content=function(file){ write.csv(data_view(), file, row.names=FALSE) }
  )
  
  # Filter by site / set for the Summary plot tab
  comp_by_site_all_summary <- reactive({
    df <- comp_by_site_all()
    sel_sites <- if (is.null(input$sites_show) || !length(input$sites_show)) sites$SiteID else input$sites_show
    if (!is.null(input$sets_show) && length(input$sets_show)) {
      df <- dplyr::filter(df, Set %in% input$sets_show)
    }
    dplyr::filter(df, SiteID %in% sel_sites)
  })
  
  site_stats_summary <- reactive({
    df <- comp_by_site_all_summary()
    n_eff <- if (identical(input$se_mode, "transects")) input$ntran else input$nsims * input$ntran
    df %>%
      dplyr::group_by(Set, SiteID) %>%
      dplyr::summarise(
        Mean = mean(Composite),
        VarWithin = stats::var(Composite),
        SE = sqrt(VarWithin / n_eff),
        .groups = "drop"
      )
  })
  
  site_stats <- reactive({
    df <- comp_by_site_all()
    if (!is.null(input$sets_show) && length(input$sets_show)) df <- filter(df, Set %in% input$sets_show)
    n_eff <- if (identical(input$se_mode, "transects")) input$ntran else input$nsims * input$ntran
    df %>% group_by(Set, SiteID) %>%
      summarise(Mean = mean(Composite), VarWithin = var(Composite), SE = sqrt(VarWithin / n_eff), .groups="drop")
  })
  output$sitePlot <- renderPlot({
    st <- site_stats_summary()
    ggplot2::ggplot(st, ggplot2::aes(x = SiteID, y = Mean, fill = Set)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = pmax(0, Mean - SE), ymax = pmin(1, Mean + SE)),
        position = ggplot2::position_dodge(width = 0.8), width = 0.2
      ) +
      ggplot2::labs(x = "Sites (PR1 = project, CTx = controls)", y = "Mean Condition (±SE)") +
      ggplot2::theme_minimal(base_size = 12)
  })
  
  output$siteStatsTable <- DT::renderDT({
    st <- site_stats_summary()
    DT::datatable(
      tidyr::pivot_wider(st, names_from = SiteID, values_from = c(Mean, SE)),
      rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  
  power_analysis_data <- reactive({
    req(confirmed())
    req(input$target_uplift_pct)
    
    target_delta <- input$target_uplift_pct / 100 # Convert percentage to proportion
    z_val <- Z()
    n_ctrl <- input$nctrl
    
    # Use the first metric's parameters for simplicity in this general power analysis,
    # or consider averaging/picking a representative one. Let's pick 'CorCov' as it's often key.
    # For a more advanced version, you might iterate through metric_sets or let the user choose.
    relevant_metric_params <- metrics_current() %>%
      dplyr::filter(ShortName == "CorCov") %>% # Focusing on CorCov for this example
      # If CorCov isn't available, pick the first one
      # If you want to generalize, average SDs or let user select a 'representative' metric
      dplyr::slice(1)
    
    validate(need(nrow(relevant_metric_params) > 0, "Metric parameters not found for power analysis."))
    
    # These variances are *per transect* per site, and among-site reef/site level
    sd_within_transect <- relevant_metric_params$SD_Combined_Transects[1] # For transect-level variability
    sd_among_reefsites <- relevant_metric_params$SD_ReefsSites[1] # For among-site variability
    
    # Ranges for iteration
    ntran_seq <- seq(input$ntran_range_power[1], input$ntran_range_power[2], by = 1)
    basemean_seq <- seq(input$basemean_range_power[1], input$basemean_range_power[2], by = 0.01)
    
    # Pre-calculate expected variances
    # The within-site variance of the *composite mean* for a given site and N transects
    # is SD_Combined_Transects^2 / Ntran.
    # The among-site variance of the *site mean* (due to reef/site effects) is SD_ReefsSites^2.
    # The variance of the *average of control sites* is (VarWithinAvg + VarAmong) / (N_ctrl * N_tran)
    
    # Simplified variances for a single metric, assuming 1 control site for among-site variance context
    # This calculation assumes that the "composite" for power analysis is just this single metric
    # and that its SDs represent the overall variability.
    expand_grid(
      N_Transects_Per_Site = ntran_seq,
      Baseline_Mean_Coral_Cover = basemean_seq
    ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        # VarWithin for the *average* composite over N_Transects_Per_Site
        # This is the variance of a single site's mean composite across its transects
        VarWithin_for_n_transects = sd_within_transect^2 / N_Transects_Per_Site,
        
        # The among-site variance (between treatment and control means)
        # This is the variance of the site-level effect.
        # This component doesn't scale with N_Transects_Per_Site directly
        # but with N_Controls if we are looking at the average of controls.
        # Here we consider the variance of the *difference* between two site means (Treatment vs Control Average)
        # Variance of a single site mean from site-level effects: sd_among_reefsites^2
        # Variance of the average of N_ctrl sites: sd_among_reefsites^2 / n_ctrl
        # Total variance of the difference: (VarWithin_for_n_transects + sd_among_reefsites^2) * (1/1 + 1/n_ctrl)
        # However, your SE calculation uses (VarWithinAvg + VarAmong) / (input$nctrl * input$ntran)
        # Let's align with that structure as much as possible for consistency.
        
        # The term VarWithinAvg in your uplift_summary_all is actually
        # the mean of the VarWithin (composite variance per site per set) over all sites
        # The VarAmong is the variance of the means of controls.
        
        # For power analysis, we need the SE of the difference (Treatment - ControlAvg)
        # SE = sqrt( (VarWithin_per_site / N_Transects_Per_Site + VarAmong_sites) / N_control_sites )
        # A more direct SE of difference between two means (treatment vs average of controls)
        # SE_diff = sqrt( Var(Treat_Mean) + Var(Control_Avg_Mean) )
        # Var(Treat_Mean) = (sd_within_transect^2 / N_Transects_Per_Site) + sd_among_reefsites^2
        # Var(Control_Avg_Mean) = ((sd_within_transect^2 / N_Transects_Per_Site) + sd_among_reefsites^2) / n_ctrl
        
        # This simpler interpretation is that the SDs are for the base metric.
        # When compositing, the variance is usually lower.
        # For a single metric, the variance of the mean of N transects at a site is (SD_Combined_Transects^2 / Ntran)
        # plus the site-level variance (SD_ReefsSites^2).
        
        # The SE of the *difference* between the treatment mean and the average of control means is:
        # SE_diff = sqrt( (SD_Combined_Transects^2 / N_Transects_Per_Site + SD_ReefsSites^2) * (1/1 + 1/n_ctrl) )
        
        # Let's use the structure from uplift_summary_all for consistency:
        # SE = sqrt( (VarWithinAvg + VarAmong) / (N_control_sites * N_Transects_Per_Site) ) is problematic
        # because it treats VarAmong as a total variance, not per site.
        
        # Re-evaluating the `uplift_summary_all`'s SE:
        # SE = sqrt( (VarWithinAvg + VarAmong) / (input$nctrl * input$ntran) )
        # This formula is effectively:
        # SE_diff = sqrt( Var(Treatment_Mean) + Var(Control_Average_Mean) )
        # Where:
        # Var(Site_Mean) = (VarWithin_per_transect / N_Transects_Per_Site) + VarAmong_site_level
        # The `VarWithinAvg` in your code is `mean(var(Composite))` which is
        # the *sample variance* of the composite values *within a site*.
        # Var(Composite) = `SD_Combined_Transects^2` at the transect level.
        # So `VarWithinAvg` effectively estimates `SD_Combined_Transects^2`.
        # And `VarAmong` estimates `SD_ReefsSites^2`.
        
        # So, your current SE formula seems to imply:
        # SE_diff = sqrt( (SD_Combined_Transects^2 + SD_ReefsSites^2) / (n_ctrl * N_Transects_Per_Site) )
        # Let's use this interpretation for the power analysis for consistency with current code.
        # This is a simplification as it doesn't separate the variance contributions perfectly for the average of controls.
        
        # A more standard formula for the SE of the difference between treatment (1 site) and average of controls (N_ctrl sites):
        # Var_Treat_Mean = (sd_within_transect^2 / N_Transects_Per_Site) + sd_among_reefsites^2
        # Var_Control_Avg_Mean = ((sd_within_transect^2 / N_Transects_Per_Site) + sd_among_reefsites^2) / n_ctrl
        # SE_diff_standard = sqrt(Var_Treat_Mean + Var_Control_Avg_Mean)
        # SE_calc = sqrt( (sd_within_transect^2 / N_Transects_Per_Site + sd_among_reefsites^2) * (1 + 1/n_ctrl) )
        
        # Given your existing SE calculation:
        # SE = sqrt( (VarWithinAvg + VarAmong) / (input$nctrl * input$ntran) )
        # If we assume VarWithinAvg ~ SD_Combined_Transects^2 and VarAmong ~ SD_ReefsSites^2
        # Then, SE ~ sqrt( (SD_Combined_Transects^2 + SD_ReefsSites^2) / (n_ctrl * N_Transects_Per_Site) )
        # Let's use this for consistency in the power calculation.
        
        SE_estimated = sqrt( (sd_within_transect^2 + sd_among_reefsites^2) / (n_ctrl * N_Transects_Per_Site) ),
        # Condition for detection: target_delta > Z * SE_estimated
        Detectable = target_delta > z_val * SE_estimated
      ) %>%
      dplyr::ungroup()
  })
  
  output$powerPlot <- renderPlot({
    req(power_analysis_data())
    df_power <- power_analysis_data()
    
    # Filter for the relevant metric (e.g., CorCov) or provide option for composite sets
    # For now, this plot is focused on a single metric (CorCov as chosen in power_analysis_data)
    # To extend to composites, we'd need to consider composite-specific variances.
    
    ggplot(df_power, aes(x = Baseline_Mean_Coral_Cover, y = N_Transects_Per_Site)) +
      geom_tile(aes(fill = Detectable), alpha = 0.8) +
      scale_fill_manual(
        values = c("TRUE" = "#2ECC71", "FALSE" = "#E74C3C"), # Green for detectable, red for not
        labels = c("Detectable", "Not Detectable"),
        name = paste0("Uplift of ", input$target_uplift_pct, "% Detectable")
      ) +
      labs(
        title = paste0(
          "Transects Needed to Detect ", input$target_uplift_pct,
          "% Uplift (with ", round(as.numeric(input$ci)*100), "% Confidence)"
        ),
        x = "Baseline Mean Coral Cover (Proportion)",
        y = "Number of Transects Per Site"
      ) +
      scale_x_continuous(limits = input$basemean_range_power, expand = c(0,0)) +
      scale_y_continuous(
        breaks = unique(df_power$N_Transects_Per_Site),
        limits = input$ntran_range_power, expand = c(0,0)
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
  })
  
  
  # OLD: output$siteStatsTable is replaced by output$powerPlot
  # output$siteStatsTable <- DT::renderDT({
  #   st <- site_stats_summary()
  #   DT::datatable(
  #     tidyr::pivot_wider(st, names_from = SiteID, values_from = c(Mean, SE)),
  #     rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)
  #   )
  # })
}

}

shinyApp(ui, server)
