# ---
# Compare BACI power: App-style (fast MC) vs epower (supplyData -> powerScenario)
# Minimal one-method example; BACI step-change effect for alignment with epower.
# ---

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(ggplot2)
})

# Install/load epower ----------------------------------------------------------
if (!requireNamespace("epower", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("bmtglobal/epower")
}
library(epower)

# Helpers ----------------------------------------------------------------------
dynamic_spatial_sd <- function(p, anchor_sd = 0.05, anchor_p = 0.5) {
  scale <- sqrt(p*(1 - p)) / sqrt(anchor_p * (1 - anchor_p))
  pmax(1e-8, anchor_sd * scale)
}

# App-style BACI (fast MC), step-change effect (delta applied after T0) --------
power_baci_appstyle_step <- function(delta_step        = 0.15,  # +15% step after T0
                                     T_before          = 1,     # one "Before" time
                                     T_after           = 5,     # five "After" times
                                     n_controls        = 5,
                                     n_transects       = 10,
                                     baseline_mean     = 0.30,
                                     sd_precision      = 0.040, # per-transect
                                     sd_spatial_peak   = 0.050, # at 50% cover
                                     sd_temporal       = 0.040, # year-to-year
                                     nsims             = 2000,
                                     seed              = 123) {
  set.seed(seed)
  times <- c(rep("Before", T_before), rep("After", T_after))
  Tn <- length(times)
  
  # variance components for site-year mean
  sd_spatial <- dynamic_spatial_sd(baseline_mean, sd_spatial_peak)
  sd_site_year <- sqrt((sd_precision^2 + sd_spatial^2)/n_transects + sd_temporal^2)
  
  sim_once <- function() {
    mu_ctrl <- rep(baseline_mean, Tn)
    mu_imp  <- baseline_mean + ifelse(times == "After", delta_step, 0)
    
    # average across sites (1 impact location, n_controls control locations)
    y_ctrl <- mu_ctrl + rnorm(Tn, 0, sd_site_year / sqrt(n_controls))
    y_imp  <- mu_imp  + rnorm(Tn, 0, sd_site_year / sqrt(1))
    
    dat <- rbind(
      data.frame(time = seq_len(Tn), y = y_ctrl, BvA = factor(times, c("Before","After")), CvI = factor("Control", c("Control","Impact"))),
      data.frame(time = seq_len(Tn), y = y_imp,  BvA = factor(times, c("Before","After")), CvI = factor("Impact",  c("Control","Impact")))
    )
    # Classical BACI contrast: After×Impact
    fit <- lm(y ~ BvA * CvI, data = dat)
    b   <- coef(summary(fit))["BvAAfter:CvIImpact","Estimate"]
    se  <- coef(summary(fit))["BvAAfter:CvIImpact","Std. Error"]
    as.integer(b > 0 && abs(b/se) > qnorm(0.975))  # two-sided 5%
  }
  
  power <- mean(replicate(nsims, sim_once()))
  tibble(power_app = power,
         sd_site_year = sd_site_year,
         sd_spatial = sd_spatial,
         sd_precision = sd_precision)
}

# Construct minimal pilot data for epower --------------------------------------
# epower wants a "pilot" long table with:
# Response, Trials (set NA for gaussian), Location, sublocation, Time, subtime, BvA, CvI.
make_pilot_gaussian <- function(baseline_mean    = 0.30,
                                sd_precision     = 0.040,
                                sd_spatial_peak  = 0.050,
                                sd_temporal      = 0.040,
                                n_controls       = 5,
                                n_transects      = 10,
                                T_before         = 2,
                                T_after          = 5) {
  
  set.seed(1)
  times <- c(rep("Before", T_before), rep("After", T_after))
  Tn <- length(times)
  
  dynamic_spatial_sd <- function(p, anchor_sd = 0.05, anchor_p = 0.5) {
    scale <- sqrt(p*(1-p)) / sqrt(anchor_p*(1-anchor_p))
    pmax(1e-8, anchor_sd * scale)
  }
  sd_spatial <- dynamic_spatial_sd(baseline_mean, sd_spatial_peak)
  sd_rep <- sqrt(sd_precision^2 + sd_spatial^2)
  
  ctrl_df <- expand.grid(
    CvI = "Control",
    Location = paste0("C", seq_len(n_controls)),
    sublocation = "sub1",              # <-- string, not NA
    Time = seq_len(Tn),
    subtime = "sub1",                  # <-- string, not NA
    BvA = times,
    replicate = seq_len(n_transects)
  )
  imp_df <- expand.grid(
    CvI = "Impact",
    Location = "I1",
    sublocation = "sub1",
    Time = seq_len(Tn),
    subtime = "sub1",
    BvA = times,
    replicate = seq_len(n_transects)
  )
  
  df <- dplyr::bind_rows(ctrl_df, imp_df) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      Response = rnorm(dplyr::n(), mean = baseline_mean, sd = sd_rep),
      Trials = NA_real_,
      BvA = factor(BvA, levels = c("Before","After")),
      CvI = factor(CvI, levels = c("Control","Impact"))
    ) |>
    dplyr::select(Response, Trials, Location, sublocation, Time, subtime, BvA, CvI)
  
  df
}

run_epower_supply <- function(dat,
                              n_controls, n_transects,
                              T_before = 2, T_after = 5,
                              delta_step = 0.15,        # multiplicative step (e.g., +0.15)
                              nsims = 200,
                              prefix = "epower_run") {
  
  ## --- defensives on pilot ---
  if (!"Trials" %in% names(dat) || any(is.na(dat$Trials))) dat$Trials <- 1L
  if (!"sublocation" %in% names(dat)) dat$sublocation <- "sub1"
  if (!"subtime" %in% names(dat))     dat$subtime     <- "sub1"
  
  # interface lists for supplyData (still useful to get posterior etc.)
  design.matrix <- list(
    Response    = "Response", Trials = "Trials",
    Location    = "Location", sublocation = "sublocation",
    Time        = "Time",     subtime     = "subtime",
    BvA         = "BvA",      CvI         = "CvI"
  )
  levels.dat <- list(Before="Before", Control="Control", After="After", Impact="Impact")
  scenario.data <- list(
    Number.of.iterations                  = as.character(nsims),
    filename                              = sprintf("%s_C%02d_T%02d", prefix, n_controls, n_transects),
    Number.of.Impact.Locations            = "1",
    Number.of.Control.Locations           = as.character(n_controls),
    Number.of.sublocations.within.Location= "1",
    Number.of.sample.times.Before         = as.character(T_before),
    Number.of.sample.times.After          = as.character(T_after),
    Number.of.subtimes.within.Time        = "1",
    Number.of.trials                      = "1",
    Number.of.replicate.measurements      = as.character(n_transects)
  )
  effect.info <- list(Multiplicative = 1, Fixed.change = 0,
                      Effect.values  = sprintf("%0.3f", delta_step))
  
  # 1) Use supplyData() to fit pilot & get posterior bits scenarioParams needs
  dc <- epower::supplyData(
    dat           = dat,
    variableType  = "gaussian",
    design.matrix = design.matrix,
    levels.dat    = levels.dat,
    scenario.data = scenario.data,
    effect.info   = effect.info,
    ncores        = 1
  )
  if (is.null(dc$pilot$global.mean) || is.na(dc$pilot$global.mean)) {
    dc$pilot$global.mean <- mean(dat$Response, na.rm = TRUE)
  }
  
  # 2) Build scenarioParams via powerScenario()
  sp <- epower::powerScenario(dc)
  
  # 3) Hard-set the fields run.scenario() actually reads (avoid NA parsing issues)
  sp$dat                 <- dc$dat
  sp$effect.type         <- if (effect.info$Multiplicative == 1) "Multiplicative" else "Fixed"
  sp$variableType        <- dc$variableType
  sp$n.its               <- as.numeric(scenario.data$Number.of.iterations)
  # Ensure these are numeric scalars or vectors (no NA)
  sp$locations.impact    <- as.numeric(scenario.data$Number.of.Impact.Locations)
  sp$locations.control   <- as.numeric(scenario.data$Number.of.Control.Locations)
  sp$times.before        <- as.numeric(scenario.data$Number.of.sample.times.Before)
  sp$times.after         <- as.numeric(scenario.data$Number.of.sample.times.After)
  sp$replicates          <- as.numeric(scenario.data$Number.of.replicate.measurements)
  sp$sublocations.within.locations <- as.numeric(scenario.data$Number.of.sublocations.within.Location)
  sp$subtimes.within.times         <- as.numeric(scenario.data$Number.of.subtimes.within.Time)
  sp$trials              <- as.numeric(scenario.data$Number.of.trials)
  # Model pieces already populated by powerScenario(): random.structure, fixed.sample,
  # fixed.levels, hyperpar.sample, post.sample, mod1.formula.use, etc.
  
  # 4) Construct the scenario matrix explicitly (single scenario here; extend as needed)
  scenario.matrix <- data.frame(
    locations.impact             = sp$locations.impact,
    locations.control            = sp$locations.control,
    times.before                 = sp$times.before,
    times.after                  = sp$times.after,
    replicates                   = sp$replicates,
    sublocations.within.locations= sp$sublocations.within.locations,
    subtimes.within.times        = sp$subtimes.within.times,
    trials                       = sp$trials,
    effect                       = delta_step,
    stringsAsFactors = FALSE
  )
  
  # sanity-check: no NA in the row we will pass to run.scenario
  if (anyNA(scenario.matrix)) {
    print(scenario.matrix)
    stop("Scenario matrix contains NA; aborting before run.scenario().")
  }
  
  # 5) Reproduce assessPower’s run loop (without its fragile label plumbing)
  #    Compute posterior model probability of 'null' (no BACI term) and convert to power.
  scen.out <- apply(
    scenario.matrix, MARGIN = 1,
    FUN = function(xrow) {
      # run.scenario requires these objects in parent env:
      scenarioParams <<- sp
      scenario.matrix <<- scenario.matrix
      # names in xrow must be character so run.scenario’s unlist() works
      xlist <- as.list(xrow); names(xlist) <- names(xrow)
      run.scenario(xlist, scenario = sp)
    }
  )
  
  # Extract model probabilities (list of length 1 here)
  model.probs <- do.call("rbind", lapply(scen.out, function(x) unlist(x$model.probs)))
  model.prob.success <- matrix(0, nrow(model.probs), ncol(model.probs))
  model.prob.success[which(model.probs < 0.5)] <- 1
  power <- mean(model.prob.success)
  
  # Return a single numeric power
  as.numeric(power)
}





# Scenario & designs -----------------------------------------------------------
SCENARIO <- list(
  baseline_mean     = 0.30,  # 30% cover
  delta_step        = 0.15,  # ≈ 3%/yr * 5 yrs (step-change proxy)
  sd_precision      = 0.040,
  sd_spatial_peak   = 0.050,
  sd_temporal       = 0.040,
  T_before          = 2,
  T_after           = 5,
  nsims             = 200   # keep modest for a quick run; increase for final
)

designs <- tibble::tribble(
  ~Design,            ~n_controls, ~n_transects,
  "A: Low effort",            3,           5,
  "B: Medium effort",         5,          10,
  "C: High effort",           8,          15,
  "D: Max transects",         5,          20,
  "E: Max controls",         10,          10
)

# Run both methods -------------------------------------------------------------
message("Generating pilot data for epower …")
pilot_dat <- make_pilot_gaussian(
  baseline_mean    = SCENARIO$baseline_mean,
  sd_precision     = SCENARIO$sd_precision,
  sd_spatial_peak  = SCENARIO$sd_spatial_peak,
  sd_temporal      = SCENARIO$sd_temporal,
  n_controls       = max(designs$n_controls),
  n_transects      = max(designs$n_transects),
  T_before         = SCENARIO$T_before,
  T_after          = SCENARIO$T_after
)
# # minimial test
# pilot_dat <- make_pilot_gaussian(
#   baseline_mean = 0.30, sd_precision = 0.040,
#   sd_spatial_peak = 0.050, sd_temporal = 0.040,
#   n_controls = 5, n_transects = 10,
#   T_before = 2, T_after = 5
# )
# 
# run_epower_supply(pilot_dat, n_controls = 5, n_transects = 10,
#                   T_before = 2, T_after = 5, delta_step = 0.15, nsims = 5)

message("Running app-style (fast MC) …")
res_app <- designs %>%
  mutate(app = pmap(., ~ power_baci_appstyle_step(
    delta_step      = SCENARIO$delta_step,
    T_before        = SCENARIO$T_before,
    T_after         = SCENARIO$T_after,
    n_controls      = ..2,
    n_transects     = ..3,
    baseline_mean   = SCENARIO$baseline_mean,
    sd_precision    = SCENARIO$sd_precision,
    sd_spatial_peak = SCENARIO$sd_spatial_peak,
    sd_temporal     = SCENARIO$sd_temporal,
    nsims           = SCENARIO$nsims)),
    Power_App = map_dbl(app, "power_app")) %>%
  select(-app)

message("Running epower (supplyData -> powerScenario -> assessPower) …")
res_ep <- designs %>%
  mutate(Power_epower = pmap_dbl(., ~ run_epower_supply(
    dat          = pilot_dat,
    n_controls   = ..2,
    n_transects  = ..3,
    T_before     = SCENARIO$T_before,
    T_after      = SCENARIO$T_after,
    delta_step   = SCENARIO$delta_step,
    nsims        = SCENARIO$nsims
  )))

comparison <- res_app %>%
  left_join(res_ep, by = c("Design","n_controls","n_transects")) %>%
  mutate(Difference = Power_epower - Power_App)

print(comparison)

# Plot -------------------------------------------------------------------------
plot_data <- comparison %>%
  pivot_longer(cols = c(Power_App, Power_epower),
               names_to = "Method", values_to = "Power") %>%
  mutate(Method = recode(Method,
                         Power_App = "App-style (fast MC, step-change)",
                         Power_epower = "epower (sim, step-change)"),
         facet = paste(n_controls, "control sites"))

p <- ggplot(plot_data, aes(n_transects, Power, color = Method)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  geom_hline(yintercept = 0.8, linetype = "dashed") +
  facet_wrap(~facet) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  labs(title = "Power to detect BACI step-change uplift",
       subtitle = sprintf("Delta = +%d%% after; baseline = %d%%; T_before=%d, T_after=%d",
                          round(SCENARIO$delta_step*100), round(SCENARIO$baseline_mean*100),
                          SCENARIO$T_before, SCENARIO$T_after),
       x = "Transects per site", y = "Power", color = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

print(p)

cat("\nNOTES:\n",
    "• epower compares BACI step-changes; your app also supports trend-based (slope) detection.\n",
    "  Here we compare step-change to step-change for like-for-like validation.\n",
    "• The pilot table is synthetic but uses your variance components (precision, spatial, temporal).\n",
    "• To test trend equivalence, we can periodise 'After' and map 3%/yr to a sequence of fixed steps,\n",
    "  or extend the app-style simulator to fit a mixed model with time×treatment and compare decisions.\n")
