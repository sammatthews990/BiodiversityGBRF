# ---- packages ----
# install.packages(c("lmerTest", "dplyr", "ggplot2"))
library(lmerTest)
library(dplyr)
library(ggplot2)

# ---- power via LMM simulation ----
run_power_lmm_sim <- function(
    target_uplift_pct,          # difference in slope (treated vs control), % per year
    monitoring_years,
    monitoring_frequency = c("Annual","Biennial"),
    survey_precision_sd,        # measurement/observer SD
    peak_spatial_sd,            # within-site spatial heterogeneity across transects (SD)
    temporal_sd,                # site×year process SD (random intercept for site:year)
    n_ctrl_sites,               # number of control sites
    treated_sites = 1,          # number of treated sites (default 1)
    n_transect_values = 2:20,   # grid of transects to evaluate
    n_sims = 300,               # simulation reps per transect setting (raise for precision)
    alpha = 0.05,               # test size (one-sided default)
    ctrl_slope = 0,             # baseline control slope per year
    seed = 123
){
  set.seed(seed)
  monitoring_frequency <- match.arg(monitoring_frequency)
  uplift <- target_uplift_pct / 100   # convert %/yr to proportion/yr
  
  time_points <- if (monitoring_frequency == "Annual") {
    seq(0, monitoring_years, by = 1)
  } else {
    seq(0, monitoring_years, by = 2)
  }
  
  # helper: simulate one dataset and test time:treat
  simulate_and_test <- function(n_tran){
    # build design frame at transect resolution
    sites_ctrl <- paste0("C", seq_len(n_ctrl_sites))
    sites_trt  <- paste0("T", seq_len(treated_sites))
    sites      <- c(sites_ctrl, sites_trt)
    
    df <- expand.grid(
      site     = sites,
      year     = time_points,
      transect = seq_len(n_tran),
      KEEP.OUT.ATTRS = FALSE
    )
    df$treat <- as.integer(substr(df$site, 1, 1) == "T")
    df$time  <- as.numeric(df$year)
    
    # random effects: site:year intercepts ~ N(0, temporal_sd^2)
    sy_levels <- unique(interaction(df$site, df$year, drop = TRUE))
    u_sy <- rnorm(length(sy_levels), 0, temporal_sd)
    names(u_sy) <- sy_levels
    df$u_sy <- u_sy[interaction(df$site, df$year, drop = TRUE)]
    
    # residual (transect-level) noise
    sigma_eps <- sqrt(peak_spatial_sd^2 + survey_precision_sd^2)
    eps <- rnorm(nrow(df), 0, sigma_eps)
    
    # fixed effects: intercept 0 (arbitrary), control slope = ctrl_slope,
    #                treated slope = ctrl_slope + uplift (i.e., interaction = uplift)
    df$y <- 0 +
      ctrl_slope * df$time +
      0 * df$treat +
      uplift * (df$time * df$treat) +
      df$u_sy + eps
    
    # fit LMM and pull p-value for time:treat
    m <- suppressMessages(lmer(y ~ time * treat + (1 | site:year), data = df, REML = TRUE))
    co <- suppressMessages(coef(summary(m)))
    # Row name might be "time:treat" or "time:treat" exactly; guard for it:
    term <- "time:treat"
    if (!term %in% rownames(co)) return(NA_real_)
    pval <- co[term, "Pr(>|t|)"]
    
    # one-sided for positive uplift: halve two-sided p and check sign
    est  <- co[term, "Estimate"]
    p_one_sided <- if (est > 0) pval/2 else 1 - pval/2
    as.numeric(p_one_sided < alpha)
  }
  
  run_one_n <- function(n_tran){
    hits <- replicate(n_sims, simulate_and_test(n_tran))
    hits <- hits[!is.na(hits)]
    power_hat <- mean(hits)
    ci <- binom.test(sum(hits), length(hits))$conf.int
    tibble(
      N_Transects = n_tran,
      Power_Mean  = power_hat,
      Power_Lower = ci[1],
      Power_Upper = ci[2]
    )
  }
  
  bind_rows(lapply(n_transect_values, run_one_n))
}

# ---- small example ----
# Example scenario: 6-year monitoring, annual; modest temporal noise; decent transect noise.
ex_results <- run_power_lmm_sim(
  target_uplift_pct   = 3,     # +5% per year uplift at treated vs controls
  monitoring_years    = 6,     # 0..6 (7 time points if Annual)
  monitoring_frequency= "Annual",
  survey_precision_sd = 0.01,  # measurement SD
  peak_spatial_sd     = 0.08,  # small-scale heterogeneity SD
  temporal_sd         = 0.03,  # site-year process SD
  n_ctrl_sites        = 5,     # 6 control sites
  treated_sites       = 1,     # single treated site
  n_transect_values   = 2:10,  # 1..15 transects per site-year
  n_sims              = 300,   # bump to 1000+ for publication-quality precision
  alpha               = 0.05,  # one-sided for uplift
  ctrl_slope          = 0,     # controls flat on average
  seed                = 2025
)

  print(ex_results)

# ---- plot ----
ggplot(ex_results, aes(N_Transects, Power_Mean)) +
  geom_ribbon(aes(ymin = Power_Lower, ymax = Power_Upper), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_hline(yintercept = 0.8, linetype = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Power to detect a positive slope difference (time × treat) in an LMM",
    subtitle = "One treated site vs control mean; annual sampling; Satterthwaite p-values",
    x = "Transects per site per year",
    y = "Estimated power (with binomial 95% CI)"
  ) +
  theme_minimal(base_size = 12)
