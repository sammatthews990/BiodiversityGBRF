# Biodiversity GBRF Tools

## Overview
This repository contains modelling utilities, data, and interactive dashboards used to support biodiversity credit and scenario analysis work for the Great Barrier Reef Foundation (GBRF). The primary deliverable is an R/Shiny application (located in `BACI_Credit/`) that explores biodiversity uplift scenarios using Bayesian inference, supported by helper scripts and input datasets.

## Repository Structure
- `BACI_Credit/` – R/Shiny application source, helper scripts, and input datasets for BACI (Before-After-Control-Impact) analysis.
  - `BACIBayesapp.R` – main Shiny application entry point.
  - `baci_analysis_functions.R` – shared analysis utilities sourced by the app.
  - `monitoringdata/` – supporting monitoring datasets used by the simulator and visualisations.
  - `simdata_ADRIA.csv`, `Reefs.csv`, `InshoreFishVarainces.R`, etc. – model inputs for the dashboards and analyses.
- `BiodiversityAccountingTools.Rproj` – RStudio project file for convenient IDE configuration.
- `BiodiversityScenarioExplorer.pbix` – Power BI scenario explorer report file.

## Prerequisites
- R (>= 4.2 recommended)
- RStudio (optional but recommended for working with the `.Rproj` project)
- System toolchain capable of compiling R packages (e.g. Rtools on Windows, Xcode Command Line Tools on macOS, build-essential on Linux)

## Setup
1. **Clone the repository**
   ```bash
   git clone https://github.com/<your-org>/BiodiversityGBRF.git
   cd BiodiversityGBRF
   ```
2. **Open the project in RStudio** (optional) by double-clicking `BiodiversityAccountingTools.Rproj` or using `File -> Open Project`.
3. **Install R package dependencies** directly in R:
   ```r
   required_packages <- c(
     "shiny", "dplyr", "tidyr", "ggplot2", "tibble", "bslib", "scales",
     "bsicons", "rstanarm", "DT", "INLA", "leaflet", "readr", "purrr"
   )
   install.packages(setdiff(required_packages, rownames(installed.packages())))
   ```

## Configuration
The Shiny application reads configuration values and inputs from objects defined at the top of `BACI_Credit/BACIBayesapp.R`.

- **Survey method parameters**: Adjust `survey_methods_params` to update default sampling costs and precision assumptions for different survey methodologies.
- **Metric definitions**: Custom definitions are loaded via `METRIC_DEFINITIONS` (sourced from `baci_analysis_functions.R`). Update the underlying tibble or helper function to add or revise metrics surfaced in the dashboard.
- **Input datasets**: Replace or extend the CSV and RData files within `BACI_Credit/` to use alternative modelling outputs. Ensure column names remain consistent with those referenced in the app logic.

When deploying to a hosting environment (e.g. shinyapps.io or Posit Connect) ensure all referenced datasets remain within the `BACI_Credit/` directory or adjust file paths accordingly.

## Running the Shiny Application
From the repository root or within RStudio, launch the application with:

```r
shiny::runApp("BACI_Credit")
```

The dashboard provides:
- Exploration of counterfactual vs. intervention scenarios with uplift calculations.
- Power analysis and survey design insights based on configurable sampling assumptions.
- Interactive maps and tables for reviewing biodiversity outcomes across reef sites.

## Power BI Scenario Explorer
The `BiodiversityScenarioExplorer.pbix` file can be opened in Microsoft Power BI Desktop for additional scenario analysis and reporting. Update data sources as needed to reflect the latest modelling outputs.

## Development Tips
- Use version control branches for new analyses or UI features and open pull requests for review.
- When adding new data files, document their provenance and schema in this README or a dedicated data dictionary.
- Consider adding unit tests or validation scripts (e.g. via `testthat`) if you extend the analysis functions.

