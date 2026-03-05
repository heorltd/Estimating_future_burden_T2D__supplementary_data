# Supplementary data for the manuscript: Estimating the future burden of T2D

## Note

Please see license file.

## Overview

This repository contains a minimal, standalone implementation of the Type 2 Diabetes (T2D) Total System Value (TSV) engine. The model simulates the progression of Type 2 Diabetes in a population over time, tracking health outcomes, complications, and the effects of different treatment scenarios.

**Quick Links:**

\- 📚 [Installation Guide](INSTALL.md) - Step-by-step for non-R users

\- 🔍 [Validation Script](validate_setup.R) - Check your setup

\- 📊 [Example Analysis](example_analysis.R) - Analyse and visualise
results

### What This Model Does

The T2D TSV engine is a **systems dynamics model** (implemented as a
system of ordinary differential equations) that:

-   Simulates a population stratified by:
    -   **Diabetes status**: No diabetes (D0), Uncontrolled T2D (D1),
        Controlled T2D (D2)
    -   **Age groups**: 18-45 years (a1), 45-65 years (a2), 65+ years
        (a3)
-   Tracks transitions between states over time
-   Models complications:
    -   **Macrovascular**: e.g. heart disease, stroke
    -   **Microvascular**: e.g. kidney disease, neuropathy, retinopathy
    -   **Other**: e.g. hypoglycaemia
-   Calculates health and economic outcomes:
    -   Deaths and mortality rates
    -   Hospitalisations and bed days
    -   Direct and indirect healthcare costs
    -   Quality-adjusted life years (QALYs)
    -   Environmental impacts

### Key Features

-   **Pre-calibrated parameters**: Model parameters are calibrated to
    match real-world epidemiological data.
-   **Treatment effects**: Ability to model interventions by adjusting
    transition rates
-   **Scenario comparison**: Compare "status quo" baseline with 4
    intervention scenarios
-   **Long-term projections**: Simulate from 2024 to 2060 (36 years)

## Repository Structure

```         
T2D_Engine_Minimal/
├── README.md                              # This file
├── run_T2D_model.R                        # Main script to run the model
├── outputs/                               # Output data (CSV files)
├── inputs/                                # Input data (CSV files)
│   ├── precalibrated_population_2016+.csv # Initial population state
│   ├── precalibrated_params_2016+.csv     # Calibrated model parameters
│   ├── static_params.csv                  # Fixed model parameters
│   ├── static_params_multifactorial.csv   # Parameters for multifactorial interventions
│   ├── net_migration_age_breakdown.csv    # Migration data by age
│   ├── complication_*.csv                 # Complication rates, costs, utilities
│   ├── diagnosis_direct_costs.csv         # Diagnosis costs
│   ├── routine_*.csv                      # Routine care costs
│   └── ... (other cost and health data files)
└── source/                                # R source code
    ├── library_loader.R                   # Loads required R packages
    ├── load_precalibrated_initial_population.R  # Loads initial state
    ├── load_params.R                      # Loads model parameters
    ├── T2D_SDM.R                          # Systems Dynamics Model (ODEs)
    ├── T2D_PHA.R                          # Post-Hoc Analysis (outcomes)
    ├── preamble.R                         # Setup script
    ├── post_hoc_functions.R               # Additional analysis functions
    └── post_processing_functions.R        # Data processing utilities
```

## Installation

### Prerequisites

You need to have **R** installed on your system (version 4.0 or higher
recommended).

#### Installing R

-   **Windows**: Download from
    <https://cran.r-project.org/bin/windows/base/>

-   **macOS**: Download from <https://cran.r-project.org/bin/macosx/>

-   **Linux (Ubuntu/Debian)**:

    ``` bash
    sudo apt-get update
    sudo apt-get install r-base
    ```

### Installing Required R Packages

The model requires the following R packages:

1.  **deSolve** - For solving ordinary differential equations (ODEs)
2.  **openxlsx** - For writing Excel output files
3.  **ggplot2** - For plotting (used in some functions)
4.  **dplyr** - For data manipulation
5.  **tidyr** - For data tidying
6.  **purrr** - For functional programming
7.  **scales** - For scaling functions
8.  **Metrics** - For model metrics
9.  **ggpubr** - For publication-ready plots
10. **growthmodels** - For Gompertz growth models

#### Option 1: Install packages individually

Open R or RStudio and run:

``` r
install.packages(c("deSolve", "openxlsx", "ggplot2", "dplyr", "tidyr", 
                   "purrr", "scales", "Metrics", "ggpubr", "growthmodels"))
```

#### Option 2: Install from the script

The packages will be loaded automatically when you run
`run_T2D_model.R`. If a package is missing, you'll see an error message.
Install it using:

``` r
install.packages("package_name")
```

### Verifying Installation

To verify R is installed correctly, open a terminal/command prompt and
type:

``` bash
R --version
```

You should see R version information displayed.

## Usage

### Running the Model

#### From Command Line

Navigate to the `T2D_Engine_Minimal` directory and run:

``` bash
Rscript run_T2D_model.R
```

#### From R/RStudio

1.  Open R or RStudio

2.  Set your working directory to the `T2D_Engine_Minimal` folder:

    ``` r
    setwd("/path/to/T2D_Engine_Minimal")
    ```

3.  Run the script:

    ``` r
    source("run_T2D_model.R")
    ```

### What Happens When You Run It

The script will:

1.  Load all required libraries and source files
2.  Run **Base case: Status Quo (Do Nothing)**
    -   Baseline scenario with no interventions
    -   All treatment effects = 1.0 (no change from current rates)
3.  Run **Scenario A: Total Control**
    -   Intervention where diabetes control rates are improved
    -   Aims to move more people from uncontrolled to controlled T2D
4.  Run **Scenario A1: Total Control & Multifactorial Control**
    -   Same as Scenario A but with multifactorial intervention parameters
    -   Uses alternative parameter set for enhanced control
5.  Run **Scenario B: Total Control & GLP1**
    -   Combines improved control rates with GLP1 treatment effects
    -   Includes reduced complication rates from GLP1 therapy
6.  Run **Scenario B1: Total Control & GLP1 & Multifactorial Control**
    -   Most comprehensive scenario combining all interventions
    -   GLP1 effects with multifactorial control parameters
7.  Save output files:
    -   `outputs/output_status_quo.csv` - Base case results
    -   `outputs/output_scenario_total_control.csv` - Scenario A results
    -   `outputs/output_scenario_total_control_MF.csv` - Scenario A1 results
    -   `outputs/output_scenario_GLP1.csv` - Scenario B results
    -   `outputs/output_scenario_GLP1_MF.csv` - Scenario B1 results
8.  Print summary statistics to the console

### Expected Runtime

-   On a typical modern computer: **30 seconds to 2 minutes**
-   Progress messages will be displayed during the run

## Understanding the Output

### Output Files

The model generates CSV files with one row per year (2024-2061). Key
columns include:

#### Population Compartments

-   `time`: Year
-   `F0`: Feeder compartment (individuals \<18 years old)
-   `D0_a1`, `D0_a2`, `D0_a3`: No diabetes by age group
-   `D1_a1`, `D1_a2`, `D1_a3`: Uncontrolled T2D by age group
-   `D2_a1`, `D2_a2`, `D2_a3`: Controlled T2D by age group

#### Complications (Annual Incidence)

-   `fC_o`: Other complications
-   `fC_Ma`: Macrovascular complications
-   `fC_Mi`: Microvascular complications

#### Deaths (Annual)

-   `D_acm_*`: All-cause mortality by diabetes status and age
-   `D_e_*`: Disease-specific mortality

#### Health Economics

-   `direct_costs`: Direct healthcare costs
-   `indirect_costs`: Indirect costs (productivity loss)
-   `bed_days`: Hospital bed days
-   `hospitalisations`: Number of hospitalizations
-   `disutilities`: Quality of life loss (QALY)
-   `LYL`: Life years lost

#### Summary Variables

-   `f_P`: Total population
-   `f_D0_aX`: Total non-diabetic population
-   `f_D1_aX`: Total uncontrolled T2D population
-   `f_D2_aX`: Total controlled T2D population

### Interpreting Results

#### Quick Analysis with Example Script

The easiest way to analyze results is using the provided
`example_analysis.R` script:

``` bash
# After running the model
Rscript example_analysis.R
```

This script will: - Calculate key outcome metrics (complications,
deaths, costs, QALYs) for all 5 scenarios - Compare intervention scenarios against
base case quantitatively - Generate
publication-ready visualizations: - `outputs/plot_population_by_status.png` -
Population trajectories by scenario - `outputs/plot_complications_by_type.png` - Annual
complications by scenario - `outputs/plot_cumulative_complications.png` - Cumulative
outcomes comparison

#### Comparing Scenarios

To understand the impact of an intervention:

1.  Load both output files in R or Excel
2.  Compare key outcomes:
    -   **Complications**: Fewer complications = better
    -   **Deaths**: Fewer deaths = better
    -   **Costs**: Lower costs = more cost-effective
    -   **QALYs**: Higher QALYs (lower disutilities) = better quality of
        life

#### Example Analysis in R

``` r
# Load output files (all 5 scenarios)
status_quo <- read.csv("outputs/output_status_quo.csv")
scenario_a <- read.csv("outputs/output_scenario_total_control.csv")
scenario_a1 <- read.csv("outputs/output_scenario_total_control_MF.csv")
scenario_b <- read.csv("outputs/output_scenario_GLP1.csv")
scenario_b1 <- read.csv("outputs/output_scenario_GLP1_MF.csv")

# Calculate total complications over entire period
sq_total_complications <- sum(status_quo$fC_Ma + status_quo$fC_Mi + status_quo$fC_o, na.rm = TRUE)
a_total_complications <- sum(scenario_a$fC_Ma + scenario_a$fC_Mi + scenario_a$fC_o, na.rm = TRUE)
b_total_complications <- sum(scenario_b$fC_Ma + scenario_b$fC_Mi + scenario_b$fC_o, na.rm = TRUE)

# Calculate reduction (Scenario B vs Base case)
reduction <- sq_total_complications - b_total_complications
pct_reduction <- (reduction / sq_total_complications) * 100

cat(sprintf("Scenario B complications avoided: %.0f (%.1f%% reduction)\n", 
            reduction, pct_reduction))

# Plot population by diabetes status over time (Base case vs Scenario B)
library(ggplot2)
library(tidyr)
library(dplyr)

# Prepare data for plotting
sq_long <- status_quo %>%
  select(time, f_D0_aX, f_D1_aX, f_D2_aX) %>%
  pivot_longer(cols = -time, names_to = "status", values_to = "population") %>%
  mutate(scenario = "Base case")

b_long <- scenario_b %>%
  select(time, f_D0_aX, f_D1_aX, f_D2_aX) %>%
  pivot_longer(cols = -time, names_to = "status", values_to = "population") %>%
  mutate(scenario = "Scenario B (GLP1)")

combined <- rbind(sq_long, b_long)

# Plot
ggplot(combined, aes(x = time, y = population, color = status)) +
  geom_line(linewidth = 1) +
  facet_wrap(~scenario) +
  labs(title = "Population by Diabetes Status",
       x = "Year", y = "Population",
       color = "Status") +
  theme_minimal()
```

## Model Details

### Mathematical Structure

The model is a **compartmental model** solved using ordinary
differential equations (ODEs). Individuals flow between compartments
based on transition rates:

```         
F0 (Age <18) → [aging] → D0_a1 (No diabetes, age 18-45)
                         ↓
                         D1_a1 (Uncontrolled T2D, age 18-45)
                         ↕
                         D2_a1 (Controlled T2D, age 18-45)
```

Similar structures exist for age groups a2 (45-65) and a3 (65+).

### Key Transition Rates

-   **δ (delta)**: Birth rate (Gompertz function)
-   **φ (phi)**: Net migration rate (Gompertz function)
-   **θ (theta)**: Aging rate (18→a1, a1→a2, a2→a3)
-   **ι (iota)**: T2D incidence rate (D0→D1)
-   **λ (lambda)**: Control rate (D1→D2)
-   **ε (epsilon)**: Loss of control rate (D2→D1)
-   **α (alpha)**: Other complication rate
-   **β (beta)**: Microvascular complication rate
-   **γ (gamma)**: Macrovascular complication rate
-   **μ (mu)**: All-cause mortality rate

### Treatment Effects

Treatment effects are multiplicative factors (η, eta) applied to
transition rates:

-   **η = 1.0**: No effect (baseline)
-   **η \< 1.0**: Rate decreases (beneficial for bad outcomes)
-   **η \> 1.0**: Rate increases (beneficial for good outcomes like
    control)

For example: - `eta_lambda_a1 = 2.0` doubles the rate of achieving
diabetes control in age group 1 - `eta_beta_1_a1 = 0.8` reduces
microvascular complications by 20% in uncontrolled T2D, age group 1

### Calibration

The model is pre-calibrated using: - UK population data -
Epidemiological studies on T2D prevalence and incidence - Published
complication rates - Healthcare cost data - Quality of life (utility)
data

## Modifying the Model

### Creating New Scenarios

To test a different intervention, modify the treatment effects in
`run_T2D_model.R`:

``` r
# Example: Reduce macrovascular complications by 15% (eta = 0.85)
treatment_effects_custom <- c(
  eta_i_a1 = 1, eta_i_a2 = 1, eta_i_a3 = 1,
  eta_lambda_a1 = 1, eta_lambda_a2 = 1, eta_lambda_a3 = 1,
  eta_epsilon_a1 = 1, eta_epsilon_a2 = 1, eta_epsilon_a3 = 1,
  eta_alpha_1_a1 = 1, eta_alpha_2_a1 = 1,
  eta_alpha_1_a2 = 1, eta_alpha_2_a2 = 1,
  eta_alpha_1_a3 = 1, eta_alpha_2_a3 = 1,
  eta_beta_1_a1 = 1, eta_beta_2_a1 = 1,
  eta_beta_1_a2 = 1, eta_beta_2_a2 = 1,
  eta_beta_1_a3 = 1, eta_beta_2_a3 = 1,
  
  # Reduce macrovascular complications
  eta_gamma_1_a1 = 0.85, eta_gamma_2_a1 = 0.85,
  eta_gamma_1_a2 = 0.85, eta_gamma_2_a2 = 0.85,
  eta_gamma_1_a3 = 0.85, eta_gamma_2_a3 = 0.85,
  
  eta_tau_alpha = 1, eta_tau_beta = 1, eta_tau_gamma = 1
)

output_custom <- T2D_TSV_engine(end_year = 2061,
                                treatment_effects = treatment_effects_custom,
                                multifactorial = FALSE)

write.csv(output_custom, file = 'output_custom_scenario.csv', row.names = FALSE)
```

### Changing Simulation Period

To simulate for a different time period, change the `end_year` variable:

``` r
end_year <- 2050  # Simulate through 2050 instead of 2061
```

Note: The start year (2024) is hardcoded in the `T2D_TSV_engine`
function.

### Using Multifactorial Interventions

The model supports multifactorial interventions (combined treatment
approaches). To use:

``` r
output <- T2D_TSV_engine(end_year = 2061,
                         treatment_effects = treatment_effects_DN,
                         multifactorial = TRUE)  # Set to TRUE
```

This loads different parameters from
`inputs/static_params_multifactorial.csv`.

## Troubleshooting

### Common Issues

#### Error: "package 'X' is not installed"

**Solution**: Install the missing package:

``` r
install.packages("package_name")
```

#### Error: "cannot open file 'inputs/...csv'"

**Solution**: Make sure you're running the script from the correct
directory. Your working directory should be `T2D_Engine_Minimal`.

Check with:

``` r
getwd()
```

Set with:

``` r
setwd("/path/to/T2D_Engine_Minimal")
```

#### The script is very slow

**Solution**: The ODE solver can be computationally intensive. This is
normal. Expected runtime is 30 seconds to 2 minutes. If it takes much
longer, you may have limited computational resources.

#### Warning messages about "NAs introduced by coercion"

**Solution**: These warnings are typically harmless. They occur during
post-processing when calculating annual rates from cumulative values for
the final year.

#### Output files show NA values in the last row

**Solution**: This is expected behavior. The last row contains NA values
because annual rates (differences) cannot be calculated for the final
year. Exclude the last row when analyzing results. Therefore we solve
until 2061 to ensure complete results for the year 2060.

### Getting Help

If you encounter issues not covered here:

1.  Check that all input files are present in the `inputs/` directory
2.  Verify that all source files are in the `source/` directory
3.  Ensure you have the correct R version (4.0+)
4.  Check that all required packages are installed

## Technical Details

### Software Environment

-   **Language**: R (version 4.0 or higher)
-   **ODE Solver**: `deSolve::ode()` using LSODA algorithm
-   **Timestep**: Annual (1 year)
-   **Method**: Systems dynamics modeling / Compartmental model

### Computational Notes

-   The model solves a system of 45 coupled ordinary differential
    equations
-   Stiff ODE solver (LSODA) automatically switches between stiff and
    non-stiff methods
-   Precision: Relative tolerance = 1e-6, Absolute tolerance = 1e-8
    (defaults)

### Model Assumptions

1.  **Closed cohort after initial period**: Migration is modeled but
    eventually stabilizes
2.  **Age-dependent rates**: All transition rates vary by age group
3.  **No recovery from T2D**: Once diagnosed, individuals cannot return
    to non-diabetic state (D0)
4.  **Memoryless transitions**: Markov assumption (future state depends
    only on current state)
5.  **Homogeneous populations**: No further stratification beyond age
    and diabetes status

### Validation

The model has been calibrated and validated against:

-   UK diabetes prevalence data
-   Published T2D incidence rates
-   Complication rates from clinical studies
-   Mortality data from national statistics

## Citation

If you use this model in academic work, please cite:

> [Manuscript citation to be added after publication]

## License

[License information to be added]

## Contact

For questions about the model, please contact:

[thomas.padgett\@heor.co.uk](mailto:thomas.padgett@heor.co.uk){.email}

## Version History

-   **v1.0** (2026-03-03): Initial minimal implementation for manuscript
    submission
    -   Core T2D TSV engine
    -   Five intervention scenarios (Base case, Scenario A, A1, B, B1)
    -   Complete documentation
    -   Example analysis comparing all scenarios
