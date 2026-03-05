#!/usr/bin/env Rscript

#' script: run_T2D_model.R
#' Author: Thomas E. Padgett (adapted for minimal example)
#' Date: 2026-03-03
#' 
#' Description: Minimal implementation of the T2D TSV engine for manuscript.
#' This is a simplified version that demonstrates the core functionality
#' of the Type 2 Diabetes Treatment Simulation Value (TSV) model.
#' 
#' The model simulates a population with Type 2 Diabetes (T2D) over time,
#' tracking:
#'   - Diabetes status (no diabetes, uncontrolled T2D, controlled T2D)
#'   - Age groups (18-45, 45-65, 65+)
#'   - Complications (macrovascular, microvascular, other)
#'   - Deaths and health outcomes
#' 
#' This script runs two scenarios:
#'   1. Status Quo (do nothing) - baseline scenario
#'   2. Scenario A (total control) - improved diabetes control

## 0. Setup ####################################################################

# Set working directory to script location (if running interactively)
if (!interactive()) {
  script_dir <- dirname(sys.frame(1)$ofile)
  setwd(script_dir)
}

# Set random seed for reproducibility
set.seed(1)

# Load required libraries
# Install packages if needed (uncomment the next line)
# install.packages(c("deSolve", "openxlsx"))

library(deSolve)  # For solving ordinary differential equations
library(openxlsx) # For writing Excel output files

# Source the model components
source('source/library_loader.R')
source('source/load_precalibrated_initial_population.R')
source('source/load_params.R')
source('source/T2D_SDM.R')
source('source/T2D_PHA.R')

cat("Libraries and functions loaded successfully.\n")

## 1. Define the T2D TSV Engine ################################################

T2D_TSV_engine <- function(end_year,
                           treatment_effects,
                           multifactorial = FALSE){
  
  start_year <- 2024 # Starting year for simulation
  output_timestep <- 1 # Annual timestep
  
  # Create time sequence
  times <- seq(from = start_year,
               to = end_year,
               by = output_timestep)
  
  cat(sprintf("Running simulation from %d to %d...\n", start_year, end_year))
  
  # Load initial population state (precalibrated to match real data)
  initial_state <- load_precalibrated_initial_population()
  
  # Load model parameters (transition rates, mortality, etc.)
  parameters <- c(load_params(multifactorial),
                  treatment_effects)
  
  # Load age structure of net migration (for demographic changes)
  mig_age_props <- read.csv('inputs/net_migration_age_breakdown.csv')
  
  # Run the Systems Dynamics Model (SDM) using ODE solver
  cat("  Solving differential equations...\n")
  output <- as.data.frame(ode(y = initial_state, 
                              times = times,
                              func = T2D_SDM,
                              parms = parameters,
                              mig_age_props = mig_age_props))
  
  # Post-processing to calculate health and economic outcomes
  cat("  Post-processing results...\n")
  output <- T2D_PHA(output, parameters, multifactorial)
  
  cat("  Simulation complete!\n")
  return(output)
}

## 2. Define Simulation Parameters #############################################

end_year <- 2061  # Simulate through 2061 (37 years)

cat(sprintf("\n=== T2D TSV Model - Minimal Example ===\n"))
cat(sprintf("Simulation period: 2024 to %d\n\n", end_year))

## 2.1. Base case: Status Quo ("Do Nothing") ####################################

cat("Running Base case: Status Quo ('Do Nothing')\n")
cat("  All treatment effects set to 1.0\n")

# Treatment effects - all set to 1 (no effect = baseline)
# These are multiplicative factors on transition rates
treatment_effects_DN <- c(
  # Effects on T2D incidence by age group
  eta_i_a1 = 1, eta_i_a2 = 1, eta_i_a3 = 1,
  
  # Effects on rate of movement from uncontrolled to controlled T2D
  eta_lambda_a1 = 1, eta_lambda_a2 = 1, eta_lambda_a3 = 1,
  
  # Effects on rate of movement from controlled to uncontrolled T2D
  eta_epsilon_a1 = 1, eta_epsilon_a2 = 1, eta_epsilon_a3 = 1,
  
  # Effects on other complications (D1=uncontrolled, D2=controlled)
  eta_alpha_1_a1 = 1, eta_alpha_2_a1 = 1,
  eta_alpha_1_a2 = 1, eta_alpha_2_a2 = 1,
  eta_alpha_1_a3 = 1, eta_alpha_2_a3 = 1,
  
  # Effects on microvascular complications
  eta_beta_1_a1 = 1, eta_beta_2_a1 = 1,
  eta_beta_1_a2 = 1, eta_beta_2_a2 = 1,
  eta_beta_1_a3 = 1, eta_beta_2_a3 = 1,
  
  # Effects on macrovascular complications
  eta_gamma_1_a1 = 1, eta_gamma_2_a1 = 1,
  eta_gamma_1_a2 = 1, eta_gamma_2_a2 = 1,
  eta_gamma_1_a3 = 1, eta_gamma_2_a3 = 1,
  
  # Effects on proportion of complications that are fatal
  eta_tau_alpha = 1,  # other complications
  eta_tau_beta = 1,   # microvascular
  eta_tau_gamma = 1   # macrovascular
)

output_DN <- T2D_TSV_engine(end_year = end_year,
                            treatment_effects = treatment_effects_DN,
                            multifactorial = FALSE)

# Save results
write.csv(output_DN, file = 'outputs/output_status_quo.csv', row.names = FALSE)
cat("  Results saved to: outputs/output_status_quo.csv\n\n")

## 2.2. Scenario A: Total Control ##############################################

cat("Running Scenario A: Total Control\n")
cat("  Improved control rates to minimize complications\n")

# Calculate improved control rates (eta_lambda values)
# These are calibrated so that moving everyone to controlled T2D
# balances out complications and mortality
P <- load_params(multifactorial = FALSE)

# For age group 1 (18-45)
eta_lambda_2_a1 <- ((1-((P['alpha_1_a1']*P['tau_alpha_a1'])
                      + (P['beta_1_a1']*P['tau_beta_a1']) 
                      + (P['gamma_1_a1']*P['tau_gamma_a1'])
                      + P['mu_a1']))
                    /P['lambda_2_a1'])

# For age group 2 (45-65)
eta_lambda_2_a2 <- ((1-((P['alpha_1_a2']*P['tau_alpha_a2'])
                        + (P['beta_1_a2']*P['tau_beta_a2']) 
                        + (P['gamma_1_a2']*P['tau_gamma_a2'])
                        + P['mu_a2']))
                    /P['lambda_2_a2'])

# For age group 3 (65+)
eta_lambda_2_a3 <- ((1-((P['alpha_1_a3']*P['tau_alpha_a3'])
                        + (P['beta_1_a3']*P['tau_beta_a3']) 
                        + (P['gamma_1_a3']*P['tau_gamma_a3'])
                        + P['mu_a3']))
                    /P['lambda_2_a3'])

cat(sprintf("  Calculated eta_lambda values: %.4f, %.4f, %.4f\n",
            eta_lambda_2_a1, eta_lambda_2_a2, eta_lambda_2_a3))

# Treatment effects with improved control rates
treatment_effects_ScenarioA <- c(
  eta_i_a1 = 1, eta_i_a2 = 1, eta_i_a3 = 1,
  
  # Improved control rates
  eta_lambda_a1 = unname(eta_lambda_2_a1),
  eta_lambda_a2 = unname(eta_lambda_2_a2),
  eta_lambda_a3 = unname(eta_lambda_2_a3),
  
  eta_epsilon_a1 = 1, eta_epsilon_a2 = 1, eta_epsilon_a3 = 1,
  eta_alpha_1_a1 = 1, eta_alpha_2_a1 = 1,
  eta_alpha_1_a2 = 1, eta_alpha_2_a2 = 1,
  eta_alpha_1_a3 = 1, eta_alpha_2_a3 = 1,
  eta_beta_1_a1 = 1, eta_beta_2_a1 = 1,
  eta_beta_1_a2 = 1, eta_beta_2_a2 = 1,
  eta_beta_1_a3 = 1, eta_beta_2_a3 = 1,
  eta_gamma_1_a1 = 1, eta_gamma_2_a1 = 1,
  eta_gamma_1_a2 = 1, eta_gamma_2_a2 = 1,
  eta_gamma_1_a3 = 1, eta_gamma_2_a3 = 1,
  eta_tau_alpha = 1, eta_tau_beta = 1, eta_tau_gamma = 1
)

output_ScenarioA <- T2D_TSV_engine(end_year = end_year,
                                   treatment_effects = treatment_effects_ScenarioA,
                                   multifactorial = FALSE)

# Save results
write.csv(output_ScenarioA, file = 'outputs/output_scenario_total_control.csv', row.names = FALSE)
cat("  Results saved to: outputs/output_scenario_total_control.csv\n\n")

## 2.3. Scenario A1: Total Control ##############################################

cat("Running Scenario A1: Total Control & Multifactorial Control\n")
cat("  Improved control rates to minimize complications\n")

# Calculate improved control rates (eta_lambda values)
# These are calibrated so that moving everyone to controlled T2D
# balances out complications and mortality
P <- load_params(multifactorial = TRUE)

# For age group 1 (18-45)
eta_lambda_2_a1 <- ((1-((P['alpha_1_a1']*P['tau_alpha_a1'])
                        + (P['beta_1_a1']*P['tau_beta_a1']) 
                        + (P['gamma_1_a1']*P['tau_gamma_a1'])
                        + P['mu_a1']))
                    /P['lambda_2_a1'])

# For age group 2 (45-65)
eta_lambda_2_a2 <- ((1-((P['alpha_1_a2']*P['tau_alpha_a2'])
                        + (P['beta_1_a2']*P['tau_beta_a2']) 
                        + (P['gamma_1_a2']*P['tau_gamma_a2'])
                        + P['mu_a2']))
                    /P['lambda_2_a2'])

# For age group 3 (65+)
eta_lambda_2_a3 <- ((1-((P['alpha_1_a3']*P['tau_alpha_a3'])
                        + (P['beta_1_a3']*P['tau_beta_a3']) 
                        + (P['gamma_1_a3']*P['tau_gamma_a3'])
                        + P['mu_a3']))
                    /P['lambda_2_a3'])

cat(sprintf("  Calculated eta_lambda values: %.4f, %.4f, %.4f\n",
            eta_lambda_2_a1, eta_lambda_2_a2, eta_lambda_2_a3))

# Treatment effects with improved control rates
treatment_effects_ScenarioA1 <- c(
  eta_i_a1 = 1, eta_i_a2 = 1, eta_i_a3 = 1,
  
  # Improved control rates
  eta_lambda_a1 = unname(eta_lambda_2_a1),
  eta_lambda_a2 = unname(eta_lambda_2_a2),
  eta_lambda_a3 = unname(eta_lambda_2_a3),
  
  eta_epsilon_a1 = 1, eta_epsilon_a2 = 1, eta_epsilon_a3 = 1,
  eta_alpha_1_a1 = 1, eta_alpha_2_a1 = 1,
  eta_alpha_1_a2 = 1, eta_alpha_2_a2 = 1,
  eta_alpha_1_a3 = 1, eta_alpha_2_a3 = 1,
  eta_beta_1_a1 = 1, eta_beta_2_a1 = 1,
  eta_beta_1_a2 = 1, eta_beta_2_a2 = 1,
  eta_beta_1_a3 = 1, eta_beta_2_a3 = 1,
  eta_gamma_1_a1 = 1, eta_gamma_2_a1 = 1,
  eta_gamma_1_a2 = 1, eta_gamma_2_a2 = 1,
  eta_gamma_1_a3 = 1, eta_gamma_2_a3 = 1,
  eta_tau_alpha = 1, eta_tau_beta = 1, eta_tau_gamma = 1
)

output_ScenarioA1 <- T2D_TSV_engine(end_year = end_year,
                                    treatment_effects = treatment_effects_ScenarioA1,
                                    multifactorial = TRUE)

# Save results
write.csv(output_ScenarioA1, file = 'outputs/output_scenario_total_control_MF.csv', row.names = FALSE)
cat("  Results saved to: outputs/output_scenario_total_control_MF.csv\n\n")


## 2.4. Scenario B: Total Control + GLP1  ######################################
cat("Running Scenario B: Total Control & GLP1\n")

# Treatment effects with improved control rates
treatment_effects_ScenarioB <- c(
  eta_i_a1 = 1, eta_i_a2 = 1, eta_i_a3 = 1,
  
  # Improved control rates - placeholders
  eta_lambda_a1 = 0,
  eta_lambda_a2 = 0,
  eta_lambda_a3 = 0,
  
  eta_epsilon_a1 = 1, eta_epsilon_a2 = 1, eta_epsilon_a3 = 1,
  
  eta_alpha_1_a1 = 1, eta_alpha_2_a1 = 1,
  eta_alpha_1_a2 = 1, eta_alpha_2_a2 = 1,
  eta_alpha_1_a3 = 1, eta_alpha_2_a3 = 1,
  
  eta_beta_1_a1 = 0.973158334,  eta_beta_2_a1 = 0.960074586,  
  eta_beta_1_a2 = 0.991091723,  eta_beta_2_a2 = 0.988931669,  
  eta_beta_1_a3 = 0.996987624,  eta_beta_2_a3 = 0.995820746,  
  
  eta_gamma_1_a1 = 0.927985317, eta_gamma_2_a1 = 0.932495738, 
  eta_gamma_1_a2 = 0.91394954,  eta_gamma_2_a2 = 0.916134931,
  eta_gamma_1_a3 = 0.905634064, eta_gamma_2_a3 = 0.905386017,
  
  eta_tau_alpha = 1, eta_tau_beta = 1, eta_tau_gamma = 0.89
)

# Figure out lambdas
P <- load_params(multifactorial = F)
# a1
eta_lambda_2_a1 <- ((1-((treatment_effects_ScenarioB['eta_alpha_1_a1']*P['alpha_1_a1']*treatment_effects_ScenarioB['eta_tau_alpha']*P['tau_alpha_a1'])
                        + (treatment_effects_ScenarioB['eta_beta_1_a1']*P['beta_1_a1']*treatment_effects_ScenarioB['eta_tau_beta']*P['tau_beta_a1']) 
                        + (treatment_effects_ScenarioB['eta_gamma_1_a1']*P['gamma_1_a1']*treatment_effects_ScenarioB['eta_tau_gamma']*P['tau_gamma_a1'])
                        + P['mu_a1']))
                    /P['lambda_2_a1'])
# a2
eta_lambda_2_a2 <- ((1-((treatment_effects_ScenarioB['eta_alpha_1_a2']*P['alpha_1_a2']*treatment_effects_ScenarioB['eta_tau_alpha']*P['tau_alpha_a2'])
                        + (treatment_effects_ScenarioB['eta_beta_1_a2']*P['beta_1_a2']*treatment_effects_ScenarioB['eta_tau_beta']*P['tau_beta_a2']) 
                        + (treatment_effects_ScenarioB['eta_gamma_1_a2']*P['gamma_1_a2']*treatment_effects_ScenarioB['eta_tau_gamma']*P['tau_gamma_a2'])
                        + P['mu_a2']))
                    /P['lambda_2_a2'])
# a3
eta_lambda_2_a3 <- ((1-((treatment_effects_ScenarioB['eta_alpha_1_a3']*P['alpha_1_a3']*treatment_effects_ScenarioB['eta_tau_alpha']*P['tau_alpha_a3'])
                        + (treatment_effects_ScenarioB['eta_beta_1_a3']*P['beta_1_a3']*treatment_effects_ScenarioB['eta_tau_beta']*P['tau_beta_a3']) 
                        + (treatment_effects_ScenarioB['eta_gamma_1_a3']*P['gamma_1_a3']*treatment_effects_ScenarioB['eta_tau_gamma']*P['tau_gamma_a3'])
                        + P['mu_a3']))
                    /P['lambda_2_a3'])


treatment_effects_ScenarioB['eta_lambda_a1'] <- unname(eta_lambda_2_a1)
treatment_effects_ScenarioB['eta_lambda_a2'] <- unname(eta_lambda_2_a2)
treatment_effects_ScenarioB['eta_lambda_a3'] <- unname(eta_lambda_2_a3)

output_ScenarioB <- T2D_TSV_engine(end_year = end_year,
                                   treatment_effects = treatment_effects_ScenarioB,
                                   multifactorial = F)

# Save results
write.csv(output_ScenarioB, file = 'outputs/output_scenario_GLP1.csv', row.names = FALSE)
cat("  Results saved to: outputs/output_scenario_GLP1.csv\n\n")

## 2.5. Scenario B1: Total Control + GLP1  + Multifactorial control #############
cat("Running Scenario B1: Total Control & GLP1 & Multifactorial control\n")

# Treatment effects with improved control rates
treatment_effects_ScenarioB1 <- c(
  eta_i_a1 = 1, eta_i_a2 = 1, eta_i_a3 = 1,
  
  # Improved control rates - placeholders
  eta_lambda_a1 = 0,
  eta_lambda_a2 = 0,
  eta_lambda_a3 = 0,
  
  eta_epsilon_a1 = 1, eta_epsilon_a2 = 1, eta_epsilon_a3 = 1,
  
  eta_alpha_1_a1 = 1, eta_alpha_2_a1 = 1,
  eta_alpha_1_a2 = 1, eta_alpha_2_a2 = 1,
  eta_alpha_1_a3 = 1, eta_alpha_2_a3 = 1,
  
  eta_beta_1_a1 = 0.973158334,  eta_beta_2_a1 = 0.960074586,  
  eta_beta_1_a2 = 0.991091723,  eta_beta_2_a2 = 0.988931669,  
  eta_beta_1_a3 = 0.996987624,  eta_beta_2_a3 = 0.995820746,  
  
  eta_gamma_1_a1 = 0.927985317, eta_gamma_2_a1 = 0.932495738, 
  eta_gamma_1_a2 = 0.91394954,  eta_gamma_2_a2 = 0.916134931,
  eta_gamma_1_a3 = 0.905634064, eta_gamma_2_a3 = 0.905386017,
  
  eta_tau_alpha = 1, eta_tau_beta = 1, eta_tau_gamma = 0.89
)

# Figure out lambdas
P <- load_params(multifactorial = T)
# a1
eta_lambda_2_a1 <- ((1-((treatment_effects_ScenarioB1['eta_alpha_1_a1']*P['alpha_1_a1']*treatment_effects_ScenarioB1['eta_tau_alpha']*P['tau_alpha_a1'])
                        + (treatment_effects_ScenarioB1['eta_beta_1_a1']*P['beta_1_a1']*treatment_effects_ScenarioB1['eta_tau_beta']*P['tau_beta_a1']) 
                        + (treatment_effects_ScenarioB1['eta_gamma_1_a1']*P['gamma_1_a1']*treatment_effects_ScenarioB1['eta_tau_gamma']*P['tau_gamma_a1'])
                        + P['mu_a1']))
                    /P['lambda_2_a1'])
# a2
eta_lambda_2_a2 <- ((1-((treatment_effects_ScenarioB1['eta_alpha_1_a2']*P['alpha_1_a2']*treatment_effects_ScenarioB1['eta_tau_alpha']*P['tau_alpha_a2'])
                        + (treatment_effects_ScenarioB1['eta_beta_1_a2']*P['beta_1_a2']*treatment_effects_ScenarioB1['eta_tau_beta']*P['tau_beta_a2']) 
                        + (treatment_effects_ScenarioB1['eta_gamma_1_a2']*P['gamma_1_a2']*treatment_effects_ScenarioB1['eta_tau_gamma']*P['tau_gamma_a2'])
                        + P['mu_a2']))
                    /P['lambda_2_a2'])
# a3
eta_lambda_2_a3 <- ((1-((treatment_effects_ScenarioB1['eta_alpha_1_a3']*P['alpha_1_a3']*treatment_effects_ScenarioB1['eta_tau_alpha']*P['tau_alpha_a3'])
                        + (treatment_effects_ScenarioB1['eta_beta_1_a3']*P['beta_1_a3']*treatment_effects_ScenarioB1['eta_tau_beta']*P['tau_beta_a3']) 
                        + (treatment_effects_ScenarioB1['eta_gamma_1_a3']*P['gamma_1_a3']*treatment_effects_ScenarioB1['eta_tau_gamma']*P['tau_gamma_a3'])
                        + P['mu_a3']))
                    /P['lambda_2_a3'])


treatment_effects_ScenarioB1['eta_lambda_a1'] <- unname(eta_lambda_2_a1)
treatment_effects_ScenarioB1['eta_lambda_a2'] <- unname(eta_lambda_2_a2)
treatment_effects_ScenarioB1['eta_lambda_a3'] <- unname(eta_lambda_2_a3)

output_ScenarioB1 <- T2D_TSV_engine(end_year = end_year,
                                   treatment_effects = treatment_effects_ScenarioB1,
                                   multifactorial = F)

# Save results
write.csv(output_ScenarioB1, file = 'outputs/output_scenario_GLP1_MF.csv', row.names = FALSE)
cat("  Results saved to: outputs/output_scenario_GLP1_MF.csv\n\n")



## 3. Summary Statistics #######################################################

cat("=== Summary Statistics ===\n\n")

# Function to print key statistics
print_summary <- function(output, scenario_name) {
  cat(sprintf("Scenario: %s\n", scenario_name))
  
  # Get final year (exclude last row which may have NA values)
  final_idx <- nrow(output) - 1
  
  # Population sizes
  cat(sprintf("  Final Year Population:\n"))
  cat(sprintf("    No Diabetes (D0): %.0f\n", sum(output[final_idx, c('D0_a1','D0_a2','D0_a3')])))
  cat(sprintf("    Uncontrolled T2D (D1): %.0f\n", sum(output[final_idx, c('D1_a1','D1_a2','D1_a3')])))
  cat(sprintf("    Controlled T2D (D2): %.0f\n", sum(output[final_idx, c('D2_a1','D2_a2','D2_a3')])))
  
  # Cumulative complications (sum over all years, excluding NA)
  if ('fC_o' %in% names(output)) {
    cat(sprintf("  Total Complications:\n"))
    cat(sprintf("    Other: %.0f\n", sum(output$fC_o, na.rm = TRUE)))
    cat(sprintf("    Macrovascular: %.0f\n", sum(output$fC_Ma, na.rm = TRUE)))
    cat(sprintf("    Microvascular: %.0f\n", sum(output$fC_Mi, na.rm = TRUE)))
  }
  
  cat("\n")
}

print_summary(output_DN, "Status Quo (Do Nothing)")
print_summary(output_ScenarioA,  "Scenario A (Total Control)")
print_summary(output_ScenarioA1, "Scenario A1 (Total Control & multifactorial)")
print_summary(output_ScenarioB,  "Scenario B (Total Control & GLP1)")
print_summary(output_ScenarioB1, "Scenario B1 (Total Control & GLP1 & multifactorial)")

cat("=== Model Run Complete ===\n")
cat("Output files created\n")
