#' script: T2D_PHA.R
#' Rproject: X_T2D_TSV.Rproj
#' Author: Thomas E. Padgett
#' Date: 2023-12-19
#' 
#' Description: Functions & wrapper for the post-hoc analysis (PHA) of the 
#' output of the T2D SDM. 
#' 

## Wrapper #####################################################################
T2D_PHA <- function(output, treatment_effects, multifactorial){
  
  # load parameters in correct format for ODE function
  parameters <- c(load_params(multifactorial),
                  treatment_effects)
  
  output <- add_postprocessed_compartments(output, parameters)
  output <- calculate_hospitalisations(output)
  output <- calculate_bed_days(output)
  output <- calculate_direct_healthcare_costs(output, parameters)
  output <- calculate_indirect_costs(output)
  output <- calculate_environment_impact(output)
  output <- calculate_disutils(output, parameters)
  output <- calculate_LYL(output)
  output <- calculate_diagnosis_costs(output)
  output <- calculate_routine_tx_costs(output)
  output <- calculate_glucose_monitoring_costs(output)
  output <- calculate_routine_appts(output)
  output <- calculate_disutils_deaths(output)
  
  return(output)
}

## Functions ###################################################################

add_postprocessed_compartments <- function(output, parameters){
  
  # Add additional faux compartments for post-processing
  # Totals in each age category
  output$f_D0_aX <- rowSums(output[,c('D0_a1','D0_a2','D0_a3')])
  output$f_D1_aX <- rowSums(output[,c('D1_a1','D1_a2','D1_a3')])
  output$f_D2_aX <- rowSums(output[,c('D2_a1','D2_a2','D2_a3')])
  
  # Total pop
  output$f_P <- rowSums(output[,c('F0',
                                  'D0_a1','D1_a1','D2_a1',
                                  'D0_a2','D1_a2','D2_a2',
                                  'D0_a3','D1_a3','D2_a3')])
  
  # make annual rather than cummulative
  output$fC_o_D1_a1 <- c(diff(output$fC_o_D1_a1), NA)
  output$fC_o_D2_a1 <- c(diff(output$fC_o_D2_a1), NA)
  output$fC_o_D1_a2 <- c(diff(output$fC_o_D1_a2), NA)
  output$fC_o_D2_a2 <- c(diff(output$fC_o_D2_a2), NA)
  output$fC_o_D1_a3 <- c(diff(output$fC_o_D1_a3), NA)
  output$fC_o_D2_a3 <- c(diff(output$fC_o_D2_a3), NA)
  
  output$fC_Ma_D1_a1 <- c(diff(output$fC_Ma_D1_a1), NA)
  output$fC_Ma_D2_a1 <- c(diff(output$fC_Ma_D2_a1), NA)
  output$fC_Ma_D1_a2 <- c(diff(output$fC_Ma_D1_a2), NA)
  output$fC_Ma_D2_a2 <- c(diff(output$fC_Ma_D2_a2), NA)
  output$fC_Ma_D1_a3 <- c(diff(output$fC_Ma_D1_a3), NA)
  output$fC_Ma_D2_a3 <- c(diff(output$fC_Ma_D2_a3), NA)
  
  output$fC_Mi_D1_a1 <- c(diff(output$fC_Mi_D1_a1), NA)
  output$fC_Mi_D2_a1 <- c(diff(output$fC_Mi_D2_a1), NA)
  output$fC_Mi_D1_a2 <- c(diff(output$fC_Mi_D1_a2), NA)
  output$fC_Mi_D2_a2 <- c(diff(output$fC_Mi_D2_a2), NA)
  output$fC_Mi_D1_a3 <- c(diff(output$fC_Mi_D1_a3), NA)
  output$fC_Mi_D2_a3 <- c(diff(output$fC_Mi_D2_a3), NA)
  
  
  # Total complications (annual) of each category
  output$fC_o <- rowSums(output[,c('fC_o_D1_a1', 'fC_o_D2_a1',
                                   'fC_o_D1_a2', 'fC_o_D2_a2',
                                   'fC_o_D1_a3', 'fC_o_D2_a3')])
  
  output$fC_Ma <- rowSums(output[,c('fC_Ma_D1_a1', 'fC_Ma_D2_a1',
                                    'fC_Ma_D1_a2', 'fC_Ma_D2_a2',
                                    'fC_Ma_D1_a3', 'fC_Ma_D2_a3')])
  
  output$fC_Mi <- rowSums(output[,c('fC_Mi_D1_a1', 'fC_Mi_D2_a1',
                                    'fC_Mi_D1_a2', 'fC_Mi_D2_a2',
                                    'fC_Mi_D1_a3', 'fC_Mi_D2_a3')])
  
  # Make deaths annual not cumulative
  output$D_acm_D0_a1 <- c(diff(output$D_acm_D0_a1), NA)
  output$D_acm_D1_a1 <- c(diff(output$D_acm_D1_a1), NA)
  output$D_acm_D2_a1 <- c(diff(output$D_acm_D2_a1), NA)
  output$D_acm_D0_a2 <- c(diff(output$D_acm_D0_a2), NA)
  output$D_acm_D1_a2 <- c(diff(output$D_acm_D1_a2), NA)
  output$D_acm_D2_a2 <- c(diff(output$D_acm_D2_a2), NA)
  output$D_acm_D0_a3 <- c(diff(output$D_acm_D0_a3), NA)
  output$D_acm_D1_a3 <- c(diff(output$D_acm_D1_a3), NA)
  output$D_acm_D2_a3 <- c(diff(output$D_acm_D2_a3), NA)
  output$D_e_D1_a1 <- c(diff(output$D_e_D1_a1), NA)
  output$D_e_D2_a1 <- c(diff(output$D_e_D2_a1), NA)
  output$D_e_D1_a2 <- c(diff(output$D_e_D1_a2), NA)
  output$D_e_D2_a2 <- c(diff(output$D_e_D2_a2), NA)
  output$D_e_D1_a3 <- c(diff(output$D_e_D1_a3), NA)
  output$D_e_D2_a3 <- c(diff(output$D_e_D2_a3), NA)
  
  # summarise deaths
  output$f_D_acm <- (output$D_acm_D0_a1
                     + output$D_acm_D1_a1
                     + output$D_acm_D2_a1
                     + output$D_acm_D0_a2
                     + output$D_acm_D1_a2
                     + output$D_acm_D2_a2
                     + output$D_acm_D0_a3
                     + output$D_acm_D1_a3
                     + output$D_acm_D2_a3)
  
  output$f_D_e <- (output$D_e_D1_a1
                   + output$D_e_D2_a1
                   + output$D_e_D1_a2
                   + output$D_e_D2_a2
                   + output$D_e_D1_a3
                   + output$D_e_D2_a3)
  
  # Approximate new diagnoses per year
  output <- approx_diagnoses(output, parameters)
    
  return(output)
}

approx_diagnoses <- function(output, parameters){
  # Approximate the number of new diagnoses of T2D per year (per age)
  output$new_diag_a1 <- (output$F0*parameters['kappa_0']*parameters['rho_1']
                         + output$F0*parameters['kappa_0']*parameters['rho_1']
                         + output$D0_a1*parameters['eta_i_a1']*parameters['lambda_1_a1'])
  
  output$new_diag_a2 <- (output$D0_a2*parameters['eta_i_a2']*parameters['lambda_1_a2'])
  
  output$new_diag_a3 <- (output$D0_a3*parameters['eta_i_a3']*parameters['lambda_1_a3'])

  return(output)
  }

calculate_hospitalisations <- function(output){
  hosp_rates <- read.csv('inputs/complication_hosp_rates.csv')
  
  output$o_fC_Ma_D1_a1_hosps <- output$fC_Ma_D1_a1 * hosp_rates$prop_hosp[hosp_rates$age == 'age1'
                                                                          & hosp_rates$dia == 'D1' 
                                                                          & hosp_rates$complication == 'macrovascular']
  output$o_fC_Ma_D2_a1_hosps <- output$fC_Ma_D2_a1 * hosp_rates$prop_hosp[hosp_rates$age == 'age1'
                                                                          & hosp_rates$dia == 'D2' 
                                                                          & hosp_rates$complication == 'macrovascular']
  output$o_fC_Ma_D1_a2_hosps <- output$fC_Ma_D1_a2 * hosp_rates$prop_hosp[hosp_rates$age == 'age2'
                                                                          & hosp_rates$dia == 'D1' 
                                                                          & hosp_rates$complication == 'macrovascular']
  output$o_fC_Ma_D2_a2_hosps <- output$fC_Ma_D2_a2 * hosp_rates$prop_hosp[hosp_rates$age == 'age2'
                                                                          & hosp_rates$dia == 'D2' 
                                                                          & hosp_rates$complication == 'macrovascular']
  output$o_fC_Ma_D1_a3_hosps <- output$fC_Ma_D1_a3 * hosp_rates$prop_hosp[hosp_rates$age == 'age3'
                                                                          & hosp_rates$dia == 'D1' 
                                                                          & hosp_rates$complication == 'macrovascular']
  output$o_fC_Ma_D2_a3_hosps <- output$fC_Ma_D2_a3 * hosp_rates$prop_hosp[hosp_rates$age == 'age3'
                                                                          & hosp_rates$dia == 'D2' 
                                                                          & hosp_rates$complication == 'macrovascular']
  
  output$o_fC_Mi_D1_a1_hosps <- output$fC_Mi_D1_a1 * hosp_rates$prop_hosp[hosp_rates$age == 'age1'
                                                                          & hosp_rates$dia == 'D1' 
                                                                          & hosp_rates$complication == 'microvascular']
  output$o_fC_Mi_D2_a1_hosps <- output$fC_Mi_D2_a1 * hosp_rates$prop_hosp[hosp_rates$age == 'age1'
                                                                          & hosp_rates$dia == 'D2' 
                                                                          & hosp_rates$complication == 'microvascular']
  output$o_fC_Mi_D1_a2_hosps <- output$fC_Mi_D1_a2 * hosp_rates$prop_hosp[hosp_rates$age == 'age2'
                                                                          & hosp_rates$dia == 'D1' 
                                                                          & hosp_rates$complication == 'microvascular']
  output$o_fC_Mi_D2_a2_hosps <- output$fC_Mi_D2_a2 * hosp_rates$prop_hosp[hosp_rates$age == 'age2'
                                                                          & hosp_rates$dia == 'D2' 
                                                                          & hosp_rates$complication == 'microvascular']
  output$o_fC_Mi_D1_a3_hosps <- output$fC_Mi_D1_a3 * hosp_rates$prop_hosp[hosp_rates$age == 'age3'
                                                                          & hosp_rates$dia == 'D1' 
                                                                          & hosp_rates$complication == 'microvascular']
  output$o_fC_Mi_D2_a3_hosps <- output$fC_Mi_D2_a3 * hosp_rates$prop_hosp[hosp_rates$age == 'age3'
                                                                          & hosp_rates$dia == 'D2' 
                                                                          & hosp_rates$complication == 'microvascular']
  
  output$o_fC_o_D1_a1_hosps <- output$fC_o_D1_a1 * hosp_rates$prop_hosp[hosp_rates$age == 'age1'
                                                                        & hosp_rates$dia == 'D1' 
                                                                        & hosp_rates$complication == 'other']
  output$o_fC_o_D2_a1_hosps <- output$fC_o_D2_a1 * hosp_rates$prop_hosp[hosp_rates$age == 'age1'
                                                                        & hosp_rates$dia == 'D2' 
                                                                        & hosp_rates$complication == 'other']
  output$o_fC_o_D1_a2_hosps <- output$fC_o_D1_a2 * hosp_rates$prop_hosp[hosp_rates$age == 'age2'
                                                                        & hosp_rates$dia == 'D1' 
                                                                        & hosp_rates$complication == 'other']
  output$o_fC_o_D2_a2_hosps <- output$fC_o_D2_a2 * hosp_rates$prop_hosp[hosp_rates$age == 'age2'
                                                                        & hosp_rates$dia == 'D2' 
                                                                        & hosp_rates$complication == 'other']
  output$o_fC_o_D1_a3_hosps <- output$fC_o_D1_a3 * hosp_rates$prop_hosp[hosp_rates$age == 'age3'
                                                                        & hosp_rates$dia == 'D1' 
                                                                        & hosp_rates$complication == 'other']
  output$o_fC_o_D2_a3_hosps <- output$fC_o_D2_a3 * hosp_rates$prop_hosp[hosp_rates$age == 'age3'
                                                                        & hosp_rates$dia == 'D2' 
                                                                        & hosp_rates$complication == 'other']
  
  output$o_fC_all_hosps <- (output$o_fC_o_D1_a1_hosps
                            + output$o_fC_o_D2_a1_hosps
                            + output$o_fC_o_D1_a2_hosps
                            + output$o_fC_o_D2_a2_hosps
                            + output$o_fC_o_D1_a3_hosps
                            + output$o_fC_o_D2_a3_hosps
                            + output$o_fC_Ma_D1_a1_hosps
                            + output$o_fC_Ma_D2_a1_hosps
                            + output$o_fC_Ma_D1_a2_hosps
                            + output$o_fC_Ma_D2_a2_hosps
                            + output$o_fC_Ma_D1_a3_hosps
                            + output$o_fC_Ma_D2_a3_hosps
                            + output$o_fC_Mi_D1_a1_hosps
                            + output$o_fC_Mi_D2_a1_hosps
                            + output$o_fC_Mi_D1_a2_hosps
                            + output$o_fC_Mi_D2_a2_hosps
                            + output$o_fC_Mi_D1_a3_hosps
                            + output$o_fC_Mi_D2_a3_hosps)
  
  return(output) 
}

calculate_bed_days <- function(output){
  hosp_LOS <- read.csv('inputs/complication_hosp_LOS.csv')
  
  output$o_fC_Ma_D1_a1_beds <- output$o_fC_Ma_D1_a1_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age1'
                                                                         & hosp_LOS$dia == 'D1' 
                                                                         & hosp_LOS$complication == 'macrovascular']
  output$o_fC_Ma_D2_a1_beds <- output$o_fC_Ma_D2_a1_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age1'
                                                                         & hosp_LOS$dia == 'D2' 
                                                                         & hosp_LOS$complication == 'macrovascular']
  output$o_fC_Ma_D1_a2_beds <- output$o_fC_Ma_D1_a2_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age2'
                                                                         & hosp_LOS$dia == 'D1' 
                                                                         & hosp_LOS$complication == 'macrovascular']
  output$o_fC_Ma_D2_a2_beds <- output$o_fC_Ma_D2_a2_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age2'
                                                                         & hosp_LOS$dia == 'D2' 
                                                                         & hosp_LOS$complication == 'macrovascular']
  output$o_fC_Ma_D1_a3_beds <- output$o_fC_Ma_D1_a3_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age3'
                                                                         & hosp_LOS$dia == 'D1' 
                                                                         & hosp_LOS$complication == 'macrovascular']
  output$o_fC_Ma_D2_a3_beds <- output$o_fC_Ma_D2_a3_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age3'
                                                                         & hosp_LOS$dia == 'D2' 
                                                                         & hosp_LOS$complication == 'macrovascular']
  
  output$o_fC_Mi_D1_a1_beds <- output$o_fC_Mi_D1_a1_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age1'
                                                                         & hosp_LOS$dia == 'D1' 
                                                                         & hosp_LOS$complication == 'microvascular']
  output$o_fC_Mi_D2_a1_beds <- output$o_fC_Mi_D2_a1_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age1'
                                                                         & hosp_LOS$dia == 'D2' 
                                                                         & hosp_LOS$complication == 'microvascular']
  output$o_fC_Mi_D1_a2_beds <- output$o_fC_Mi_D1_a2_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age2'
                                                                         & hosp_LOS$dia == 'D1' 
                                                                         & hosp_LOS$complication == 'microvascular']
  output$o_fC_Mi_D2_a2_beds <- output$o_fC_Mi_D2_a2_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age2'
                                                                         & hosp_LOS$dia == 'D2' 
                                                                         & hosp_LOS$complication == 'microvascular']
  output$o_fC_Mi_D1_a3_beds <- output$o_fC_Mi_D1_a3_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age3'
                                                                         & hosp_LOS$dia == 'D1' 
                                                                         & hosp_LOS$complication == 'microvascular']
  output$o_fC_Mi_D2_a3_beds <- output$o_fC_Mi_D2_a3_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age3'
                                                                         & hosp_LOS$dia == 'D2' 
                                                                         & hosp_LOS$complication == 'microvascular']
  
  output$o_fC_o_D1_a1_beds <- output$o_fC_o_D1_a1_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age1'
                                                                       & hosp_LOS$dia == 'D1' 
                                                                       & hosp_LOS$complication == 'other']
  output$o_fC_o_D2_a1_beds <- output$o_fC_o_D2_a1_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age1'
                                                                       & hosp_LOS$dia == 'D2' 
                                                                       & hosp_LOS$complication == 'other']
  output$o_fC_o_D1_a2_beds <- output$o_fC_o_D1_a2_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age2'
                                                                       & hosp_LOS$dia == 'D1' 
                                                                       & hosp_LOS$complication == 'other']
  output$o_fC_o_D2_a2_beds <- output$o_fC_o_D2_a2_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age2'
                                                                       & hosp_LOS$dia == 'D2' 
                                                                       & hosp_LOS$complication == 'other']
  output$o_fC_o_D1_a3_beds <- output$o_fC_o_D1_a3_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age3'
                                                                       & hosp_LOS$dia == 'D1' 
                                                                       & hosp_LOS$complication == 'other']
  output$o_fC_o_D2_a3_beds <- output$o_fC_o_D2_a3_hosps * hosp_LOS$LOS[hosp_LOS$age == 'age3'
                                                                       & hosp_LOS$dia == 'D2' 
                                                                       & hosp_LOS$complication == 'other']
  
  output$o_fC_all_beds <- (output$o_fC_o_D1_a1_beds
                           + output$o_fC_o_D2_a1_beds
                           + output$o_fC_o_D1_a2_beds
                           + output$o_fC_o_D2_a2_beds
                           + output$o_fC_o_D1_a3_beds
                           + output$o_fC_o_D2_a3_beds
                           + output$o_fC_Ma_D1_a1_beds
                           + output$o_fC_Ma_D2_a1_beds
                           + output$o_fC_Ma_D1_a2_beds
                           + output$o_fC_Ma_D2_a2_beds
                           + output$o_fC_Ma_D1_a3_beds
                           + output$o_fC_Ma_D2_a3_beds
                           + output$o_fC_Mi_D1_a1_beds
                           + output$o_fC_Mi_D2_a1_beds
                           + output$o_fC_Mi_D1_a2_beds
                           + output$o_fC_Mi_D2_a2_beds
                           + output$o_fC_Mi_D1_a3_beds
                           + output$o_fC_Mi_D2_a3_beds)
  return(output) 
}

calculate_direct_healthcare_costs <- function(output, parameters){
  direct_costs <- read.csv('inputs/complication_direct_costs.csv')
  
  # subsequent maintenance years defined as 25% of 
  # life expectancy given T2D
  LYL <- read.csv('inputs/life_years_lost.csv')
  
  macro_sub_years_a1 <- 0.25*(LYL$exp_t2d[LYL$age == 'a1'] - LYL$avg_age[LYL$age == 'a1'])
  macro_sub_years_a2 <- 0.25*(LYL$exp_t2d[LYL$age == 'a2'] - LYL$avg_age[LYL$age == 'a2'])
  macro_sub_years_a3 <- 0.25*(LYL$exp_t2d[LYL$age == 'a3'] - LYL$avg_age[LYL$age == 'a3'])
  micro_sub_years_a1 <- 0.25*(LYL$exp_t2d[LYL$age == 'a1'] - LYL$avg_age[LYL$age == 'a1'])
  micro_sub_years_a2 <- 0.25*(LYL$exp_t2d[LYL$age == 'a2'] - LYL$avg_age[LYL$age == 'a2'])
  micro_sub_years_a3 <- 0.25*(LYL$exp_t2d[LYL$age == 'a3'] - LYL$avg_age[LYL$age == 'a3'])
  other_sub_years_a1 <- 0.25*(LYL$exp_t2d[LYL$age == 'a1'] - LYL$avg_age[LYL$age == 'a1'])
  other_sub_years_a2 <- 0.25*(LYL$exp_t2d[LYL$age == 'a2'] - LYL$avg_age[LYL$age == 'a2'])
  other_sub_years_a3 <- 0.25*(LYL$exp_t2d[LYL$age == 'a3'] - LYL$avg_age[LYL$age == 'a3'])
    
  ############ MACRO EVENTS
  output$o_fC_Ma_D1_a1_directCosts <- (((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a1']))
    * output$fC_Ma_D1_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a1'])
  * output$fC_Ma_D1_a1 
  * direct_costs$cost_fatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'macrovascular']
  ) + 
    macro_sub_years_a1 * (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a1']))
  * output$fC_Ma_D1_a1 
  * direct_costs$cost_maintain[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'macrovascular']
  )
  
  
  output$o_fC_Ma_D2_a1_directCosts <- (((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a1']))
    * output$fC_Ma_D2_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a1'])
  * output$fC_Ma_D2_a1 
  * direct_costs$cost_fatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'macrovascular']
  ) + 
    macro_sub_years_a1 * (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a1']))
  * output$fC_Ma_D2_a1 
  * direct_costs$cost_maintain[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'macrovascular']
  )
  
  
  output$o_fC_Ma_D1_a2_directCosts <- (((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a2']))
    * output$fC_Ma_D1_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a2'])
  * output$fC_Ma_D1_a2 
  * direct_costs$cost_fatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'macrovascular']
  ) + 
    macro_sub_years_a2 * (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a2']))
  * output$fC_Ma_D1_a2 
  * direct_costs$cost_maintain[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'macrovascular']
  )
  
  
  output$o_fC_Ma_D2_a2_directCosts <- (((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a2']))
    * output$fC_Ma_D2_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a2'])
  * output$fC_Ma_D2_a2 
  * direct_costs$cost_fatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'macrovascular']
  ) + 
    macro_sub_years_a2 * (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a2']))
  * output$fC_Ma_D2_a2 
  * direct_costs$cost_maintain[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'macrovascular']
  )
  
  
  output$o_fC_Ma_D1_a3_directCosts <- (((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a3']))
    * output$fC_Ma_D1_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a3'])
  * output$fC_Ma_D1_a3 
  * direct_costs$cost_fatal[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'macrovascular']
  ) + 
    macro_sub_years_a3 * (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a3']))
  * output$fC_Ma_D1_a3 
  * direct_costs$cost_maintain[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'macrovascular']
  ) 
  
                                 
  output$o_fC_Ma_D2_a3_directCosts <- (((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a3']))
    * output$fC_Ma_D2_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a3'])
  * output$fC_Ma_D2_a3 
  * direct_costs$cost_fatal[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'macrovascular']
  ) + 
    macro_sub_years_a3 * (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a3']))
  * output$fC_Ma_D2_a3 
  * direct_costs$cost_maintain[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'macrovascular']
  ) 
  
  
  
  ############ MICRO EVENTS
  output$o_fC_Mi_D1_a1_directCosts <- (((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a1']))
    * output$fC_Mi_D1_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a1'])
  * output$fC_Mi_D1_a1 
  * direct_costs$cost_fatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'microvascular']
  ) + 
    micro_sub_years_a1 * (1-(parameters['eta_tau_beta']*parameters['tau_beta_a1']))
  * output$fC_Mi_D1_a1 
  * direct_costs$cost_maintain[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'microvascular']
  )
  
  
  output$o_fC_Mi_D2_a1_directCosts <- (((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a1']))
    * output$fC_Mi_D2_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a1'])
  * output$fC_Mi_D2_a1 
  * direct_costs$cost_fatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'microvascular']
  ) + 
    micro_sub_years_a1 * (1-(parameters['eta_tau_beta']*parameters['tau_beta_a1']))
  * output$fC_Mi_D2_a1 
  * direct_costs$cost_maintain[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'microvascular']
  )
  
  
  output$o_fC_Mi_D1_a2_directCosts <- (((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a2']))
    * output$fC_Mi_D1_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a2'])
  * output$fC_Mi_D1_a2 
  * direct_costs$cost_fatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'microvascular']
  ) + 
    micro_sub_years_a2 * (1-(parameters['eta_tau_beta']*parameters['tau_beta_a2']))
  * output$fC_Mi_D1_a2 
  * direct_costs$cost_maintain[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'microvascular']
  )
  
  
  output$o_fC_Mi_D2_a2_directCosts <- (((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a2']))
    * output$fC_Mi_D2_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a2'])
  * output$fC_Mi_D2_a2 
  * direct_costs$cost_fatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'microvascular']
  ) + 
    micro_sub_years_a2 * (1-(parameters['eta_tau_beta']*parameters['tau_beta_a2']))
  * output$fC_Mi_D2_a2 
  * direct_costs$cost_maintain[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'microvascular']
  )
  
  
  output$o_fC_Mi_D1_a3_directCosts <- (((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a3']))
    * output$fC_Mi_D1_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a3'])
  * output$fC_Mi_D1_a3 
  * direct_costs$cost_fatal[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'microvascular']
  ) + 
    micro_sub_years_a3 * (1-(parameters['eta_tau_beta']*parameters['tau_beta_a3']))
  * output$fC_Mi_D1_a3 
  * direct_costs$cost_maintain[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'microvascular']
  )
  
  output$o_fC_Mi_D2_a3_directCosts <- (((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a3']))
    * output$fC_Mi_D2_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a3'])
  * output$fC_Mi_D2_a3 
  * direct_costs$cost_fatal[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'microvascular']
  ) + 
    micro_sub_years_a3 * (1-(parameters['eta_tau_beta']*parameters['tau_beta_a3']))
  * output$fC_Mi_D2_a3 
  * direct_costs$cost_maintain[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'microvascular']
  )
  
  
  ############ OTHER EVENTS
  output$o_fC_o_D1_a1_directCosts <- (((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a1']))
    * output$fC_o_D1_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a1'])
  * output$fC_o_D1_a1 
  * direct_costs$cost_fatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'other']
  ) + 
    other_sub_years_a1 * (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a1']))
  * output$fC_o_D1_a1 
  * direct_costs$cost_maintain[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'other']
  )
  
  
  output$o_fC_o_D2_a1_directCosts <- (((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a1']))
    * output$fC_o_D2_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a1'])
  * output$fC_o_D2_a1 
  * direct_costs$cost_fatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'other']
  ) + 
    other_sub_years_a1 * (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a1']))
  * output$fC_o_D2_a1 
  * direct_costs$cost_maintain[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'other']
  )

  
  output$o_fC_o_D1_a2_directCosts <- (((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a2']))
    * output$fC_o_D1_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a2'])
  * output$fC_o_D1_a2 
  * direct_costs$cost_fatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'other']
  ) + 
    other_sub_years_a2 * (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a2']))
  * output$fC_o_D1_a2 
  * direct_costs$cost_maintain[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'other']
  )
  
  
  output$o_fC_o_D2_a2_directCosts <- (((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a2']))
    * output$fC_o_D2_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a2'])
  * output$fC_o_D2_a2 
  * direct_costs$cost_fatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'other']
  ) + 
    other_sub_years_a2 * (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a2']))
  * output$fC_o_D2_a2 
  * direct_costs$cost_maintain[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'other']
  )
  
  
  output$o_fC_o_D1_a3_directCosts <- (((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a3']))
    * output$fC_o_D1_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a3'])
  * output$fC_o_D1_a3 
  * direct_costs$cost_fatal[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'other']
  ) + 
    other_sub_years_a3 * (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a3']))
  * output$fC_o_D1_a3 
  * direct_costs$cost_maintain[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'other']
  )
  
  
  output$o_fC_o_D2_a3_directCosts <- (((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a3']))
    * output$fC_o_D2_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a3'])
  * output$fC_o_D2_a3 
  * direct_costs$cost_fatal[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'other']
  ) + 
    other_sub_years_a3 * (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a3']))
  * output$fC_o_D2_a3 
  * direct_costs$cost_maintain[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'other']
  )
  
  
  output$o_fC_all_directCosts <- (output$o_fC_o_D1_a1_directCosts
                                  + output$o_fC_o_D2_a1_directCosts
                                  + output$o_fC_o_D1_a2_directCosts
                                  + output$o_fC_o_D2_a2_directCosts
                                  + output$o_fC_o_D1_a3_directCosts
                                  + output$o_fC_o_D2_a3_directCosts
                                  + output$o_fC_Ma_D1_a1_directCosts
                                  + output$o_fC_Ma_D2_a1_directCosts
                                  + output$o_fC_Ma_D1_a2_directCosts
                                  + output$o_fC_Ma_D2_a2_directCosts
                                  + output$o_fC_Ma_D1_a3_directCosts
                                  + output$o_fC_Ma_D2_a3_directCosts
                                  + output$o_fC_Mi_D1_a1_directCosts
                                  + output$o_fC_Mi_D2_a1_directCosts
                                  + output$o_fC_Mi_D1_a2_directCosts
                                  + output$o_fC_Mi_D2_a2_directCosts
                                  + output$o_fC_Mi_D1_a3_directCosts
                                  + output$o_fC_Mi_D2_a3_directCosts)
  
  return(output)
}

calculate_indirect_costs <- function(output){
  indirect_costs <- read.csv('inputs/complication_indirect_costs.csv')
  
  output$o_NoComp_D1_a1_incomeLoss <- output$D1_a1 * indirect_costs$income_loss[indirect_costs$age == 'age1'
                                                                                & indirect_costs$dia == 'D1'
                                                                                & indirect_costs$complication == 'none']
  output$o_NoComp_D2_a1_incomeLoss <- output$D2_a1 * indirect_costs$income_loss[indirect_costs$age == 'age1'
                                                                                & indirect_costs$dia == 'D2'
                                                                                & indirect_costs$complication == 'none']
  output$o_NoComp_D1_a2_incomeLoss <- output$D1_a2 * indirect_costs$income_loss[indirect_costs$age == 'age2'
                                                                                & indirect_costs$dia == 'D1'
                                                                                & indirect_costs$complication == 'none']
  output$o_NoComp_D2_a2_incomeLoss <- output$D2_a2 * indirect_costs$income_loss[indirect_costs$age == 'age2'
                                                                                & indirect_costs$dia == 'D2'
                                                                                & indirect_costs$complication == 'none']
  
  output$o_fC_Ma_D1_a1_incomeLoss <- output$fC_Ma_D1_a1 * indirect_costs$income_loss[indirect_costs$age == 'age1'
                                                                                     & indirect_costs$dia == 'D1' 
                                                                                     & indirect_costs$complication == 'macrovascular']
  output$o_fC_Ma_D2_a1_incomeLoss <- output$fC_Ma_D2_a1 * indirect_costs$income_loss[indirect_costs$age == 'age1'
                                                                                     & indirect_costs$dia == 'D2' 
                                                                                     & indirect_costs$complication == 'macrovascular']
  output$o_fC_Ma_D1_a2_incomeLoss <- output$fC_Ma_D1_a2 * indirect_costs$income_loss[indirect_costs$age == 'age2'
                                                                                     & indirect_costs$dia == 'D1' 
                                                                                     & indirect_costs$complication == 'macrovascular']
  output$o_fC_Ma_D2_a2_incomeLoss <- output$fC_Ma_D2_a2 * indirect_costs$income_loss[indirect_costs$age == 'age2'
                                                                                     & indirect_costs$dia == 'D2' 
                                                                                     & indirect_costs$complication == 'macrovascular']
  
  
  output$o_fC_Mi_D1_a1_incomeLoss <- output$fC_Mi_D1_a1 * indirect_costs$income_loss[indirect_costs$age == 'age1'
                                                                                     & indirect_costs$dia == 'D1' 
                                                                                     & indirect_costs$complication == 'microvascular']
  output$o_fC_Mi_D2_a1_incomeLoss <- output$fC_Mi_D2_a1 * indirect_costs$income_loss[indirect_costs$age == 'age1'
                                                                                     & indirect_costs$dia == 'D2' 
                                                                                     & indirect_costs$complication == 'microvascular']
  output$o_fC_Mi_D1_a2_incomeLoss <- output$fC_Mi_D1_a2 * indirect_costs$income_loss[indirect_costs$age == 'age2'
                                                                                     & indirect_costs$dia == 'D1' 
                                                                                     & indirect_costs$complication == 'microvascular']
  output$o_fC_Mi_D2_a2_incomeLoss <- output$fC_Mi_D2_a2 * indirect_costs$income_loss[indirect_costs$age == 'age2'
                                                                                     & indirect_costs$dia == 'D2' 
                                                                                     & indirect_costs$complication == 'microvascular']
  
  
  output$o_fC_o_D1_a1_incomeLoss <- output$fC_o_D1_a1 * indirect_costs$income_loss[indirect_costs$age == 'age1'
                                                                                   & indirect_costs$dia == 'D1' 
                                                                                   & indirect_costs$complication == 'other']
  output$o_fC_o_D2_a1_incomeLoss <- output$fC_o_D2_a1 * indirect_costs$income_loss[indirect_costs$age == 'age1'
                                                                                   & indirect_costs$dia == 'D2' 
                                                                                   & indirect_costs$complication == 'other']
  output$o_fC_o_D1_a2_incomeLoss <- output$fC_o_D1_a2 * indirect_costs$income_loss[indirect_costs$age == 'age2'
                                                                                   & indirect_costs$dia == 'D1' 
                                                                                   & indirect_costs$complication == 'other']
  output$o_fC_o_D2_a2_incomeLoss <- output$fC_o_D2_a2 * indirect_costs$income_loss[indirect_costs$age == 'age2'
                                                                                   & indirect_costs$dia == 'D2' 
                                                                                   & indirect_costs$complication == 'other']
  
  output$o_fC_all_incomeLoss <- (output$o_fC_o_D1_a1_incomeLoss
                                 + output$o_fC_o_D2_a1_incomeLoss
                                 + output$o_fC_o_D1_a2_incomeLoss
                                 + output$o_fC_o_D2_a2_incomeLoss
                                 + output$o_fC_Ma_D1_a1_incomeLoss
                                 + output$o_fC_Ma_D2_a1_incomeLoss
                                 + output$o_fC_Ma_D1_a2_incomeLoss
                                 + output$o_fC_Ma_D2_a2_incomeLoss
                                 + output$o_fC_Mi_D1_a1_incomeLoss
                                 + output$o_fC_Mi_D2_a1_incomeLoss
                                 + output$o_fC_Mi_D1_a2_incomeLoss
                                 + output$o_fC_Mi_D2_a2_incomeLoss
                                 + output$o_NoComp_D1_a1_incomeLoss
                                 + output$o_NoComp_D2_a1_incomeLoss
                                 + output$o_NoComp_D1_a2_incomeLoss
                                 + output$o_NoComp_D2_a2_incomeLoss)
  
  output$o_NoComp_D1_a1_absenteeism <- output$D1_a1 * indirect_costs$absenteeism[indirect_costs$age == 'age1'
                                                                                 & indirect_costs$dia == 'D1'
                                                                                 & indirect_costs$complication == 'none']
  output$o_NoComp_D2_a1_absenteeism <- output$D2_a1 * indirect_costs$absenteeism[indirect_costs$age == 'age1'
                                                                                 & indirect_costs$dia == 'D2'
                                                                                 & indirect_costs$complication == 'none']
  output$o_NoComp_D1_a2_absenteeism <- output$D1_a2 * indirect_costs$absenteeism[indirect_costs$age == 'age2'
                                                                                 & indirect_costs$dia == 'D1'
                                                                                 & indirect_costs$complication == 'none']
  output$o_NoComp_D2_a2_absenteeism <- output$D2_a2 * indirect_costs$absenteeism[indirect_costs$age == 'age2'
                                                                                 & indirect_costs$dia == 'D2'
                                                                                 & indirect_costs$complication == 'none']
  
  output$o_fC_Ma_D1_a1_absenteeism <- output$fC_Ma_D1_a1 * indirect_costs$absenteeism[indirect_costs$age == 'age1'
                                                                                      & indirect_costs$dia == 'D1' 
                                                                                      & indirect_costs$complication == 'macrovascular']
  output$o_fC_Ma_D2_a1_absenteeism <- output$fC_Ma_D2_a1 * indirect_costs$absenteeism[indirect_costs$age == 'age1'
                                                                                      & indirect_costs$dia == 'D2' 
                                                                                      & indirect_costs$complication == 'macrovascular']
  output$o_fC_Ma_D1_a2_absenteeism <- output$fC_Ma_D1_a2 * indirect_costs$absenteeism[indirect_costs$age == 'age2'
                                                                                      & indirect_costs$dia == 'D1' 
                                                                                      & indirect_costs$complication == 'macrovascular']
  output$o_fC_Ma_D2_a2_absenteeism <- output$fC_Ma_D2_a2 * indirect_costs$absenteeism[indirect_costs$age == 'age2'
                                                                                      & indirect_costs$dia == 'D2' 
                                                                                      & indirect_costs$complication == 'macrovascular']
  
  
  output$o_fC_Mi_D1_a1_absenteeism <- output$fC_Mi_D1_a1 * indirect_costs$absenteeism[indirect_costs$age == 'age1'
                                                                                      & indirect_costs$dia == 'D1' 
                                                                                      & indirect_costs$complication == 'microvascular']
  output$o_fC_Mi_D2_a1_absenteeism <- output$fC_Mi_D2_a1 * indirect_costs$absenteeism[indirect_costs$age == 'age1'
                                                                                      & indirect_costs$dia == 'D2' 
                                                                                      & indirect_costs$complication == 'microvascular']
  output$o_fC_Mi_D1_a2_absenteeism <- output$fC_Mi_D1_a2 * indirect_costs$absenteeism[indirect_costs$age == 'age2'
                                                                                      & indirect_costs$dia == 'D1' 
                                                                                      & indirect_costs$complication == 'microvascular']
  output$o_fC_Mi_D2_a2_absenteeism <- output$fC_Mi_D2_a2 * indirect_costs$absenteeism[indirect_costs$age == 'age2'
                                                                                      & indirect_costs$dia == 'D2' 
                                                                                      & indirect_costs$complication == 'microvascular']
  
  
  output$o_fC_o_D1_a1_absenteeism <- output$fC_o_D1_a1 * indirect_costs$absenteeism[indirect_costs$age == 'age1'
                                                                                    & indirect_costs$dia == 'D1' 
                                                                                    & indirect_costs$complication == 'other']
  output$o_fC_o_D2_a1_absenteeism <- output$fC_o_D2_a1 * indirect_costs$absenteeism[indirect_costs$age == 'age1'
                                                                                    & indirect_costs$dia == 'D2' 
                                                                                    & indirect_costs$complication == 'other']
  output$o_fC_o_D1_a2_absenteeism <- output$fC_o_D1_a2 * indirect_costs$absenteeism[indirect_costs$age == 'age2'
                                                                                    & indirect_costs$dia == 'D1' 
                                                                                    & indirect_costs$complication == 'other']
  output$o_fC_o_D2_a2_absenteeism <- output$fC_o_D2_a2 * indirect_costs$absenteeism[indirect_costs$age == 'age2'
                                                                                    & indirect_costs$dia == 'D2' 
                                                                                    & indirect_costs$complication == 'other']
  
  output$o_fC_all_absenteeism <- (output$o_fC_o_D1_a1_absenteeism
                                  + output$o_fC_o_D2_a1_absenteeism
                                  + output$o_fC_o_D1_a2_absenteeism
                                  + output$o_fC_o_D2_a2_absenteeism
                                  + output$o_fC_Ma_D1_a1_absenteeism
                                  + output$o_fC_Ma_D2_a1_absenteeism
                                  + output$o_fC_Ma_D1_a2_absenteeism
                                  + output$o_fC_Ma_D2_a2_absenteeism
                                  + output$o_fC_Mi_D1_a1_absenteeism
                                  + output$o_fC_Mi_D2_a1_absenteeism
                                  + output$o_fC_Mi_D1_a2_absenteeism
                                  + output$o_fC_Mi_D2_a2_absenteeism
                                  + output$o_NoComp_D1_a1_absenteeism
                                  + output$o_NoComp_D2_a1_absenteeism
                                  + output$o_NoComp_D1_a2_absenteeism
                                  + output$o_NoComp_D2_a2_absenteeism)
  
  return(output)
}

calculate_environment_impact <- function(output){
  env_impacts <- read.csv('inputs/env_impacts.csv')
  
  output$o_co2e_emissions_D1_a1 <- (output$o_fC_Ma_D1_a1_beds 
                                          + output$o_fC_Mi_D1_a1_beds
                                          + output$o_fC_o_D1_a1_beds) * env_impacts$val_.kg.[env_impacts$category == 'co2e per low intensity bed day']
  
  output$o_co2e_emissions_D2_a1 <- (output$o_fC_Ma_D2_a1_beds 
                                          + output$o_fC_Mi_D2_a1_beds
                                          + output$o_fC_o_D2_a1_beds) * env_impacts$val_.kg.[env_impacts$category == 'co2e per low intensity bed day']
  
  output$o_co2e_emissions_D1_a2 <- (output$o_fC_Ma_D1_a2_beds 
                                          + output$o_fC_Mi_D1_a2_beds
                                          + output$o_fC_o_D1_a2_beds) * env_impacts$val_.kg.[env_impacts$category == 'co2e per low intensity bed day']
  
  output$o_co2e_emissions_D2_a2 <- (output$o_fC_Ma_D2_a2_beds 
                                          + output$o_fC_Mi_D2_a2_beds
                                          + output$o_fC_o_D2_a2_beds) * env_impacts$val_.kg.[env_impacts$category == 'co2e per low intensity bed day']
  
  output$o_co2e_emissions_D1_a3 <- (output$o_fC_Ma_D1_a3_beds 
                                          + output$o_fC_Mi_D1_a3_beds
                                          + output$o_fC_o_D1_a3_beds) * env_impacts$val_.kg.[env_impacts$category == 'co2e per low intensity bed day']
  
  output$o_co2e_emissions_D2_a3 <- (output$o_fC_Ma_D2_a3_beds 
                                          + output$o_fC_Mi_D2_a3_beds
                                          + output$o_fC_o_D2_a3_beds) * env_impacts$val_.kg.[env_impacts$category == 'co2e per low intensity bed day']
  
  output$o_co2e_emissions_total <- (output$o_co2e_emissions_D1_a1
                                    + output$o_co2e_emissions_D2_a1
                                    + output$o_co2e_emissions_D1_a2
                                    + output$o_co2e_emissions_D2_a2
                                    + output$o_co2e_emissions_D1_a3
                                    + output$o_co2e_emissions_D2_a3)
  
  
  output$o_water_consumption_D1_a1 <- (output$o_fC_Ma_D1_a1_beds 
                                       + output$o_fC_Mi_D1_a1_beds
                                       + output$o_fC_o_D1_a1_beds) * env_impacts$val_.kg.[env_impacts$category == 'water per low intensity bed day']
  
  output$o_water_consumption_D2_a1 <- (output$o_fC_Ma_D2_a1_beds 
                                       + output$o_fC_Mi_D2_a1_beds
                                       + output$o_fC_o_D2_a1_beds) * env_impacts$val_.kg.[env_impacts$category == 'water per low intensity bed day']
  
  output$o_water_consumption_D1_a2 <- (output$o_fC_Ma_D1_a2_beds 
                                       + output$o_fC_Mi_D1_a2_beds
                                       + output$o_fC_o_D1_a2_beds) * env_impacts$val_.kg.[env_impacts$category == 'water per low intensity bed day']
  
  output$o_water_consumption_D2_a2 <- (output$o_fC_Ma_D2_a2_beds 
                                       + output$o_fC_Mi_D2_a2_beds
                                       + output$o_fC_o_D2_a2_beds) * env_impacts$val_.kg.[env_impacts$category == 'water per low intensity bed day']
  
  output$o_water_consumption_D1_a3 <- (output$o_fC_Ma_D1_a3_beds 
                                       + output$o_fC_Mi_D1_a3_beds
                                       + output$o_fC_o_D1_a3_beds) * env_impacts$val_.kg.[env_impacts$category == 'water per low intensity bed day']
  
  output$o_water_consumption_D2_a3 <- (output$o_fC_Ma_D2_a3_beds 
                                       + output$o_fC_Mi_D2_a3_beds
                                       + output$o_fC_o_D2_a3_beds) * env_impacts$val_.kg.[env_impacts$category == 'water per low intensity bed day']
  
  output$o_water_consumption_total <- (output$o_water_consumption_D1_a1
                                        + output$o_water_consumption_D2_a1
                                        + output$o_water_consumption_D1_a2
                                        + output$o_water_consumption_D2_a2
                                        + output$o_water_consumption_D1_a3
                                        + output$o_water_consumption_D2_a3)
  
  # assumes all bed days are low intensity
  return(output)
}

calculate_disutils <- function(output, parameters){
  disutils <- read.csv('inputs/complication_disutilities.csv')
  
  # MACRO EVENTS
  output$o_fC_Ma_D1_a1_disutil_acute <- (
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a1'])) 
    * output$fC_Ma_D1_a1 
    * disutils$disutil_non_fatal[disutils$age == 'age1'
                                 & disutils$dia == 'D1' 
                                 & disutils$complication == 'macrovascular']
  )
  
  output$o_fC_Ma_D2_a1_disutil_acute <- (
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a1'])) 
    * output$fC_Ma_D2_a1 
    * disutils$disutil_non_fatal[disutils$age == 'age1'
                                 & disutils$dia == 'D2' 
                                 & disutils$complication == 'macrovascular']
  )
  
  output$o_fC_Ma_D1_a2_disutil_acute <- (
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a2'])) 
    * output$fC_Ma_D1_a2 
    * disutils$disutil_non_fatal[disutils$age == 'age2'
                                 & disutils$dia == 'D1' 
                                 & disutils$complication == 'macrovascular']
  )
  
  output$o_fC_Ma_D2_a2_disutil_acute <- (
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a2'])) 
    * output$fC_Ma_D2_a2 
    * disutils$disutil_non_fatal[disutils$age == 'age2'
                                 & disutils$dia == 'D2' 
                                 & disutils$complication == 'macrovascular']
  )
  
  output$o_fC_Ma_D1_a3_disutil_acute <- (
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a3'])) 
    * output$fC_Ma_D1_a3 
    * disutils$disutil_non_fatal[disutils$age == 'age3'
                                 & disutils$dia == 'D1' 
                                 & disutils$complication == 'macrovascular']
  )
  
  output$o_fC_Ma_D2_a3_disutil_acute <- (
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a3'])) 
    * output$fC_Ma_D2_a3 
    * disutils$disutil_non_fatal[disutils$age == 'age3'
                                 & disutils$dia == 'D2' 
                                 & disutils$complication == 'macrovascular']
  )
  
  
  # MICRO EVENTS
  output$o_fC_Mi_D1_a1_disutil_acute <- (
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a1'])) 
    * output$fC_Mi_D1_a1 
    * disutils$disutil_non_fatal[disutils$age == 'age1'
                                 & disutils$dia == 'D1' 
                                 & disutils$complication == 'microvascular']
  )
  
  output$o_fC_Mi_D2_a1_disutil_acute <- (
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a1'])) 
    * output$fC_Mi_D2_a1 
    * disutils$disutil_non_fatal[disutils$age == 'age1'
                                 & disutils$dia == 'D2' 
                                 & disutils$complication == 'microvascular']
  )
  
  output$o_fC_Mi_D1_a2_disutil_acute <- (
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a2'])) 
    * output$fC_Mi_D1_a2 
    * disutils$disutil_non_fatal[disutils$age == 'age2'
                                 & disutils$dia == 'D1' 
                                 & disutils$complication == 'microvascular']
  )
  
  output$o_fC_Mi_D2_a2_disutil_acute <- (
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a2'])) 
    * output$fC_Mi_D2_a2 
    * disutils$disutil_non_fatal[disutils$age == 'age2'
                                 & disutils$dia == 'D2' 
                                 & disutils$complication == 'microvascular']
  )
  
  output$o_fC_Mi_D1_a3_disutil_acute <- (
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a3'])) 
    * output$fC_Mi_D1_a3 
    * disutils$disutil_non_fatal[disutils$age == 'age3'
                                 & disutils$dia == 'D1' 
                                 & disutils$complication == 'microvascular']
  )
  
  output$o_fC_Mi_D2_a3_disutil_acute <- (
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a3'])) 
    * output$fC_Mi_D2_a3 
    * disutils$disutil_non_fatal[disutils$age == 'age3'
                                 & disutils$dia == 'D2' 
                                 & disutils$complication == 'microvascular']
  )
  
  
  # MICRO EVENTS
  output$o_fC_o_D1_a1_disutil_acute <- (
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a1'])) 
    * output$fC_o_D1_a1 
    * disutils$disutil_non_fatal[disutils$age == 'age1'
                                 & disutils$dia == 'D1' 
                                 & disutils$complication == 'other']
  )
  
  output$o_fC_o_D2_a1_disutil_acute <- (
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a1'])) 
    * output$fC_o_D2_a1 
    * disutils$disutil_non_fatal[disutils$age == 'age1'
                                 & disutils$dia == 'D2' 
                                 & disutils$complication == 'other']
  )
  
  output$o_fC_o_D1_a2_disutil_acute <- (
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a2'])) 
    * output$fC_o_D1_a2 
    * disutils$disutil_non_fatal[disutils$age == 'age2'
                                 & disutils$dia == 'D1' 
                                 & disutils$complication == 'other']
  )
  
  output$o_fC_o_D2_a2_disutil_acute <- (
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a2'])) 
    * output$fC_o_D2_a2 
    * disutils$disutil_non_fatal[disutils$age == 'age2'
                                 & disutils$dia == 'D2' 
                                 & disutils$complication == 'other']
  )
  
  output$o_fC_o_D1_a3_disutil_acute <- (
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a3'])) 
    * output$fC_o_D1_a3 
    * disutils$disutil_non_fatal[disutils$age == 'age3'
                                 & disutils$dia == 'D1' 
                                 & disutils$complication == 'other']
  )
  
  output$o_fC_o_D2_a3_disutil_acute <- (
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a3'])) 
    * output$fC_o_D2_a3 
    * disutils$disutil_non_fatal[disutils$age == 'age3'
                                 & disutils$dia == 'D2' 
                                 & disutils$complication == 'other']
  )
  
  
  output$o_fC_all_disutil_acute <- (output$o_fC_o_D1_a1_disutil_acute
                                    + output$o_fC_o_D2_a1_disutil_acute
                                    + output$o_fC_o_D1_a2_disutil_acute
                                    + output$o_fC_o_D2_a2_disutil_acute
                                    + output$o_fC_o_D1_a3_disutil_acute
                                    + output$o_fC_o_D2_a3_disutil_acute
                                    + output$o_fC_Ma_D1_a1_disutil_acute
                                    + output$o_fC_Ma_D2_a1_disutil_acute
                                    + output$o_fC_Ma_D1_a2_disutil_acute
                                    + output$o_fC_Ma_D2_a2_disutil_acute
                                    + output$o_fC_Ma_D1_a3_disutil_acute
                                    + output$o_fC_Ma_D2_a3_disutil_acute
                                    + output$o_fC_Mi_D1_a1_disutil_acute
                                    + output$o_fC_Mi_D2_a1_disutil_acute
                                    + output$o_fC_Mi_D1_a2_disutil_acute
                                    + output$o_fC_Mi_D2_a2_disutil_acute
                                    + output$o_fC_Mi_D1_a3_disutil_acute
                                    + output$o_fC_Mi_D2_a3_disutil_acute)
  
  return(output)
}

calculate_LYL <- function(output){
  LYL_dat <- read.csv(file = 'inputs/life_years_lost.csv')
  
  output$o_LYL_D1_a1 <- output$D_e_D1_a1*LYL_dat$LYL_v2[LYL_dat$age == 'a1']
  output$o_LYL_D2_a1 <- output$D_e_D2_a1*LYL_dat$LYL_v2[LYL_dat$age == 'a1']
  output$o_LYL_D1_a2 <- output$D_e_D1_a2*LYL_dat$LYL_v2[LYL_dat$age == 'a2']
  output$o_LYL_D2_a2 <- output$D_e_D2_a2*LYL_dat$LYL_v2[LYL_dat$age == 'a2']
  output$o_LYL_D1_a3 <- output$D_e_D1_a3*LYL_dat$LYL_v2[LYL_dat$age == 'a3']
  output$o_LYL_D2_a3 <- output$D_e_D2_a3*LYL_dat$LYL_v2[LYL_dat$age == 'a3']
  
  output$o_LYL_aX <- (output$o_LYL_D1_a1 
                      + output$o_LYL_D2_a1
                      + output$o_LYL_D1_a2 
                      + output$o_LYL_D2_a2
                      + output$o_LYL_D1_a3 
                      + output$o_LYL_D2_a3)
  
  return(output)
}

calculate_diagnosis_costs <- function(output){
  diag_costs <- read.csv(file = 'inputs/diagnosis_direct_costs.csv')
  
  output$o_diag_cost_a1 <- output$new_diag_a1*diag_costs$cost[diag_costs$age == 'a1']
  output$o_diag_cost_a2 <- output$new_diag_a2*diag_costs$cost[diag_costs$age == 'a2']
  output$o_diag_cost_a3 <- output$new_diag_a3*diag_costs$cost[diag_costs$age == 'a3']
  
  output$o_diag_cost_aX <- (output$o_diag_cost_a1 
                            + output$o_diag_cost_a2 
                            + output$o_diag_cost_a3)
  
  return(output)
}

calculate_routine_tx_costs <- function(output){
  tx_costs <- read.csv(file = 'inputs/routine_tx_costs.csv')
  
  output$o_routine_tx_cost_D1_a1 <- output$D1_a1 * tx_costs$cost[tx_costs$age == 'a1']
  output$o_routine_tx_cost_D2_a1 <- output$D2_a1 * tx_costs$cost[tx_costs$age == 'a1']
  output$o_routine_tx_cost_D1_a2 <- output$D1_a2 * tx_costs$cost[tx_costs$age == 'a2']
  output$o_routine_tx_cost_D2_a2 <- output$D2_a2 * tx_costs$cost[tx_costs$age == 'a2']
  output$o_routine_tx_cost_D1_a3 <- output$D1_a3 * tx_costs$cost[tx_costs$age == 'a3']
  output$o_routine_tx_cost_D2_a3 <- output$D2_a3 * tx_costs$cost[tx_costs$age == 'a3']
  
  output$o_routine_tx_cost_aX <- (output$o_routine_tx_cost_D1_a1 
                                  + output$o_routine_tx_cost_D2_a1 
                                  + output$o_routine_tx_cost_D1_a2 
                                  + output$o_routine_tx_cost_D2_a2
                                  + output$o_routine_tx_cost_D1_a3 
                                  + output$o_routine_tx_cost_D2_a3)
  
  return(output)
}

calculate_glucose_monitoring_costs <- function(output){
  glucose_monitoring_costs <- read.csv(file = 'inputs/glucose_monitoring_costs.csv')
  
  output$o_glucose_monitoring_cost_D1_a1 <- output$D1_a1 * glucose_monitoring_costs$cost[glucose_monitoring_costs$age == 'a1']
  output$o_glucose_monitoring_cost_D2_a1 <- output$D2_a1 * glucose_monitoring_costs$cost[glucose_monitoring_costs$age == 'a1']
  output$o_glucose_monitoring_cost_D1_a2 <- output$D1_a2 * glucose_monitoring_costs$cost[glucose_monitoring_costs$age == 'a2']
  output$o_glucose_monitoring_cost_D2_a2 <- output$D2_a2 * glucose_monitoring_costs$cost[glucose_monitoring_costs$age == 'a2']
  output$o_glucose_monitoring_cost_D1_a3 <- output$D1_a3 * glucose_monitoring_costs$cost[glucose_monitoring_costs$age == 'a3']
  output$o_glucose_monitoring_cost_D2_a3 <- output$D2_a3 * glucose_monitoring_costs$cost[glucose_monitoring_costs$age == 'a3']
  
  output$o_glucose_monitoring_cost_aX <- (output$o_glucose_monitoring_cost_D1_a1 
                                          + output$o_glucose_monitoring_cost_D2_a1 
                                          + output$o_glucose_monitoring_cost_D1_a2 
                                          + output$o_glucose_monitoring_cost_D2_a2 
                                          + output$o_glucose_monitoring_cost_D1_a3 
                                          + output$o_glucose_monitoring_cost_D2_a3)
  return(output)
}

calculate_routine_appts <- function(output){
  routine_appts_data <- read.csv(file = 'inputs/routine_appointments.csv')
  
  output$o_excess_nurse_appts_D1_a1 <- output$D1_a1 * routine_appts_data$cost[routine_appts_data$HCP == 'Nurse']
  output$o_excess_nurse_appts_D2_a1 <- output$D2_a1 * routine_appts_data$cost[routine_appts_data$HCP == 'Nurse']
  output$o_excess_nurse_appts_D1_a2 <- output$D1_a2 * routine_appts_data$cost[routine_appts_data$HCP == 'Nurse']
  output$o_excess_nurse_appts_D2_a2 <- output$D2_a2 * routine_appts_data$cost[routine_appts_data$HCP == 'Nurse']
  output$o_excess_nurse_appts_D1_a3 <- output$D1_a3 * routine_appts_data$cost[routine_appts_data$HCP == 'Nurse']
  output$o_excess_nurse_appts_D2_a3 <- output$D2_a3 * routine_appts_data$cost[routine_appts_data$HCP == 'Nurse']
  
  output$o_excess_nurse_appts_aX <- (output$o_excess_nurse_appts_D1_a1
                                     + output$o_excess_nurse_appts_D2_a1
                                     + output$o_excess_nurse_appts_D1_a2
                                     + output$o_excess_nurse_appts_D2_a2
                                     + output$o_excess_nurse_appts_D1_a3
                                     + output$o_excess_nurse_appts_D2_a3)
  
  output$o_excess_other_appts_D1_a1 <- output$D1_a1 * routine_appts_data$cost[routine_appts_data$HCP == 'Other practitioners']
  output$o_excess_other_appts_D2_a1 <- output$D2_a1 * routine_appts_data$cost[routine_appts_data$HCP == 'Other practitioners']
  output$o_excess_other_appts_D1_a2 <- output$D1_a2 * routine_appts_data$cost[routine_appts_data$HCP == 'Other practitioners']
  output$o_excess_other_appts_D2_a2 <- output$D2_a2 * routine_appts_data$cost[routine_appts_data$HCP == 'Other practitioners']
  output$o_excess_other_appts_D1_a3 <- output$D1_a3 * routine_appts_data$cost[routine_appts_data$HCP == 'Other practitioners']
  output$o_excess_other_appts_D2_a3 <- output$D2_a3 * routine_appts_data$cost[routine_appts_data$HCP == 'Other practitioners']
  
  output$o_excess_other_appts_aX <- (output$o_excess_other_appts_D1_a1
                                     + output$o_excess_other_appts_D2_a1
                                     + output$o_excess_other_appts_D1_a2
                                     + output$o_excess_other_appts_D2_a2
                                     + output$o_excess_other_appts_D1_a3
                                     + output$o_excess_other_appts_D2_a3)
  
  output$o_excess_retinal_appts_D1_a1 <- output$D1_a1 * routine_appts_data$cost[routine_appts_data$HCP == 'Retinal screening']
  output$o_excess_retinal_appts_D2_a1 <- output$D2_a1 * routine_appts_data$cost[routine_appts_data$HCP == 'Retinal screening']
  output$o_excess_retinal_appts_D1_a2 <- output$D1_a2 * routine_appts_data$cost[routine_appts_data$HCP == 'Retinal screening']
  output$o_excess_retinal_appts_D2_a2 <- output$D2_a2 * routine_appts_data$cost[routine_appts_data$HCP == 'Retinal screening']
  output$o_excess_retinal_appts_D1_a3 <- output$D1_a3 * routine_appts_data$cost[routine_appts_data$HCP == 'Retinal screening']
  output$o_excess_retinal_appts_D2_a3 <- output$D2_a3 * routine_appts_data$cost[routine_appts_data$HCP == 'Retinal screening']
  
  output$o_excess_retinal_appts_aX <- (output$o_excess_retinal_appts_D1_a1
                                       + output$o_excess_retinal_appts_D2_a1
                                       + output$o_excess_retinal_appts_D1_a2
                                       + output$o_excess_retinal_appts_D2_a2
                                       + output$o_excess_retinal_appts_D1_a3
                                       + output$o_excess_retinal_appts_D2_a3)
  
  output$o_excess_podiatry_appts_D1_a1 <- output$D1_a1 * routine_appts_data$cost[routine_appts_data$HCP == 'Podiatry']
  output$o_excess_podiatry_appts_D2_a1 <- output$D2_a1 * routine_appts_data$cost[routine_appts_data$HCP == 'Podiatry']
  output$o_excess_podiatry_appts_D1_a2 <- output$D1_a2 * routine_appts_data$cost[routine_appts_data$HCP == 'Podiatry']
  output$o_excess_podiatry_appts_D2_a2 <- output$D2_a2 * routine_appts_data$cost[routine_appts_data$HCP == 'Podiatry']
  output$o_excess_podiatry_appts_D1_a3 <- output$D1_a3 * routine_appts_data$cost[routine_appts_data$HCP == 'Podiatry']
  output$o_excess_podiatry_appts_D2_a3 <- output$D2_a3 * routine_appts_data$cost[routine_appts_data$HCP == 'Podiatry']
  
  output$o_excess_podiatry_appts_aX <- (output$o_excess_podiatry_appts_D1_a1
                                        + output$o_excess_podiatry_appts_D2_a1
                                        + output$o_excess_podiatry_appts_D1_a2
                                        + output$o_excess_podiatry_appts_D2_a2
                                        + output$o_excess_podiatry_appts_D1_a3
                                        + output$o_excess_podiatry_appts_D2_a3)

  output$o_excess_appts_monetised_D1_a1 <- output$D1_a1 * routine_appts_data$cost[routine_appts_data$HCP == 'Total cost']
  output$o_excess_appts_monetised_D2_a1 <- output$D2_a1 * routine_appts_data$cost[routine_appts_data$HCP == 'Total cost']
  output$o_excess_appts_monetised_D1_a2 <- output$D1_a2 * routine_appts_data$cost[routine_appts_data$HCP == 'Total cost']
  output$o_excess_appts_monetised_D2_a2 <- output$D2_a2 * routine_appts_data$cost[routine_appts_data$HCP == 'Total cost']
  output$o_excess_appts_monetised_D1_a3 <- output$D1_a3 * routine_appts_data$cost[routine_appts_data$HCP == 'Total cost']
  output$o_excess_appts_monetised_D2_a3 <- output$D2_a3 * routine_appts_data$cost[routine_appts_data$HCP == 'Total cost']
  
  output$o_excess_appts_monetised_aX <- (output$o_excess_appts_monetised_D1_a1
                                     + output$o_excess_appts_monetised_D2_a1
                                     + output$o_excess_appts_monetised_D1_a2
                                     + output$o_excess_appts_monetised_D2_a2
                                     + output$o_excess_appts_monetised_D1_a3
                                     + output$o_excess_appts_monetised_D2_a3)
  
  return(output)
}


calculate_disutils_deaths <- function(output){
  qaly_loss_by_age <- read.csv('inputs/deaths_utility_loss.csv')
  
  output$D_e_QALYs_D1_a1 <- output$D_e_D1_a1 * qaly_loss_by_age$val[qaly_loss_by_age$category == 'a1']
  output$D_e_QALYs_D2_a1 <- output$D_e_D2_a1 * qaly_loss_by_age$val[qaly_loss_by_age$category == 'a1']
  output$D_e_QALYs_D1_a2 <- output$D_e_D1_a2 * qaly_loss_by_age$val[qaly_loss_by_age$category == 'a2']
  output$D_e_QALYs_D2_a2 <- output$D_e_D2_a2 * qaly_loss_by_age$val[qaly_loss_by_age$category == 'a2']
  output$D_e_QALYs_D1_a3 <- output$D_e_D1_a3 * qaly_loss_by_age$val[qaly_loss_by_age$category == 'a3']
  output$D_e_QALYs_D2_a3 <- output$D_e_D2_a3 * qaly_loss_by_age$val[qaly_loss_by_age$category == 'a3']
  
  output$D_e_QALYs_aX <- (output$D_e_QALYs_D1_a1
                          + output$D_e_QALYs_D2_a1
                          + output$D_e_QALYs_D1_a2
                          + output$D_e_QALYs_D2_a2
                          + output$D_e_QALYs_D1_a3
                          + output$D_e_QALYs_D2_a3)
  
  return(output)
  }