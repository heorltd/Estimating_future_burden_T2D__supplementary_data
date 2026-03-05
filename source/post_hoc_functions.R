#' script: post_hoc_functions.R
#' Rproject: X_T2D_TSV.Rproj
#' Author: Thomas E. Padgett
#' Date: 2023-12-04
#' 
#' Description: Collection of functions used in SDM post-hoc calculations

################################################################################

calculate_hospitalisations <- function(output){
  hosp_rates <- read.csv('data/value_perspectives/hosp_rates.csv')
  
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

plot_hosps <- function(output){
  
  plt1 <- ggplot(data = output) +
    geom_line(aes(x = time, y = o_fC_all_hosps)) +
    
    labs(title = 'Annual hospitalisations due to T2D complications',
         x = 'Year',
         y = 'Hospitalisations') + 
    
    scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                    scale = 1e-6))
  
  plt1
}

calculate_bed_days <- function(output){
  hosp_LOS <- read.csv('data/value_perspectives/hosp_LOS.csv')
  
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

plot_bed_days <- function(output){
  
  plt1 <- ggplot(data = output) +
    geom_line(aes(x = time, y = o_fC_all_beds)) +
    
    labs(title = 'Annual total number of bed days due to T2D complications',
         x = 'Year',
         y = 'Bed days') + 
    
    scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                    scale = 1e-6))
  
  plt1
}

calculate_direct_healthcare_costs <- function(output, parameters){
  direct_costs <- read.csv('data/value_perspectives/complication_direct_costs.csv')
  
  
  
  ############ MACRO EVENTS
  output$o_fC_Ma_D1_a1_directCosts <- ((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a1']))
    * output$fC_Ma_D1_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a1'])
  * output$fC_Ma_D1_a1 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'macrovascular']
  )
  
  
  output$o_fC_Ma_D2_a1_directCosts <- ((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a1']))
    * output$fC_Ma_D2_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a1'])
  * output$fC_Ma_D2_a1 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'macrovascular']
  )
  
  output$o_fC_Ma_D1_a2_directCosts <- ((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a2']))
    * output$fC_Ma_D1_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a2'])
  * output$fC_Ma_D1_a2 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'macrovascular']
  )
  
  output$o_fC_Ma_D2_a2_directCosts <- ((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a2']))
    * output$fC_Ma_D2_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a2'])
  * output$fC_Ma_D2_a2 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'macrovascular']
  )
  
  output$o_fC_Ma_D1_a3_directCosts <- ((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a3']))
    * output$fC_Ma_D1_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a3'])
  * output$fC_Ma_D1_a3 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'macrovascular']
  )
  
  output$o_fC_Ma_D2_a3_directCosts <- ((
    (1-(parameters['eta_tau_gamma']*parameters['tau_gamma_a3']))
    * output$fC_Ma_D2_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'macrovascular']
  ) +
    (parameters['eta_tau_gamma']*parameters['tau_gamma_a3'])
  * output$fC_Ma_D2_a3 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'macrovascular']
  )
  
  
  
  ############ MICRO EVENTS
  output$o_fC_Mi_D1_a1_directCosts <- ((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a1']))
    * output$fC_Mi_D1_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a1'])
  * output$fC_Mi_D1_a1 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'microvascular']
  )
  
  
  output$o_fC_Mi_D2_a1_directCosts <- ((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a1']))
    * output$fC_Mi_D2_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a1'])
  * output$fC_Mi_D2_a1 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'microvascular']
  )
  
  output$o_fC_Mi_D1_a2_directCosts <- ((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a2']))
    * output$fC_Mi_D1_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a2'])
  * output$fC_Mi_D1_a2 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'microvascular']
  )
  
  output$o_fC_Mi_D2_a2_directCosts <- ((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a2']))
    * output$fC_Mi_D2_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a2'])
  * output$fC_Mi_D2_a2 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'microvascular']
  )
  
  output$o_fC_Mi_D1_a3_directCosts <- ((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a3']))
    * output$fC_Mi_D1_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a3'])
  * output$fC_Mi_D1_a3 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'microvascular']
  )
  
  output$o_fC_Mi_D2_a3_directCosts <- ((
    (1-(parameters['eta_tau_beta']*parameters['tau_beta_a3']))
    * output$fC_Mi_D2_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'microvascular']
  ) +
    (parameters['eta_tau_beta']*parameters['tau_beta_a3'])
  * output$fC_Mi_D2_a3 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'microvascular']
  )
  
  
  ############ OTHER EVENTS
  output$o_fC_o_D1_a1_directCosts <- ((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a1']))
    * output$fC_o_D1_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a1'])
  * output$fC_o_D1_a1 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'other']
  )
  
  
  output$o_fC_o_D2_a1_directCosts <- ((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a1']))
    * output$fC_o_D2_a1 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a1'])
  * output$fC_o_D2_a1 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age1'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'other']
  )
  
  output$o_fC_o_D1_a2_directCosts <- ((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a2']))
    * output$fC_o_D1_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a2'])
  * output$fC_o_D1_a2 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'other']
  )
  
  output$o_fC_o_D2_a2_directCosts <- ((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a2']))
    * output$fC_o_D2_a2 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a2'])
  * output$fC_o_D2_a2 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age2'
                               & direct_costs$dia == 'D2'
                               & direct_costs$complication == 'other']
  )
  
  output$o_fC_o_D1_a3_directCosts <- ((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a3']))
    * output$fC_o_D1_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D1'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a3'])
  * output$fC_o_D1_a3 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                               & direct_costs$dia == 'D1'
                               & direct_costs$complication == 'other']
  )
  
  output$o_fC_o_D2_a3_directCosts <- ((
    (1-(parameters['eta_tau_alpha']*parameters['tau_alpha_a3']))
    * output$fC_o_D2_a3 
    * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
                                 & direct_costs$dia == 'D2'
                                 & direct_costs$complication == 'other']
  ) +
    (parameters['eta_tau_alpha']*parameters['tau_alpha_a3'])
  * output$fC_o_D2_a3 
  * direct_costs$cost_nonfatal[direct_costs$age == 'age3'
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

plot_direct_costs <- function(output){
  
  plt1 <- ggplot(data = output) +
    geom_line(aes(x = time, y = o_fC_all_directCosts)) +
    
    labs(title = 'Annual direct healthcare expenditure due to T2D complications',
         x = 'Year',
         y = 'Annual Cost (£)') + 
    
    scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                    scale = 1e-6))
  
  plt1
}

# Calculate indirect costs
# inc: 
# total lost income due to complications
# total absenteeism due to complications
calculate_indirect_costs <- function(output){
  indirect_costs <- read.csv('data/value_perspectives/complication_indirect_costs.csv')

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

plot_income_loss <- function(output){
  
  plt1 <- ggplot(data = output) +
    geom_line(aes(x = time, y = o_fC_all_incomeLoss)) +
    
    labs(title = 'Annual income loss due to T2D',
         x = 'Year',
         y = 'Income loss (£)') + 
    
    scale_y_continuous(labels = scales::unit_format(unit = "B", 
                                                    scale = 1e-9))
  
  plt1

}

plot_absenteeism <- function(output){
  
  plt1 <- ggplot(data = output) +
    geom_line(aes(x = time, y = o_fC_all_absenteeism)) +
    
    labs(title = 'Annual absenteeism due to T2D',
         x = 'Year',
         y = 'Days') + 
    
    scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                    scale = 1e-6))
  
  plt1
  
}

# Calculate environmental impact
# inc:
# total co2 emission due to bed days
# total water consumption due to bed days
calculate_environment_impact <- function(output){
  env_impacts <- read.csv('data/value_perspectives/env_impacts.csv')
  
  output$o_total_co2e_emissions <- output$o_fC_all_beds * env_impacts$val_.kg.[env_impacts$category == 'co2e']
  output$o_total_water_consumption <- output$o_fC_all_beds * env_impacts$val_.kg.[env_impacts$category == 'water']
  # assumes all bed days are low intensity
  return(output)
}

plot_environment_impact <- function(output){
  
  plt1 <- ggplot(data = output) +
    geom_line(aes(x = time, y = o_total_co2e_emissions, colour = 'CO2e')) +
    geom_line(aes(x = time, y = o_total_water_consumption, colour = 'Water')) +
    
    labs(title = 'Total annual CO2e emissions and water consumed due hospitalisations due to T2D complications',
         x = 'Year',
         y = 'Emission/Consumption (kg)') + 
    
    scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                    scale = 1e-6))
  
  plt1
  
}

# calculate total disutility due to complications
# Acute + maintenance
calculate_disutils <- function(output, parameters){
  
  disutils <- read.csv('data/value_perspectives/complication_disutilities.csv')
  
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

plot_disutils <- function(output){
  
  plt1 <- ggplot(data = output) +
    geom_line(aes(x = time, y = o_fC_all_disutil_acute)) +
    
    labs(title = 'Annual acute utility decrements due to T2D complications',
         x = 'Year',
         y = 'Total utility decrement')
  
  plt1
}

plot_disutils_monetary <- function(output){
  
  WTP = 20000
  
  plt1 <- ggplot(data = output) +
    geom_line(aes(x = time, y = o_fC_all_disutil_acute*WTP)) +
    
    labs(title = 'Value in treating the total annual acute utility decrements due to T2D complications, assuming a WTP threshold of £20k',
         x = 'Year',
         y = 'Value (£)') + 
    
    scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                    scale = 1e-6))
  
  plt1
}
