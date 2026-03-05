#' Script: load_precalibrated_initial_population.R
#' Rproject: X_T2D_TSV.Rproj
#' Author: Thomas E. Padgett
#' Date: 2023-12-19
#'  
#' Description: Returns the initial population required to satisfy the ODE
#' function based on precalibrated data. 
#' 
#' The reason this is written as it's own function rather than just a read.csv() 
#' call is because the ODE systems needs it in a 'named num' format as below, 
#' which is messy and gross and therefore hidden in a function.
#' 


load_precalibrated_initial_population <- function(){
  #precalibrated <- read.csv('inputs/precalibrated_population.csv')
  precalibrated <- read.csv('inputs/precalibrated_population_2016+.csv')
  
  
  output <- c(F0 = precalibrated$F0,
              D0_a1 = precalibrated$D0_a1,  
              D1_a1 = precalibrated$D1_a1, 
              D2_a1 = precalibrated$D2_a1, 
              D0_a2 = precalibrated$D0_a2,  
              D1_a2 = precalibrated$D1_a2,
              D2_a2 = precalibrated$D2_a2, 
              D0_a3 = precalibrated$D0_a3,  
              D1_a3 = precalibrated$D1_a3,
              D2_a3 = precalibrated$D2_a3, 
              D_acm_a0 = 0,
              D_acm_D0_a1 = 0,
              D_acm_D1_a1 = 0,
              D_acm_D2_a1 = 0,
              D_acm_D0_a2 = 0,
              D_acm_D1_a2 = 0,
              D_acm_D2_a2 = 0,
              D_acm_D0_a3 = 0,
              D_acm_D1_a3 = 0,
              D_acm_D2_a3 = 0,
              D_e_D1_a1 = 0,
              D_e_D2_a1 = 0,
              D_e_D1_a2 = 0,
              D_e_D2_a2 = 0,
              D_e_D1_a3 = 0,
              D_e_D2_a3 = 0,
              fC_o_D1_a1 = 0,
              fC_o_D2_a1 = 0,
              fC_o_D1_a2 = 0,
              fC_o_D2_a2 = 0,
              fC_o_D1_a3 = 0,
              fC_o_D2_a3 = 0,
              fC_Mi_D1_a1 = 0,
              fC_Mi_D2_a1 = 0,
              fC_Mi_D1_a2 = 0,
              fC_Mi_D2_a2 = 0,
              fC_Mi_D1_a3 = 0,
              fC_Mi_D2_a3 = 0,
              fC_Ma_D1_a1 = 0,
              fC_Ma_D2_a1 = 0,
              fC_Ma_D1_a2 = 0,
              fC_Ma_D2_a2 = 0,
              fC_Ma_D1_a3 = 0,
              fC_Ma_D2_a3 = 0)
  
  return(output)
}
