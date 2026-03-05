#' Script: load_params.R
#' Rproject: X_T2D_TSV.Rproj
#' Author: Thomas E. Padgett
#' Date: 2023-12-19
#'  
#' Description: Returns the static and precalibrated parameters required to 
#' satisfy the ODE function. 
#' 
#' The reason this is written as it's own function rather than just a read.csv() 
#' call is because the ODE systems needs it in a 'named num' format as below, 
#' which is messy and gross and therefore hidden in a function.
#' 


load_params <- function(multifactorial){
  
  if (multifactorial == F){
    static_params <- read.csv('inputs/static_params.csv')
  } else{
    static_params <- read.csv('inputs/static_params_multifactorial.csv')
  }
  
  #cali_params <- read.csv('inputs/precalibrated_params.csv')
  cali_params <- read.csv('inputs/precalibrated_params_2016+.csv')
  
  params <- c(static_params$val, cali_params$val)
  names(params) <- c(static_params$name, cali_params$name)
  
  return(params)
}

load_params_calib <- function(){
  static_params <- read.csv('inputs/static_params.csv')
  
  params <- c(static_params$val)
  names(params) <- c(static_params$name)
  
  return(params)
}
