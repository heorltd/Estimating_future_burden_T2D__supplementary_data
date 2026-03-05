#' script: T2D_SDM
#' Rproject: X_T2D_TSV.Rproj
#' Author: Thomas E. Padgett
#' Date: 2023-09-29
#' 
#' Description: ODE system for the T2D TSV
#' 
#'   

T2D_SDM <- function(time, 
                    initial_state, 
                    parameters,
                    mig_age_props){
  with(as.list(c(initial_state, parameters)), {
    
    #' For information:
    #' initial_state contains the compartment occupancies for the initial year.
    #'
    #' Compartments are: 
    #'     F0 (feeder compartment)
    #'     D0_a1 (Diabetic status 0 (not diabetic) AND age band 1 (18 to 45))
    #'     D1_a1 (Diabetic status 1 (uncontrolled T2D) AND age band 1 (18 to 45))
    #'     D2_a1 (Diabetic status 2 (controlled T2D) AND age band 1 (18 to 45))
    #'     etc.
    #'     D_ACM (all cause mortality)
    #'     D_e (Death due to disease)
    #'     
    #' Age options are age0: <18
    #'                 age1: 18<=x<45
    #'                 age2: 45<=x<65
    #'                 age3: 65<=x<100 (assume death >100)
    #'                 
    #' T2D options are D0: Not diabetic
    #'                 D1: T2D uncontrolled
    #'                 D2: T2D controlled
    #'                 
    #' Complications are split into: Macrovascular
    #'                               Microvascular
    #'                               Other
    #' 
    
    # Get birth rate and net migration from gompertz fits

    delta <- gompertz(t = (time - 2001),
                      alpha = 3.172297848,
                      beta = -1.228265722,
                      k = 0.002740153)/1000
       
    phi <- gompertz(t = (time - 2001),
                    alpha = 3.26196401,
                    beta = -0.67389543,
                    k = 0.06644021)/1000
    
    # note that these gompertz are fit to time = 0:60, so need for offset for 
    # true time. The reason for this is that it's much less stable at trying to
    # fit to t = 2000:2060
    # Determine proportion newly 18 who are not diabetic (T2D)
    rho_0 <- 1 - rho_1 - rho_2
    
    # Total population
    P <- (F0 + 
          D0_a1 + D1_a1 + D2_a1 +
          D0_a2 + D1_a2 + D2_a2 +
          D0_a3 + D1_a3 + D2_a3)
          
    # Determine the total incoming/outgoing migrants
    total_migs <- phi*P
    
    # Use avg distribution of migrants by age
    total_migs_age0 <- total_migs*mig_age_props[mig_age_props$source == 'ONS',]$age0
    total_migs_age1 <- total_migs*mig_age_props[mig_age_props$source == 'ONS',]$age1
    total_migs_age2 <- total_migs*mig_age_props[mig_age_props$source == 'ONS',]$age2
    total_migs_age3 <- total_migs*mig_age_props[mig_age_props$source == 'ONS',]$age3
    
    # Feeder compartment
    dF <- (delta*P                  # increase due to new births
           - kappa_0*F0             # decrease due to ageing
           - mu_a0*F0               # decrease due to ACM
           + total_migs_age0)       # increase due to net migration
    
    
    # First age band
    dD0_a1 <- (kappa_0*rho_0*F0     # increase due to people ageing (from feeder)
               - mu_a1*D0_a1        # decrease due to ACM
               - eta_i_a1*lambda_1_a1*D0_a1  # decrease due to people becoming T2D (T2D incidence)
               - kappa_1*D0_a1      # decrease due to people ageing 
               + (total_migs_age1   # increase from net migration
                  * (D0_a1/(D0_a1 + D1_a1 + D2_a1))))
    
    dD1_a1 <- (kappa_0*rho_1*F0     # increase due to people ageing (from feeder)
               - mu_a1*D1_a1        # decrease due to ACM
               + eta_i_a1*lambda_1_a1*D0_a1  # increase due to people becoming T2D (T2D incidence)
               + eta_epsilon_a1*epsilon_a1*D2_a1   # increase due to people losing T2D control
               - eta_lambda_a1*lambda_2_a1*D1_a1  # decrease due to people gaining T2D control
               - kappa_1*D1_a1      # decrease due to people ageing
               - eta_alpha_1_a1*alpha_1_a1*D1_a1*eta_tau_alpha*tau_alpha_a1  # decrease due to death from other complications
               - eta_beta_1_a1*beta_1_a1*D1_a1*eta_tau_beta*tau_beta_a1    # decrease due to death from microvascular complications
               - eta_gamma_1_a1*gamma_1_a1*D1_a1*eta_tau_gamma*tau_gamma_a1 # decrease due to death from macrovascular complications
               + (total_migs_age1   # increase from net migration
                  * (D1_a1/(D0_a1 + D1_a1 + D2_a1))))
    
    dD2_a1 <- (kappa_0*rho_2*F0     # increase due to people ageing (from feeder)
               - mu_a1*D2_a1        # decrease due to ACM
               + eta_lambda_a1*lambda_2_a1*D1_a1  # increase due to people gaining t2d control
               - eta_epsilon_a1*epsilon_a1*D2_a1   # decrease due to people losing T2D control
               - kappa_1*D2_a1      # decrease due to people ageing
               - eta_alpha_2_a1*alpha_2_a1*D2_a1*eta_tau_alpha*tau_alpha_a1  # decrease due to death from other complications
               - eta_beta_2_a1*beta_2_a1*D2_a1*eta_tau_beta*tau_beta_a1    # decrease due to death from microvascular complications
               - eta_gamma_2_a1*gamma_2_a1*D2_a1*eta_tau_gamma*tau_gamma_a1  # decrease due to death from macrovascular complications
               + (total_migs_age1   # increase from net migration
                  * (D2_a1/(D0_a1 + D1_a1 + D2_a1))))
    
    
    # Second age band
    dD0_a2 <- (kappa_1*D0_a1        # increase due to people ageing 
               - mu_a2*D0_a2        # decrease due to ACM
               - eta_i_a2*lambda_1_a2*D0_a2  # decrease due to people becoming T2D (T2D incidence)
               - kappa_2*D0_a2      # decrease due to people ageing
               + (total_migs_age2   # increase from net migration
                  * (D0_a2/(D0_a2 + D1_a2 + D2_a2))))
    
    dD1_a2 <- (kappa_1*D1_a1        # increase due to people ageing 
               - mu_a2*D1_a2        # decrease due to ACM
               + eta_i_a2*lambda_1_a2*D0_a2  # increase due to people becoming T2D (T2D incidence)
               + eta_epsilon_a2*epsilon_a2*D2_a2   # increase due to people losing T2D control
               - eta_lambda_a2*lambda_2_a2*D1_a2  # decrease due to people gaining T2D control
               - kappa_2*D1_a2      # decrease due to people ageing
               - eta_alpha_1_a2*alpha_1_a2*D1_a2*eta_tau_alpha*tau_alpha_a2  # decrease due to death from other complications
               - eta_beta_1_a2*beta_1_a2*D1_a2*eta_tau_beta*tau_beta_a2    # decrease due to death from microvascular complications
               - eta_gamma_1_a2*gamma_1_a2*D1_a2*eta_tau_gamma*tau_gamma_a2  # decrease due to death from macrovascular complications
               + (total_migs_age2   # increase from net migration
                  * (D1_a2/(D0_a2 + D1_a2 + D2_a2))))
    
    dD2_a2 <- (kappa_1*D2_a1        # increase due to people ageing 
               - mu_a2*D2_a2        # decrease due to ACM
               + eta_lambda_a2*lambda_2_a2*D1_a2  # increase due to people gaining t2d control
               - eta_epsilon_a2*epsilon_a2*D2_a2   # decrease due to people losing T2D control
               - kappa_2*D2_a2      # decrease due to people ageing
               - eta_alpha_2_a2*alpha_2_a2*D2_a2*eta_tau_alpha*tau_alpha_a2  # decrease due to death from other complications
               - eta_beta_2_a2*beta_2_a2*D2_a2*eta_tau_beta*tau_beta_a2    # decrease due to death from microvascular complications
               - eta_gamma_2_a2*gamma_2_a2*D2_a2*eta_tau_gamma*tau_gamma_a2  # decrease due to death from macrovascular complications
               + (total_migs_age2   # increase from net migration
                  * (D2_a2/(D0_a2 + D1_a2 + D2_a2))))
    
    
    # Third age band
    dD0_a3 <- (kappa_2*D0_a2      # increase due to people ageing
               - mu_a3*D0_a3        # decrease due to ACM
               - eta_i_a3*lambda_1_a3*D0_a3  # decrease due to people becoming T2D (T2D incidence)
               - kappa_3*D0_a3      # decrease due to people ageing
               + (total_migs_age3   # increase from net migration
                  * (D0_a3/(D0_a3 + D1_a3 + D2_a3))))
    
    dD1_a3 <- (kappa_2*D1_a2      # increase due to people ageing 
               - mu_a3*D1_a3        # decrease due to ACM
               + eta_i_a3*lambda_1_a3*D0_a3  # increase due to people becoming T2D (T2D incidence)
               + eta_epsilon_a3*epsilon_a3*D2_a3   # increase due to people losing T2D control
               - eta_lambda_a3*lambda_2_a3*D1_a3  # decrease due to people gaining T2D control
               - kappa_3*D1_a3      # decrease due to people ageing
               - eta_alpha_1_a3*alpha_1_a3*D1_a3*eta_tau_alpha*tau_alpha_a3  # decrease due to death from other complications
               - eta_beta_1_a3*beta_1_a3*D1_a3*eta_tau_beta*tau_beta_a3    # decrease due to death from microvascular complications
               - eta_gamma_1_a3*gamma_1_a3*D1_a3*eta_tau_gamma*tau_gamma_a3  # decrease due to death from macrovascular complications
               + (total_migs_age3   # increase from net migration
                  * (D1_a3/(D0_a3 + D1_a3 + D2_a3))))
               
    
    dD2_a3 <- (kappa_2*D2_a2      # increase due to people ageing 
               - mu_a3*D2_a3        # decrease due to ACM
               + eta_lambda_a3*lambda_2_a3*D1_a3  # increase due to people gaining t2d control
               - eta_epsilon_a3*epsilon_a3*D2_a3   # decrease due to people losing T2D control
               - kappa_3*D2_a3      # decrease due to people ageing
               - eta_alpha_2_a3*alpha_2_a3*D2_a3*eta_tau_alpha*tau_alpha_a3  # decrease due to death from other complications
               - eta_beta_2_a3*beta_2_a3*D2_a3*eta_tau_beta*tau_beta_a3    # decrease due to death from microvascular complications
               - eta_gamma_2_a3*gamma_2_a3*D2_a3*eta_tau_gamma*tau_gamma_a3  # decrease due to death from macrovascular complications
               + (total_migs_age3   # increase from net migration
                  * (D2_a3/(D0_a3 + D1_a3 + D2_a3))))
    
    
    # Death compartments
    dD_acm_a0 <- (mu_a0*F0)  # ACM from feeder compartment
    
    dD_acm_D0_a1 <- (mu_a1*D0_a1) # ACM from D0 and age1
    dD_acm_D1_a1 <- (mu_a1*D1_a1) # ACM from D1 and age1
    dD_acm_D2_a1 <- (mu_a1*D2_a1) # ACM from D2 and age1
    
    dD_acm_D0_a2 <- (mu_a2*D0_a2) # ACM from D0 and age2
    dD_acm_D1_a2 <- (mu_a2*D1_a2) # ACM from D1 and age2
    dD_acm_D2_a2 <- (mu_a2*D2_a2) # ACM from D2 and age2
    
    dD_acm_D0_a3 <- (mu_a3*D0_a3      # ACM from D0 and age3
                     + kappa_3*D0_a3) # death due to ageing from D0 and age3
    
    dD_acm_D1_a3 <- (mu_a3*D1_a3      # ACM from D1 and age3
                     + kappa_3*D1_a3) # death due to ageing from D1 and age3
    
    dD_acm_D2_a3 <- (mu_a3*D2_a3      # ACM from D2 and age3
                     + kappa_3*D2_a3) # death due to ageing from D2 and age3
    
    dD_e_D1_a1 <- (eta_alpha_1_a1*alpha_1_a1*D1_a1*eta_tau_alpha*tau_alpha_a1     # excess death from other complications, D1, age1
                   + eta_beta_1_a1*beta_1_a1*D1_a1*eta_tau_beta*tau_beta_a1       # excess death from microvascular complications, D1, age1
                   + eta_gamma_1_a1*gamma_1_a1*D1_a1*eta_tau_gamma*tau_gamma_a1)  # excess death from macrovascular complications, D1, age1
                
    dD_e_D2_a1 <- (eta_alpha_2_a1*alpha_2_a1*D2_a1*eta_tau_alpha*tau_alpha_a1     # excess death from other complications, D2, age1
                   + eta_beta_2_a1*beta_2_a1*D2_a1*eta_tau_beta*tau_beta_a1       # excess death from microvascular complications, D2, age1
                   + eta_gamma_2_a1*gamma_2_a1*D2_a1*eta_tau_gamma*tau_gamma_a1)  # excess death from macrovascular complications, D2, age1
                
    dD_e_D1_a2 <- (eta_alpha_1_a2*alpha_1_a2*D1_a2*eta_tau_alpha*tau_alpha_a2     # excess death from other complications, D1, age2
                   + eta_beta_1_a2*beta_1_a2*D1_a2*eta_tau_beta*tau_beta_a2       # excess death from microvascular complications, D1, age2
                   + eta_gamma_1_a2*gamma_1_a2*D1_a2*eta_tau_gamma*tau_gamma_a2)  # excess death from macrovascular complications, D1, age2
    
    dD_e_D2_a2 <- (eta_alpha_2_a2*alpha_2_a2*D2_a2*eta_tau_alpha*tau_alpha_a2     # excess death from other complications, D2, age2
                   + eta_beta_2_a2*beta_2_a2*D2_a2*eta_tau_beta*tau_beta_a2       # excess death from microvascular complications, D2, age2
                   + eta_gamma_2_a2*gamma_2_a2*D2_a2*eta_tau_gamma*tau_gamma_a2)  # excess death from macrovascular complications, D2, age2
                 
    dD_e_D1_a3 <- (eta_alpha_1_a3*alpha_1_a3*D1_a3*eta_tau_alpha*tau_alpha_a3     # excess death from other complications, D1, age3
                   + eta_beta_1_a3*beta_1_a3*D1_a3*eta_tau_beta*tau_beta_a3       # excess death from microvascular complications, D1, age3
                   + eta_gamma_1_a3*gamma_1_a3*D1_a3*eta_tau_gamma*tau_gamma_a3)  # excess death from macrovascular complications, D1, age3
    
    dD_e_D2_a3 <- (eta_alpha_2_a3*alpha_2_a3*D2_a3*eta_tau_alpha*tau_alpha_a3     # excess death from other complications, D2, age3
                   + eta_beta_2_a3*beta_2_a3*D2_a3*eta_tau_beta*tau_beta_a3       # excess death from microvascular complications, D2, age3
                   + eta_gamma_2_a3*gamma_2_a3*D2_a3*eta_tau_gamma*tau_gamma_a3)  # excess death from macrovascular complications, D2, age3
             
    # False compartments to record numbers of complications
    dfC_o_D1_a1 <- eta_alpha_1_a1*alpha_1_a1*D1_a1 # Other complications, D1, a1
    dfC_o_D2_a1 <- eta_alpha_2_a1*alpha_2_a1*D2_a1 # Other complications, D2, a1
    dfC_o_D1_a2 <- eta_alpha_1_a2*alpha_1_a2*D1_a2 # Other complications, D1, a2  
    dfC_o_D2_a2 <- eta_alpha_2_a2*alpha_2_a2*D2_a2 # Other complications, D2, a2
    dfC_o_D1_a3 <- eta_alpha_1_a3*alpha_1_a3*D1_a3 # Other complications, D1, a3
    dfC_o_D2_a3 <- eta_alpha_2_a3*alpha_2_a3*D2_a3 # Other complications, D2, a3
  
    dfC_Mi_D1_a1 <- eta_beta_1_a1*beta_1_a1*D1_a1 # Microvascular complications, D1, a1
    dfC_Mi_D2_a1 <- eta_beta_2_a1*beta_2_a1*D2_a1 # Microvascular complications, D2, a1
    dfC_Mi_D1_a2 <- eta_beta_1_a2*beta_1_a2*D1_a2 # Microvascular complications, D1, a2  
    dfC_Mi_D2_a2 <- eta_beta_2_a2*beta_2_a2*D2_a2 # Microvascular complications, D2, a2
    dfC_Mi_D1_a3 <- eta_beta_1_a3*beta_1_a3*D1_a3 # Microvascular complications, D1, a3
    dfC_Mi_D2_a3 <- eta_beta_2_a3*beta_2_a3*D2_a3 # Microvascular complications, D2, a3
    
    dfC_Ma_D1_a1 <- eta_gamma_1_a1*gamma_1_a1*D1_a1 # Macrovascular complications, D1, a1
    dfC_Ma_D2_a1 <- eta_gamma_2_a1*gamma_2_a1*D2_a1 # Macrovascular complications, D2, a1
    dfC_Ma_D1_a2 <- eta_gamma_1_a2*gamma_1_a2*D1_a2 # Macrovascular complications, D1, a2 
    dfC_Ma_D2_a2 <- eta_gamma_2_a2*gamma_2_a2*D2_a2 # Macrovascular complications, D2, a2
    dfC_Ma_D1_a3 <- eta_gamma_1_a3*gamma_1_a3*D1_a3 # Macrovascular complications, D1, a3
    dfC_Ma_D2_a3 <- eta_gamma_2_a3*gamma_2_a3*D2_a3 # Macrovascular complications, D2, a3
      
    return(list(c(dF,
                  dD0_a1,
                  dD1_a1,
                  dD2_a1,
                  dD0_a2,
                  dD1_a2,
                  dD2_a2,
                  dD0_a3,
                  dD1_a3,
                  dD2_a3,
                  dD_acm_a0,
                  dD_acm_D0_a1,
                  dD_acm_D1_a1,
                  dD_acm_D2_a1,
                  dD_acm_D0_a2,
                  dD_acm_D1_a2,
                  dD_acm_D2_a2,
                  dD_acm_D0_a3,
                  dD_acm_D1_a3,
                  dD_acm_D2_a3,
                  dD_e_D1_a1,
                  dD_e_D2_a1,
                  dD_e_D1_a2,
                  dD_e_D2_a2,
                  dD_e_D1_a3,
                  dD_e_D2_a3,
                  dfC_o_D1_a1,
                  dfC_o_D2_a1,
                  dfC_o_D1_a2,
                  dfC_o_D2_a2,
                  dfC_o_D1_a3,
                  dfC_o_D2_a3,
                  dfC_Mi_D1_a1,
                  dfC_Mi_D2_a1,
                  dfC_Mi_D1_a2,
                  dfC_Mi_D2_a2,
                  dfC_Mi_D1_a3,
                  dfC_Mi_D2_a3,
                  dfC_Ma_D1_a1,
                  dfC_Ma_D2_a1,
                  dfC_Ma_D1_a2,
                  dfC_Ma_D2_a2,
                  dfC_Ma_D1_a3,
                  dfC_Ma_D2_a3)))
  })
}
