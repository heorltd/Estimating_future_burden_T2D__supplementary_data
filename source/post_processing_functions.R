#' script: post_processing_functions.R
#' Rproject: X_T2D_TSV.Rproj
#' Author: Thomas E. Padgett
#' Date: 2023-10-26
#' 
#' Description: Collection of functions used in SDM post processing

################################################################################
post_processed_compartments <- function(output){
  
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
  
  
  output$fC_o <- rowSums(output[,c('fC_o_D1_a1', 'fC_o_D2_a1',
                                    'fC_o_D1_a2', 'fC_o_D2_a2',
                                    'fC_o_D1_a3', 'fC_o_D2_a3')])
  
  output$fC_Ma <- rowSums(output[,c('fC_Ma_D1_a1', 'fC_Ma_D2_a1',
                                    'fC_Ma_D1_a2', 'fC_Ma_D2_a2',
                                    'fC_Ma_D1_a3', 'fC_Ma_D2_a3')])
  
  output$fC_Mi <- rowSums(output[,c('fC_Mi_D1_a1', 'fC_Mi_D2_a1',
                                    'fC_Mi_D1_a2', 'fC_Mi_D2_a2',
                                    'fC_Mi_D1_a3', 'fC_Mi_D2_a3')])
  
  output$D_acm <- c(diff(output$D_acm), NA)
  output$D_e <- c(diff(output$D_e), NA)
    
  return(output)
}

plt_total_pop_comparison <- function(output, 
                                     calib_data, 
                                     ONS_prediction = NULL,
                                     calibrated = F){
  if (calibrated == F){
    if (!is.null(ONS_prediction)){
      plt_totalPop <- ggplot(data = output) +
        geom_line(aes(x = time, 
                      y = (F0
                           + D0_a1 + D1_a1 + D2_a1
                           + D0_a2 + D1_a2 + D2_a2
                           + D0_a3 + D1_a3 + D2_a3),
                      colour = 'TSV Prediction')) +
        geom_point(data = calib_data, 
                   aes(x = time, 
                       y = (cal_F0
                            + cal_D0_a1 + cal_D1_a1 + cal_D2_a1
                            + cal_D0_a2 + cal_D1_a2 + cal_D2_a2
                            + cal_D0_a3 + cal_D1_a3 + cal_D2_a3),
                       colour = 'ONS & NDA data')) +
        geom_point(data = ONS_prediction, 
                   aes(x = year, 
                       y = total,
                       colour = 'ONS Prediction')) +
        labs(title = 'Total population comparison',
             x = 'Year', 
             y = 'Total Pop',
             colour = '') + 
        scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                        scale = 1e-6),
                           limits = c(50e6,100e6))
    } else{
      plt_totalPop <- ggplot(data = output) +
        geom_line(aes(x = time, 
                      y = (F0
                           + D0_a1 + D1_a1 + D2_a1
                           + D0_a2 + D1_a2 + D2_a2
                           + D0_a3 + D1_a3 + D2_a3),
                      colour = 'TSV Prediction')) +
        geom_point(data = calib_data, 
                   aes(x = time, 
                       y = (cal_F0
                            + cal_D0_a1 + cal_D1_a1 + cal_D2_a1
                            + cal_D0_a2 + cal_D1_a2 + cal_D2_a2
                            + cal_D0_a3 + cal_D1_a3 + cal_D2_a3),
                       colour = 'ONS & NDA data')) +
        labs(title = 'Total population comparison',
             x = 'Year', 
             y = 'Total Pop',
             colour = '') + 
        scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                        scale = 1e-6),
                           limits = c(50e6,100e6))
    }
  }
  
  if (calibrated == T){
    if (!is.null(ONS_prediction)){
      plt_totalPop <- ggplot(data = output) +
        geom_line(aes(x = time, 
                      y = (F0
                           + D0_a1 + D1_a1 + D2_a1
                           + D0_a2 + D1_a2 + D2_a2
                           + D0_a3 + D1_a3 + D2_a3),
                      colour = 'TSV Prediction')) +
        geom_point(data = calib_data, 
                   aes(x = time, 
                       y = (cal_F0
                            + cal_D0_a1 + cal_D1_a1 + cal_D2_a1
                            + cal_D0_a2 + cal_D1_a2 + cal_D2_a2
                            + cal_D0_a3 + cal_D1_a3 + cal_D2_a3),
                       colour = 'ONS & NDA data')) +
        geom_point(data = ONS_prediction, 
                   aes(x = year, 
                       y = total,
                       colour = 'ONS Prediction')) +
        geom_vline(xintercept = 2020, 
                   linetype = "dotted", 
                   linewidth = 0.3) +
        geom_text(aes(x = 2020, 
                      label="\n Calibration ends", 
                      y=60e6), 
                  size = 4, fontface = 'italic', colour="black", angle=90) +
        labs(title = 'Total population comparison',
             x = 'Year', 
             y = 'Total Pop',
             colour = '') + 
        scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                        scale = 1e-6),
                           limits = c(50e6,100e6))
    } else{
      plt_totalPop <- ggplot(data = output) +
        geom_line(aes(x = time, 
                      y = (F0
                           + D0_a1 + D1_a1 + D2_a1
                           + D0_a2 + D1_a2 + D2_a2
                           + D0_a3 + D1_a3 + D2_a3),
                      colour = 'TSV Prediction')) +
        geom_point(data = calib_data, 
                   aes(x = time, 
                       y = (cal_F0
                            + cal_D0_a1 + cal_D1_a1 + cal_D2_a1
                            + cal_D0_a2 + cal_D1_a2 + cal_D2_a2
                            + cal_D0_a3 + cal_D1_a3 + cal_D2_a3),
                       colour = 'ONS & NDA data')) +
        geom_vline(xintercept = 2020, 
                   linetype = "dotted", 
                   linewidth = 0.3) +
        geom_text(aes(x = 2020, 
                      label="\n Calibration ends", 
                      y=60e6), 
                  size = 4, fontface = 'italic', colour="black", angle=90) +
        labs(title = 'Total population comparison',
             x = 'Year', 
             y = 'Total Pop',
             colour = '') + 
        scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                        scale = 1e-6),
                           limits = c(50e6,100e6))
    }
  }
  
  return(plt_totalPop)
}

plt_age_strat_comparison <- function(output, 
                                     calib_data, 
                                     ONS_prediction = NULL,
                                     calibrated = F){
  
  if (calibrated == F){
    if(!is.null(ONS_prediction)){
      plt_aX <- ggplot(data = output) +
        geom_line(aes(x = time, 
                      y = F0,
                      colour = 'age0')) +
        geom_line(aes(x = time, 
                      y = (D0_a1 + D1_a1 + D2_a1),
                      colour = 'age1')) +
        geom_line(aes(x = time, 
                      y = (D0_a2 + D1_a2 + D2_a2),
                      colour = 'age2')) +
        geom_line(aes(x = time, 
                      y = (D0_a3 + D1_a3 + D2_a3),
                      colour = 'age3')) +
        
        geom_point(data = calib_data,
                   aes(x = time,
                       y = cal_F0,
                       colour = 'age0')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a1 + cal_D1_a1 + cal_D2_a1),
                       colour = 'age1')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a2 + cal_D1_a2 + cal_D2_a2),
                       colour = 'age2')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a3 + cal_D1_a3 + cal_D2_a3),
                       colour = 'age3')) +
        
        geom_point(data = ONS_prediction,
                   aes(x = year,
                       y = age0,
                       colour = 'age0')) +
        geom_point(data = ONS_prediction,
                   aes(x = year,
                       y = age1,
                       colour = 'age1')) +
        geom_point(data = ONS_prediction,
                   aes(x = year,
                       y = age2,
                       colour = 'age2')) +
        geom_point(data = ONS_prediction,
                   aes(x = year,
                       y = age3,
                       colour = 'age3')) +
        
        labs(title = 'Age stratified population comparison',
             x = 'Year', 
             y = 'N',
             colour = '') + 
        scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                        scale = 1e-6),
                           limits = c(5e6,25e6))
    } else{
      plt_aX <- ggplot(data = output) +
        geom_line(aes(x = time, 
                      y = F0,
                      colour = 'age0')) +
        geom_line(aes(x = time, 
                      y = (D0_a1 + D1_a1 + D2_a1),
                      colour = 'age1')) +
        geom_line(aes(x = time, 
                      y = (D0_a2 + D1_a2 + D2_a2),
                      colour = 'age2')) +
        geom_line(aes(x = time, 
                      y = (D0_a3 + D1_a3 + D2_a3),
                      colour = 'age3')) +
        
        geom_point(data = calib_data,
                   aes(x = time,
                       y = cal_F0,
                       colour = 'age0')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a1 + cal_D1_a1 + cal_D2_a1),
                       colour = 'age1')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a2 + cal_D1_a2 + cal_D2_a2),
                       colour = 'age2')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a3 + cal_D1_a3 + cal_D2_a3),
                       colour = 'age3')) +
        
        labs(title = 'Age stratified population comparison',
             x = 'Year', 
             y = 'N',
             colour = '') + 
        scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                        scale = 1e-6),
                           limits = c(5e6,25e6))
    }
  }
  
  if (calibrated == T){
    if(!is.null(ONS_prediction)){
      plt_aX <- ggplot(data = output) +
        geom_line(aes(x = time, 
                      y = F0,
                      colour = 'age0')) +
        geom_line(aes(x = time, 
                      y = (D0_a1 + D1_a1 + D2_a1),
                      colour = 'age1')) +
        geom_line(aes(x = time, 
                      y = (D0_a2 + D1_a2 + D2_a2),
                      colour = 'age2')) +
        geom_line(aes(x = time, 
                      y = (D0_a3 + D1_a3 + D2_a3),
                      colour = 'age3')) +
        
        geom_point(data = calib_data,
                   aes(x = time,
                       y = cal_F0,
                       colour = 'age0')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a1 + cal_D1_a1 + cal_D2_a1),
                       colour = 'age1')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a2 + cal_D1_a2 + cal_D2_a2),
                       colour = 'age2')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a3 + cal_D1_a3 + cal_D2_a3),
                       colour = 'age3')) +
        
        geom_point(data = ONS_prediction,
                   aes(x = year,
                       y = age0,
                       colour = 'age0')) +
        geom_point(data = ONS_prediction,
                   aes(x = year,
                       y = age1,
                       colour = 'age1')) +
        geom_point(data = ONS_prediction,
                   aes(x = year,
                       y = age2,
                       colour = 'age2')) +
        geom_point(data = ONS_prediction,
                   aes(x = year,
                       y = age3,
                       colour = 'age3')) +
        
        geom_vline(xintercept = 2020, 
                   linetype = "dotted", 
                   linewidth = 0.3) +
        geom_text(aes(x = 2020, 
                      label="\n Calibration ends", 
                      y=10e6), 
                  size = 4, fontface = 'italic', colour="black", angle=90) +
        labs(title = 'Age stratified population comparison',
             x = 'Year', 
             y = 'N',
             colour = '') + 
        scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                        scale = 1e-6),
                           limits = c(5e6,25e6))
    } else{
      plt_aX <- ggplot(data = output) +
        geom_line(aes(x = time, 
                      y = F0,
                      colour = 'age0')) +
        geom_line(aes(x = time, 
                      y = (D0_a1 + D1_a1 + D2_a1),
                      colour = 'age1')) +
        geom_line(aes(x = time, 
                      y = (D0_a2 + D1_a2 + D2_a2),
                      colour = 'age2')) +
        geom_line(aes(x = time, 
                      y = (D0_a3 + D1_a3 + D2_a3),
                      colour = 'age3')) +
        
        geom_point(data = calib_data,
                   aes(x = time,
                       y = cal_F0,
                       colour = 'age0')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a1 + cal_D1_a1 + cal_D2_a1),
                       colour = 'age1')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a2 + cal_D1_a2 + cal_D2_a2),
                       colour = 'age2')) +
        geom_point(data = calib_data,
                   aes(x = time,
                       y = (cal_D0_a3 + cal_D1_a3 + cal_D2_a3),
                       colour = 'age3')) +
        
        geom_vline(xintercept = 2020, 
                   linetype = "dotted", 
                   linewidth = 0.3) +
        geom_text(aes(x = 2020, 
                      label="\n Calibration ends", 
                      y=10e6), 
                  size = 4, fontface = 'italic', colour="black", angle=90) +
        labs(title = 'Age stratified population comparison',
             x = 'Year', 
             y = 'N',
             colour = '') + 
        scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                        scale = 1e-6),
                           limits = c(5e6,25e6))
    }
  }
  
  return(plt_aX)
}

plt_full_strat_comparison <- function(output, 
                                      calib_data,
                                      calibrated = F){
  
  if (calibrated == T){
    plt_a0 <- ggplot(data = output) +
      geom_line(aes(x = time, 
                    y = F0,
                    colour = 'TSV')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_F0,
                     colour = 'ONS')) +
      
      geom_vline(xintercept = 2020, 
                 linetype = "dotted", 
                 linewidth = 0.3) +
      geom_text(aes(x = 2020, 
                    label="\n Calibration ends", 
                    y=10e6), 
                size = 4, fontface = 'italic', colour="black", angle=90) +
      labs(title = 'Age < 18',
           x = 'Year', 
           y = 'N',
           colour = '') + 
      scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                      scale = 1e-6),
                         limits = c(0,15e6))
    
    plt_D0 <- ggplot(data = output) +
      
      geom_line(aes(x = time, 
                    y = D0_a1,
                    colour = 'age1')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D0_a1,
                     colour = 'age1')) +
      
      geom_line(aes(x = time, 
                    y = D0_a2,
                    colour = 'age2')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D0_a2,
                     colour = 'age2')) +
      
      geom_line(aes(x = time, 
                    y = D0_a3,
                    colour = 'age3')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D0_a3,
                     colour = 'age3')) +
      
      geom_vline(xintercept = 2020, 
                 linetype = "dotted", 
                 linewidth = 0.3) +
      geom_text(aes(x = 2020, 
                    label="\n Calibration ends", 
                    y=5e6), 
                size = 4, fontface = 'italic', colour="black", angle=90) +
      labs(title = 'Non-diabetics',
           x = 'Year', 
           y = 'N',
           colour = '') + 
      scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                      scale = 1e-6),
                         limits = c(0,30e6))
    
    
    plt_D1 <- ggplot(data = output) +
      
      geom_line(aes(x = time, 
                    y = D1_a1,
                    colour = 'age1')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D1_a1,
                     colour = 'age1')) +
      
      
      geom_line(aes(x = time, 
                    y = D1_a2,
                    colour = 'age2')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D1_a2,
                     colour = 'age2')) +
      
      
      geom_line(aes(x = time, 
                    y = D1_a3,
                    colour = 'age3')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D1_a3,
                     colour = 'age3')) +
      
      geom_vline(xintercept = 2020, 
                 linetype = "dotted", 
                 linewidth = 0.3) +
      geom_text(aes(x = 2020, 
                    label="\n Calibration ends", 
                    y=0.8e6), 
                size = 4, fontface = 'italic', colour="black", angle=90) +
      labs(title = 'Uncontrolled T2D',
           x = 'Year', 
           y = 'N',
           colour = '') + 
      scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                      scale = 1e-6),
                         limits = c(0,1e6))
    
    
    plt_D2 <- ggplot(data = output) +
      
      geom_line(aes(x = time, 
                    y = D2_a1,
                    colour = 'age1')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D2_a1,
                     colour = 'age1')) +
      
      geom_line(aes(x = time, 
                    y = D2_a2,
                    colour = 'age2')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D2_a2,
                     colour = 'age2')) +
      
      geom_line(aes(x = time, 
                    y = D2_a3,
                    colour = 'age3')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D2_a3,
                     colour = 'age3')) +
      
      geom_vline(xintercept = 2020, 
                 linetype = "dotted", 
                 linewidth = 0.3) +
      geom_text(aes(x = 2020, 
                    label="\n Calibration ends", 
                    y=1.6e6), 
                size = 4, fontface = 'italic', colour="black", angle=90) +
      labs(title = 'Controlled T2D',
           x = 'Year', 
           y = 'N',
           colour = '') + 
      scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                      scale = 1e-6),
                         limits = c(0,2e6))
  }
  
  if (calibrated == F){
    plt_a0 <- ggplot(data = output) +
      geom_line(aes(x = time, 
                    y = F0,
                    colour = 'TSV')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_F0,
                     colour = 'ONS')) +
      
      labs(title = 'Age < 18',
           x = 'Year', 
           y = 'N',
           colour = '') + 
      scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                      scale = 1e-6),
                         limits = c(0,15e6))
    
    plt_D0 <- ggplot(data = output) +
      
      geom_line(aes(x = time, 
                    y = D0_a1,
                    colour = 'age1')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D0_a1,
                     colour = 'age1')) +
      
      geom_line(aes(x = time, 
                    y = D0_a2,
                    colour = 'age2')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D0_a2,
                     colour = 'age2')) +
      
      geom_line(aes(x = time, 
                    y = D0_a3,
                    colour = 'age3')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D0_a3,
                     colour = 'age3')) +
      
      labs(title = 'Non-diabetics',
           x = 'Year', 
           y = 'N',
           colour = '') + 
      scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                      scale = 1e-6),
                         limits = c(0,30e6))
    
    
    plt_D1 <- ggplot(data = output) +
      
      geom_line(aes(x = time, 
                    y = D1_a1,
                    colour = 'age1')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D1_a1,
                     colour = 'age1')) +
      
      
      geom_line(aes(x = time, 
                    y = D1_a2,
                    colour = 'age2')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D1_a2,
                     colour = 'age2')) +
      
      
      geom_line(aes(x = time, 
                    y = D1_a3,
                    colour = 'age3')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D1_a3,
                     colour = 'age3')) +
      
      labs(title = 'Uncontrolled T2D',
           x = 'Year', 
           y = 'N',
           colour = '') + 
      scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                      scale = 1e-6),
                         limits = c(0,1e6))
    
    
    plt_D2 <- ggplot(data = output) +
      
      geom_line(aes(x = time, 
                    y = D2_a1,
                    colour = 'age1')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D2_a1,
                     colour = 'age1')) +
      
      geom_line(aes(x = time, 
                    y = D2_a2,
                    colour = 'age2')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D2_a2,
                     colour = 'age2')) +
      
      geom_line(aes(x = time, 
                    y = D2_a3,
                    colour = 'age3')) +
      
      geom_point(data = calib_data,
                 aes(x = time,
                     y = cal_D2_a3,
                     colour = 'age3')) +
      
      labs(title = 'Controlled T2D',
           x = 'Year', 
           y = 'N',
           colour = '') + 
      scale_y_continuous(labels = scales::unit_format(unit = "M", 
                                                      scale = 1e-6),
                         limits = c(0,2e6))
  }
  
  plt_total <- ggarrange(plt_a0, plt_D0, plt_D1, plt_D2,
                         ncol = 2, nrow = 2)
  return(plt_total)
  
}

plt_age_strat_comparison_norm <- function(output,
                                          calib_data,
                                          ONS_prediction,
                                          calibrated = F){
  
  age0 <- output$F0 / c(calib_data$cal_F0,
                        ONS_prediction$age0)
  
  age1 <- (output$D0_a1 + output$D1_a1 + output$D2_a1) / 
    c((calib_data$cal_D0_a1
       + calib_data$cal_D1_a1
       + calib_data$cal_D2_a1),ONS_prediction$age1)
  
  age2 <- (output$D0_a2 + output$D1_a2 + output$D2_a2) / 
    c((calib_data$cal_D0_a2
       + calib_data$cal_D1_a2
       + calib_data$cal_D2_a2),ONS_prediction$age2)
  
  age3 <- (output$D0_a3 + output$D1_a3 + output$D2_a3) / 
    c((calib_data$cal_D0_a3
       + calib_data$cal_D1_a3
       + calib_data$cal_D2_a3),ONS_prediction$age3)
  
  df <- data.frame(year = seq(2001,2060,1),
                   age0 = age0,
                   age1 = age1,
                   age2 = age2,
                   age3 = age3)
  
  if (calibrated == F){
    plt_aX <- ggplot(data = df) +
      
      geom_point(aes(x = year,
                     y = age0,
                     colour = 'age0')) +
      geom_point(aes(x = year,
                     y = age1,
                     colour = 'age1')) +
      geom_point(aes(x = year,
                     y = age2,
                     colour = 'age2')) +
      geom_point(aes(x = year,
                     y = age3,
                     colour = 'age3')) +
      
      geom_hline(yintercept = 1.05, 
                 linetype = "dotted", 
                 linewidth = 0.3) +
      geom_hline(yintercept = 0.95, 
                 linetype = "dotted", 
                 linewidth = 0.3) +
      geom_text(aes(x = 2000, 
                    label="+5%", 
                    y = 1.06), 
                size = 4, fontface = 'italic', colour="black", angle=0) +
      geom_text(aes(x = 2000, 
                    label="-5%", 
                    y = 0.94), 
                size = 4, fontface = 'italic', colour="black", angle=0) +
      
      labs(title = 'Normalised age stratified population comparison',
           x = 'Year', 
           y = 'Predicted/Observed',
           colour = '') + 
      
      scale_y_continuous(limits = c(0.5, 1.5))
  }
  
  if (calibrated == T){
    plt_aX <- ggplot(data = df) +
      
      geom_point(aes(x = year,
                     y = age0,
                     colour = 'age0')) +
      geom_point(aes(x = year,
                     y = age1,
                     colour = 'age1')) +
      geom_point(aes(x = year,
                     y = age2,
                     colour = 'age2')) +
      geom_point(aes(x = year,
                     y = age3,
                     colour = 'age3')) +
      
      geom_hline(yintercept = 1.05, 
                 linetype = "dotted", 
                 linewidth = 0.3) +
      geom_hline(yintercept = 0.95, 
                 linetype = "dotted", 
                 linewidth = 0.3) +
      geom_text(aes(x = 2000, 
                    label="+5%", 
                    y = 1.06), 
                size = 4, fontface = 'italic', colour="black", angle=0) +
      geom_text(aes(x = 2000, 
                    label="-5%", 
                    y = 0.94), 
                size = 4, fontface = 'italic', colour="black", angle=0) +
      
      geom_vline(xintercept = 2020, 
                 linetype = "dotted", 
                 linewidth = 0.3) +
      geom_text(aes(x = 2020, 
                    label="\n Calibration ends", 
                    y = 0.75), 
                size = 4, fontface = 'italic', colour="black", angle=90) +
      
      labs(title = 'Normalised age stratified population comparison',
           x = 'Year', 
           y = 'Predicted/Observed',
           colour = '') + 
      
      scale_y_continuous(limits = c(0.5, 1.5))
  }
  
  return(plt_aX)
}

plt_full_strat_comparison_norm <- function(output,
                                           calib_data){
  
  D0_a1 <- (output$D0_a1[1:length(calib_data$cal_D0_a1)]) / 
    (calib_data$cal_D0_a1)
  
  D1_a1 <- (output$D1_a1[1:length(calib_data$cal_D1_a1)]) / 
    (calib_data$cal_D1_a1)
  
  D2_a1 <- (output$D2_a1[1:length(calib_data$cal_D2_a1)]) / 
    (calib_data$cal_D2_a1)
  
  
  D0_a2 <- (output$D0_a2[1:length(calib_data$cal_D0_a2)]) / 
    (calib_data$cal_D0_a2)
  
  D1_a2 <- (output$D1_a2[1:length(calib_data$cal_D1_a2)]) / 
    (calib_data$cal_D1_a2)
  
  D2_a2 <- (output$D2_a2[1:length(calib_data$cal_D2_a2)]) / 
    (calib_data$cal_D2_a2)
  
  
  D0_a3 <- (output$D0_a3[1:length(calib_data$cal_D0_a3)]) / 
    (calib_data$cal_D0_a3)
  
  D1_a3 <- (output$D1_a3[1:length(calib_data$cal_D1_a3)]) / 
    (calib_data$cal_D1_a3)
  
  D2_a3 <- (output$D2_a3[1:length(calib_data$cal_D2_a3)]) / 
    (calib_data$cal_D2_a3)
  
  df <- data.frame(year = seq(2001,2020,1),
                   D0_a1 = D0_a1,
                   D1_a1 = D1_a1,
                   D2_a1 = D2_a1,
                   D0_a2 = D0_a2,
                   D1_a2 = D1_a2,
                   D2_a2 = D2_a2,
                   D0_a3 = D0_a3,
                   D1_a3 = D1_a3,
                   D2_a3 = D2_a3)
  
  plt_D0 <- ggplot(data = df) +
    
    geom_point(aes(x = year,
                   y = D0_a1,
                   colour = 'age1')) +
    geom_point(aes(x = year,
                   y = D0_a2,
                   colour = 'age2')) +
    geom_point(aes(x = year,
                   y = D0_a3,
                   colour = 'age3')) +
    
    geom_hline(yintercept = 1.05, 
               linetype = "dotted", 
               linewidth = 0.3) +
    geom_hline(yintercept = 0.95, 
               linetype = "dotted", 
               linewidth = 0.3) +
    geom_text(aes(x = 2000, 
                  label="+5%", 
                  y = 1.06), 
              size = 4, fontface = 'italic', colour="black", angle=0) +
    geom_text(aes(x = 2000, 
                  label="-5%", 
                  y = 0.94), 
              size = 4, fontface = 'italic', colour="black", angle=0) +
    
    labs(title = 'Non-diabetics',
         x = 'Year', 
         y = 'Predicted/Observed',
         colour = '') + 
    
    scale_y_continuous(limits = c(0.5, 1.5))
  
  
  plt_D1 <- ggplot(data = df) +
    
    geom_point(aes(x = year,
                   y = D1_a1,
                   colour = 'age1')) +
    geom_point(aes(x = year,
                   y = D1_a2,
                   colour = 'age2')) +
    geom_point(aes(x = year,
                   y = D1_a3,
                   colour = 'age3')) +
    
    geom_hline(yintercept = 1.05, 
               linetype = "dotted", 
               linewidth = 0.3) +
    geom_hline(yintercept = 0.95, 
               linetype = "dotted", 
               linewidth = 0.3) +
    geom_text(aes(x = 2000, 
                  label="+5%", 
                  y = 1.06), 
              size = 4, fontface = 'italic', colour="black", angle=0) +
    geom_text(aes(x = 2000, 
                  label="-5%", 
                  y = 0.94), 
              size = 4, fontface = 'italic', colour="black", angle=0) +
    
    labs(title = 'Uncontrolled T2D',
         x = 'Year', 
         y = 'Predicted/Observed',
         colour = '') + 
    
    scale_y_continuous(limits = c(0.5, 1.5))
  
  
  plt_D2 <- ggplot(data = df) +
    
    geom_point(aes(x = year,
                   y = D2_a1,
                   colour = 'age1')) +
    geom_point(aes(x = year,
                   y = D2_a2,
                   colour = 'age2')) +
    geom_point(aes(x = year,
                   y = D2_a3,
                   colour = 'age3')) +
    
    geom_hline(yintercept = 1.05, 
               linetype = "dotted", 
               linewidth = 0.3) +
    geom_hline(yintercept = 0.95, 
               linetype = "dotted", 
               linewidth = 0.3) +
    geom_text(aes(x = 2000, 
                  label="+5%", 
                  y = 1.06), 
              size = 4, fontface = 'italic', colour="black", angle=0) +
    geom_text(aes(x = 2000, 
                  label="-5%", 
                  y = 0.94), 
              size = 4, fontface = 'italic', colour="black", angle=0) +
    
    labs(title = 'Controlled T2D',
         x = 'Year', 
         y = 'Predicted/Observed',
         colour = '') + 
    
    scale_y_continuous(limits = c(0.5, 1.5))
  
  plt_total <- ggarrange(plt_D0, plt_D1, plt_D2,
                         ncol = 2, nrow = 2, common.legend = TRUE)
  return(plt_total)
}
