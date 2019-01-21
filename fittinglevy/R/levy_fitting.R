if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

# Fittig function for the levy
levy_fitting <- function(dat_t, bin_num, include_bootstrap=FALSE) { # three arguments: 1) data and 2) the bin number 3) indicator whether to include bootstrap
  p_data <- dat_t
  est_levy_qt <- Levy_fun_QT(p_data) # Levy estimation with QT
  #est_levy_gmm <- Levy_fun_GMM(p_data)
  
  if (include_bootstrap) {
    # bootstrapping
    est_levy_qt_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_QT, R=1000, sim="ordinary", parallel="multicore", ncpus=3)
    ## Note: if this does not work on windows, try with snow instead of multicore
    #est_levy_qt_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_QT, R=100, sim="ordinary", parallel="snow", ncpus=3)
    ## ... if this does also not work, try without parallelization. This will take multiple times longer.
    #est_levy_qt_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_QT, R=100, sim="ordinary")

    #est_levy_qt_np_std_error <- boot(p_data, Levy_fun_QT, R=1000, sim="parametric", ran.gen=get_stabledist_variates, mle=est_levy_qt, parallel="multicore", ncpus=3)
  } else {
    est_levy_qt_std_error <- NULL
  }

  p_data_h <- hist(p_data, plot = F, breaks = seq(min(p_data), max(p_data), l = bin_num)) # binning the data

  
  obs_mid <- p_data_h$mids # location of the bin
  obs_p <- p_data_h$counts / sum(p_data_h$counts) # normalized counts of the bin: the normalized empirical density

  pred_p_levy_b <- dstable(obs_mid, est_levy_qt[1], est_levy_qt[2], est_levy_qt[3], est_levy_qt[4]) # the predicted density from the Levy model given the estimated parameters
  
  #pred_p_levy_b <- dstable(obs_mid, est_levy_gmm$Estim$par[1], est_levy_gmm$Estim$par[2], est_levy_gmm$Estim$par[3], est_levy_gmm$Estim$par[4]) # the predicted density from the Levy model given the estimated parameters
  
  pred_p_levy <- pred_p_levy_b / sum(pred_p_levy_b) # normalized density

  levy_soofi <- 1 - soofi_gen(obs_p, pred_p_levy) # soofi index


  ok_list <- list(raw_data = p_data, data_mid = obs_mid, data_p = obs_p, levy_q = pred_p_levy, levy_para = est_levy_qt, levy_soofi = round(levy_soofi, 4) * 100, est_levy_qt_std_error = est_levy_qt_std_error)
  #ok_list <- list(raw_data = p_data, data_mid = obs_mid, data_p = obs_p, levy_q = pred_p_levy, levy_para = est_levy_gmm, levy_soofi = round(levy_soofi, 4) * 100)

  return(ok_list)
}
