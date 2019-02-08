# Function for handling Levy alpha-stable fits and bootstraps

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

# Fittig function for the levy
levy_fitting <- function(dat_t, bin_num, include_bootstrap=FALSE, fitting_method="QT") { # three arguments: 1) data and 2) the bin number 3) indicator whether to include bootstrap
  p_data <- dat_t
  if (fitting_method=="QT") {
      est_levy <- Levy_fun_QT(p_data) # Levy estimation with QT
  } else if (fitting_method=="GMM") {
      est_levy <- Levy_fun_GMM(p_data)
  } else if (fitting_method=="ML") {
      est_levy <- Levy_fun_ML(p_data)
  } else {
      print("Non-implemented fitting method requested. You must implement it first. Exiting.")
      quit(status=1)
  }
  
  if (include_bootstrap) {
    # bootstrapping
    if (fitting_method=="QT") {
      est_levy_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_QT, R=1000, sim="ordinary", parallel="multicore", ncpus=3)
      ## Note: if this does not work on windows, try with snow instead of multicore
      #est_levy_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_QT, R=100, sim="ordinary", parallel="snow", ncpus=3)
      ## ... if this does also not work, try without parallelization. This will take multiple times longer.
      #est_levy_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_QT, R=100, sim="ordinary")
    } else if (fitting_method=="GMM") {
      est_levy_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_GMM, R=1000, sim="ordinary", parallel="multicore", ncpus=3)     # TODO: wrapper_nonparametric_Levy_fun_GMM is not implemented
      ## Note: if this does not work on windows, try with snow instead of multicore
      #est_levy_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_GMM, R=100, sim="ordinary", parallel="snow", ncpus=3)
      ## ... if this does also not work, try without parallelization. This will take multiple times longer.
      #est_levy_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_GMM, R=100, sim="ordinary")
    } else if (fitting_method=="ML") {
      est_levy_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_ML, R=1000, sim="ordinary", parallel="multicore", ncpus=3)      # TODO: wrapper_nonparametric_Levy_fun_ML is not implemented
      ## Note: if this does not work on windows, try with snow instead of multicore
      #est_levy_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_GMM, R=100, sim="ordinary", parallel="snow", ncpus=3)
      ## ... if this does also not work, try without parallelization. This will take multiple times longer.
      #est_levy_std_error <- boot(p_data, wrapper_nonparametric_Levy_fun_GMM, R=100, sim="ordinary")
    }

    #est_levy_np_std_error <- boot(p_data, Levy_fun_QT, R=1000, sim="parametric", ran.gen=get_stabledist_variates, mle=est_levy, parallel="multicore", ncpus=3)
  } else {
    est_levy_std_error <- NULL
  }

  p_data_h <- hist(p_data, plot = F, breaks = seq(min(p_data), max(p_data), l = bin_num)) # binning the data

  
  obs_mid <- p_data_h$mids # location of the bin
  obs_p <- p_data_h$counts / sum(p_data_h$counts) # normalized counts of the bin: the normalized empirical density

  pred_p_levy_b <- dstable(obs_mid, est_levy[1], est_levy[2], est_levy[3], est_levy[4]) # the predicted density from the Levy model given the estimated parameters
  
  pred_p_levy <- pred_p_levy_b / sum(pred_p_levy_b) # normalized density

  levy_soofi <- 1 - soofi_gen(obs_p, pred_p_levy) # soofi index

  ok_list <- list(raw_data = p_data, data_mid = obs_mid, data_p = obs_p, levy_q = pred_p_levy, levy_para = est_levy, levy_soofi = round(levy_soofi, 4) * 100, est_levy_std_error = est_levy_std_error)
  
  return(ok_list)
}
