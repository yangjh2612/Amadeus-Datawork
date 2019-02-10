# Wrapper function for Levy-alpha stable estimation using Maximum Likelihood with MLParametersEstim from StableEstim package. This will take a long time.

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

Levy_fun_ML <- function(x) {
  # Wrapper function
  #     Note that the function uses generic parameters for the fitting procedure. It can be extended to allow
  #     different parametrizations for the MLParametersEstim. 
  #     For details, see https://cran.r-project.org/web/packages/StableEstim/StableEstim.pdf
  # Arguments:
  #     x: numeric, dataset to be fitted
  # Returns:
  #     list of the four parameters.
  #     Note that the wrapper function discards the other return values of MLParametersEstim.
  #     For details, see https://cran.r-project.org/web/packages/StableEstim/StableEstim.pdf

  # set generic parameters
  pm <- 0
  
  # start fit
  ML_1 <- MLParametersEstim(x = x, pm = pm, PrintTime = TRUE)
  
  # return parameters only
  return(ML_1$Estim$par)
}
