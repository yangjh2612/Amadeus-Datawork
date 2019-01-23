# Wrapper function for Levy-alpha stable estimation using GMM with GMMParametersEstim from StableEstim package

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

Levy_fun_GMM <- function(x) {
  # Wrapper function
  #     Note that the function uses generic parameters for the fitting procedure. It can be extended to allow
  #     different parametrizations for the GMMParametersEstim. 
  #     For details, see https://cran.r-project.org/web/packages/StableEstim/StableEstim.pdf
  # Arguments:
  #     x: numeric, dataset to be fitted
  # Returns:
  #     list of the four parameters.
  #     Note that the wrapper function discards the other return values of GMMParametersEstim.
  #     For details, see https://cran.r-project.org/web/packages/StableEstim/StableEstim.pdf

  # set generic parameters
  pm <- 0
  regularization = "cut-off"
  WeightingMatrix = "OptAsym"
  alphaReg = 0.005
  algo = "2SGMM"
  
  t_scheme = "free"
  t_seq = seq(0.1, 2, length.out = 12)
  
  # start fit
  GMM_1 <- GMMParametersEstim(x = x,
                              algo = algo, alphaReg = alphaReg,
                              regularization = regularization,
                              WeightingMatrix = WeightingMatrix,
                              t_scheme = t_scheme,
                              pm = pm, PrintTime = TRUE, t_free = t_seq)
  
  # return parameters only
  return(GMM_1$Estim$par)
}
