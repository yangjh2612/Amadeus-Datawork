if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

Levy_fun_GMM <- function(x) {
  pm <- 0
  regularization = "cut-off"
  WeightingMatrix = "OptAsym"
  alphaReg = 0.005
  algo = "2SGMM"
  
  t_scheme = "free"
  t_seq = seq(0.1, 2, length.out = 12)
  GMM_1 <- GMMParametersEstim(x = x,
                              algo = algo, alphaReg = alphaReg,
                              regularization = regularization,
                              WeightingMatrix = WeightingMatrix,
                              t_scheme = t_scheme,
                              pm = pm, PrintTime = TRUE, t_free = t_seq)
  
}
