# Wrapper function for generating artificial Levy alpha stable samples for parametric bootstrap

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

get_stabledist_variates <- function(dat, est_levy_qt=NULL) {
  # Wrapper function
  # Arguments:
  #     dat: numeric, original series for the surrogate distributions to be compared to
  #     est_levy_qt: list of four numeric or NULL, parameter estimates. Optional. Will be fitted with McCulloch if not supplied.
  # Returns;
  #     list of random variates (the artificial distribution)
  if (is.null(est_levy_qt)) {
      est_levy_qt <- Levy_fun_QT(dat)
  }
  variates <- stabledist::rstable(n=length(dat), alpha=est_levy_qt[[1]], 
                                  beta=est_levy_qt[[2]], gamma=est_levy_qt[[3]], 
                                  delta=est_levy_qt[[4]])
  return(variates)
}
