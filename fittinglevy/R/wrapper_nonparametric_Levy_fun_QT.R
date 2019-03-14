# Wrapper function for nonparametric bootstrap of Levy alpha-stable using McCulloch estimation from StableEstim

if (!'pacman' %in% installed.packages()[,'Package']) install.packages('pacman', repos='http://cran.r-project.org')
pacman::p_load(boot,dplyr,StableEstim)

wrapper_nonparametric_Levy_fun_QT <- function(dat, indices) {
  # Wrapper function
  #  Arguments
  #     dat: numeric, original series for the surrogate distributions to be compared to
  #     indices: list of indices of the subsample to be fitted
  # Returns;
  #     list of the four parameters.
  dat2 <- dat[indices]
  res <- Levy_fun_QT(dat2)
  return(res)
}
